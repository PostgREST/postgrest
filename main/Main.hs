{-# LANGUAGE CPP #-}

module Main where

import           Protolude
import           PostgREST.App
import           PostgREST.Config                     (AppConfig (..),
                                                       PgVersion (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)
import           PostgREST.Error                      (encodeError)
import           PostgREST.OpenAPI                    (isMalformedProxyUri)
import           PostgREST.DbStructure
import           PostgREST.Types                      (DbStructure, Schema)
import           PostgRESTWS

import           Control.AutoUpdate
import           Control.Retry
import           Data.ByteString.Base64               (decode)
import           Data.Function                        (id)
import           Data.String                          (IsString (..))
import           Data.Text                            (stripPrefix, pack, replace)
import           Data.Text.Encoding                   (encodeUtf8, decodeUtf8)
import           Data.Text.IO                         (hPutStrLn, readFile)
import           Data.Time.Clock.POSIX                (getPOSIXTime)
import qualified Hasql.Query                          as H
import qualified Hasql.Session                        as H
import qualified Hasql.Decoders                       as HD
import qualified Hasql.Encoders                       as HE
import qualified Hasql.Pool                           as P
import           Network.Wai.Handler.Warp
import           System.IO                            (BufferMode (..),
                                                       hSetBuffering)
import           Data.IORef
#ifndef mingw32_HOST_OS
import           System.Posix.Signals
#endif

isServerVersionSupported :: H.Session Bool
isServerVersionSupported = do
  ver <- H.query () pgVersion
  return $ ver >= pgvNum minimumPgVersion
 where
  pgVersion =
    H.statement "SELECT current_setting('server_version_num')::integer"
      HE.unit (HD.singleRow $ HD.value HD.int4) False

{-|
  Background thread that does the following :
  1. Tries to connect to pg server and will keep trying until success.
  2. Checks if the pg version is supported and if it's not it kills the main program.
  3. Obtains the dbStructure.
  4. If 2 or 3 fail to give their result it means the connection is down so it goes back to 1,
     otherwise it finishes his work successfully.
-}
connectionWorker :: ThreadId -> P.Pool -> Schema -> IORef (Maybe DbStructure) -> IORef Bool -> IO ()
connectionWorker mainTid pool schema refDbStructure refIsWorkerOn = do
  isWorkerOn <- readIORef refIsWorkerOn
  unless isWorkerOn $ do
    atomicWriteIORef refIsWorkerOn True
    void $ forkIO work
  where
    work = do
      atomicWriteIORef refDbStructure Nothing
      putStrLn ("Attempting to connect to the database..." :: Text)
      connected <- connectingSucceeded pool
      when connected $ do
        result <- P.use pool $ do
          supported <- isServerVersionSupported
          unless supported $ liftIO $ do
            hPutStrLn stderr
              ("Cannot run in this PostgreSQL version, PostgREST needs at least "
              <> pgvName minimumPgVersion)
            killThread mainTid
          dbStructure <- getDbStructure schema
          liftIO $ atomicWriteIORef refDbStructure $ Just dbStructure
        case result of
          Left e -> do
            putStrLn ("Failed to query the database. Retrying." :: Text)
            hPutStrLn stderr (toS $ encodeError e)
            work
          Right _ -> do
            atomicWriteIORef refIsWorkerOn False
            putStrLn ("Connection successful" :: Text)

-- | Connect to pg server if it fails retry with capped exponential backoff until success
connectingSucceeded :: P.Pool -> IO Bool
connectingSucceeded pool =
  retrying (capDelay 32000000 $ exponentialBackoff 1000000)
           shouldRetry
           (const $ P.release pool >> isConnectionSuccessful)
  where
    isConnectionSuccessful :: IO Bool
    isConnectionSuccessful = do
      testConn <- P.use pool $ H.sql "SELECT 1"
      case testConn of
        Left e -> hPutStrLn stderr (toS $ encodeError e) >> pure False
        _ -> pure True
    shouldRetry :: RetryStatus -> Bool -> IO Bool
    shouldRetry rs isConnSucc = do
      delay <- pure $ fromMaybe 0 (rsPreviousDelay rs) `div` 1000000
      itShould <- pure $ not isConnSucc
      when itShould $
        putStrLn $ "Attempting to reconnect to the database in " <> (show delay::Text) <> " seconds..."
      return itShould

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  conf <- loadSecretFile =<< readOptions
  let host = configHost conf
      port = configPort conf
      proxy = configProxyUri conf
      pgSettings = toS (configDatabase conf)
      appSettings = setHost ((fromString . toS) host)
                  . setPort port
                  . setServerName (toS $ "postgrest/" <> prettyVersion)
                  . setTimeout 3600
                  $ defaultSettings

  when (isMalformedProxyUri $ toS <$> proxy) $ panic
    "Malformed proxy uri, a correct example: https://example.com:8443/basePath"

  putStrLn $ ("Listening on port " :: Text) <> show (configPort conf)

  pool <- P.acquire (configPool conf, 10, pgSettings)

  refDbStructure <- newIORef Nothing

  -- Helper ref to make sure just one connectionWorker can run at a time
  refIsWorkerOn <- newIORef False

  mainTid <- myThreadId

  connectionWorker mainTid pool (configSchema conf) refDbStructure refIsWorkerOn

#ifndef mingw32_HOST_OS
  forM_ [sigINT, sigTERM] $ \sig ->
    void $ installHandler sig (Catch $ do
        P.release pool
        throwTo mainTid UserInterrupt
      ) Nothing

  void $ installHandler sigHUP (
      Catch $ connectionWorker mainTid pool (configSchema conf) refDbStructure refIsWorkerOn
   ) Nothing
#endif

  -- ask for the OS time at most once per second
  getTime <- mkAutoUpdate
    defaultUpdateSettings { updateAction = getPOSIXTime }


  wsMiddleware <-
        if configWebsockets conf
          then do
            multiOrError <- newHasqlBroadcasterOrError pgSettings
            let multi = either (panic . show) id multiOrError
                secret = fromMaybe (panic "You need to set a JWT secret to enable websockets") $ configJwtSecret conf
            return $ postgrestWsMiddleware secret getTime pool multi
          else return id

  runSettings appSettings $
    wsMiddleware $
    postgrest conf refDbStructure pool getTime
      (connectionWorker mainTid pool (configSchema conf) refDbStructure refIsWorkerOn)

loadSecretFile :: AppConfig -> IO AppConfig
loadSecretFile conf = extractAndTransform mSecret
  where
    mSecret   = decodeUtf8 <$> configJwtSecret conf
    isB64     = configJwtSecretIsBase64 conf

    extractAndTransform :: Maybe Text -> IO AppConfig
    extractAndTransform Nothing  = return conf
    extractAndTransform (Just s) =
      fmap setSecret $ transformString isB64 =<<
        case stripPrefix "@" s of
            Nothing       -> return s
            Just filename -> readFile (toS filename)

    transformString :: Bool -> Text -> IO ByteString
    transformString False t = return . encodeUtf8 $ t
    transformString True  t =
      case decode (encodeUtf8 $ replaceUrlChars t) of
        Left errMsg -> panic $ pack errMsg
        Right bs    -> return bs

    setSecret bs = conf { configJwtSecret = Just bs }

    replaceUrlChars = replace "_" "/" . replace "-" "+" . replace "." "="
