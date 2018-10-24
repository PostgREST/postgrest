{-# LANGUAGE CPP #-}

module Main where


import           PostgREST.App            (postgrest)
import           PostgREST.Config         (AppConfig (..),
                                           prettyVersion, readOptions)
import           PostgREST.DbStructure    (getDbStructure, getPgVersion)
import           PostgREST.Error          (encodeError)
import           PostgREST.OpenAPI        (isMalformedProxyUri)
import           PostgREST.Types          (DbStructure, Schema, PgVersion(..), minimumPgVersion)
import           Protolude                hiding (hPutStrLn, replace)


import           Control.AutoUpdate       (defaultUpdateSettings,
                                           mkAutoUpdate, updateAction)
import           Control.Retry            (RetryStatus, capDelay,
                                           exponentialBackoff,
                                           retrying, rsPreviousDelay)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as B64
import           Data.IORef               (IORef, atomicWriteIORef,
                                           newIORef, readIORef)
import           Data.String              (IsString (..))
import           Data.Text                (pack, replace, stripPrefix, strip)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Data.Text.IO             (hPutStrLn)
import           Data.Time.Clock          (getCurrentTime)
import qualified Hasql.Pool               as P
import qualified Hasql.Session            as H
import           Network.Wai.Handler.Warp (defaultSettings,
                                           runSettings, setHost,
                                           setPort, setServerName)
import           System.IO                (BufferMode (..),
                                           hSetBuffering)

#ifndef mingw32_HOST_OS
import           System.Posix.Signals
#endif

{-|
  The purpose of this worker is to fill the refDbStructure created in 'main'
  with the 'DbStructure' returned from calling 'getDbStructure'. This method
  is meant to be called by multiple times by the same thread, but does nothing if
  the previous invocation has not terminated. In all cases this method does not
  halt the calling thread, the work is preformed in a separate thread.

  Note: 'atomicWriteIORef' is essentially a lazy semaphore that prevents two
  threads from running 'connectionWorker' at the same time.

  Background thread that does the following :
  1. Tries to connect to pg server and will keep trying until success.
  2. Checks if the pg version is supported and if it's not it kills the main
     program.
  3. Obtains the dbStructure.
  4. If 2 or 3 fail to give their result it means the connection is down so it
     goes back to 1, otherwise it finishes his work successfully.
-}
connectionWorker
  :: ThreadId -- ^ This thread is killed if pg version is unsupported
  -> P.Pool   -- ^ The PostgreSQL connection pool
  -> Schema   -- ^ Schema PostgREST is serving up
  -> IORef (Maybe DbStructure) -- ^ mutable reference to 'DbStructure'
  -> IORef Bool                -- ^ Used as a binary Semaphore
  -> IO ()
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
          actualPgVersion <- getPgVersion
          unless (actualPgVersion >= minimumPgVersion) $ liftIO $ do
            hPutStrLn stderr
              ("Cannot run in this PostgreSQL version, PostgREST needs at least "
              <> pgvName minimumPgVersion)
            killThread mainTid
          dbStructure <- getDbStructure schema actualPgVersion
          liftIO $ atomicWriteIORef refDbStructure $ Just dbStructure
        case result of
          Left e -> do
            putStrLn ("Failed to query the database. Retrying." :: Text)
            hPutStrLn stderr (toS $ encodeError e)
            work
          Right _ -> do
            atomicWriteIORef refIsWorkerOn False
            putStrLn ("Connection successful" :: Text)

{-|
  Used by 'connectionWorker' to check if the provided db-uri lets
  the application access the PostgreSQL database. This method is used
  the first time the connection is tested, but only to test before
  calling 'getDbStructure' inside the 'connectionWorker' method.

  The connection tries are capped, but if the connection times out no error is
  thrown, just 'False' is returned.
-}
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

{-|
  This is where everything starts.
-}
main :: IO ()
main = do
  --
  -- LineBuffering: the entire output buffer is flushed whenever a newline is
  -- output, the buffer overflows, a hFlush is issued or the handle is closed
  --
  -- NoBuffering: output is written immediately and never stored in the buffer
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  hSetBuffering stderr NoBuffering
  --
  -- readOptions builds the 'AppConfig' from the config file specified on the
  -- command line
  conf <- loadSecretFile =<< readOptions
  let host = configHost conf
      port = configPort conf
      proxy = configProxyUri conf
      pgSettings = toS (configDatabase conf) -- is the db-uri
      roleClaimKey = configRoleClaimKey conf
      appSettings =
        setHost ((fromString . toS) host) -- Warp settings
        . setPort port
        . setServerName (toS $ "postgrest/" <> prettyVersion) $
        defaultSettings

  -- Checks that the provided proxy uri is formated correctly
  when (isMalformedProxyUri $ toS <$> proxy) $
    panic
      "Malformed proxy uri, a correct example: https://example.com:8443/basePath"

  -- Checks that the provided jspath is valid
  when (isLeft roleClaimKey) $
    panic $ show roleClaimKey

  putStrLn $ ("Listening on port " :: Text) <> show (configPort conf)
  --
  -- create connection pool with the provided settings, returns either
  -- a 'Connection' or a 'ConnectionError'. Does not throw.
  pool <- P.acquire (configPool conf, 10, pgSettings)
  --
  -- To be filled in by connectionWorker
  refDbStructure <- newIORef Nothing
  --
  -- Helper ref to make sure just one connectionWorker can run at a time
  refIsWorkerOn <- newIORef False
  --
  -- This is passed to the connectionWorker method so it can kill the main
  -- thread if the PostgreSQL's version is not supported.
  mainTid <- myThreadId
  --
  -- Sets the refDbStructure
  connectionWorker
    mainTid
    pool
    (configSchema conf)
    refDbStructure
    refIsWorkerOn
  --
  -- Only for systems with signals:
  --
  -- releases the connection pool whenever the program is terminated,
  -- see issue #268
  --
  -- Plus the SIGHUP signal updates the internal 'DbStructure' by running
  -- 'connectionWorker' exactly as before.
#ifndef mingw32_HOST_OS
  forM_ [sigINT, sigTERM] $ \sig ->
    void $ installHandler sig (Catch $ do
        P.release pool
        throwTo mainTid UserInterrupt
      ) Nothing

  forM_ [sigHUP, sigUSR1] $ \sig ->
    void $ installHandler sig (
      Catch $ connectionWorker
                mainTid
                pool
                (configSchema conf)
                refDbStructure
                refIsWorkerOn
      ) Nothing
#endif


  -- ask for the OS time at most once per second
  getTime <- mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}

  -- run the postgrest application
  runSettings appSettings $
    postgrest
      conf
      refDbStructure
      pool
      getTime
      (connectionWorker
         mainTid
         pool
         (configSchema conf)
         refDbStructure
         refIsWorkerOn)

{-|
  The purpose of this function is to load the JWT secret from a file if
  configJwtSecret is actually a filepath and replaces some characters if the JWT
  is base64 encoded.

  The reason some characters need to be replaced is because JWT is actually
  base64url encoded which must be turned into just base64 before decoding.

  To check if the JWT secret is provided is in fact a file path, it must be
  decoded as 'Text' to be processed.

  decodeUtf8: Decode a ByteString containing UTF-8 encoded text that is known to
  be valid.
-}
loadSecretFile :: AppConfig -> IO AppConfig
loadSecretFile conf = extractAndTransform mSecret
  where
    mSecret = decodeUtf8 <$> configJwtSecret conf
    isB64 = configJwtSecretIsBase64 conf
    --
    -- The Text (variable name secret) here is mSecret from above which is the JWT
    -- decoded as Utf8
    --
    -- stripPrefix: Return the suffix of the second string if its prefix matches
    -- the entire first string.
    --
    -- The configJwtSecret is a filepath instead of the JWT secret itself if the
    -- secret has @ as its prefix.
    extractAndTransform :: Maybe Text -> IO AppConfig
    extractAndTransform Nothing = return conf
    extractAndTransform (Just secret) =
      fmap setSecret $
      transformString isB64 =<<
      case stripPrefix "@" secret of
        Nothing       -> return . encodeUtf8 $ secret
        Just filename -> chomp <$> BS.readFile (toS filename)
      where
        chomp bs = fromMaybe bs (BS.stripSuffix "\n" bs)
    --
    -- Turns the Base64url encoded JWT into Base64
    transformString :: Bool -> ByteString -> IO ByteString
    transformString False t = return t
    transformString True t =
      case B64.decode $ encodeUtf8 $ strip $ replaceUrlChars $ decodeUtf8 t of
        Left errMsg -> panic $ pack errMsg
        Right bs    -> return bs
    setSecret bs = conf {configJwtSecret = Just bs}
    --
    -- replace: Replace every occurrence of one substring with another
    replaceUrlChars =
      replace "_" "/" . replace "-" "+" . replace "." "="
