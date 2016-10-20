{-# LANGUAGE CPP #-}

module Main where

import           Protolude
import           PostgREST.App
import           PostgREST.Config                     (AppConfig (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)
import           PostgREST.Error                      (prettyUsageError)
import           PostgREST.OpenAPI                    (isMalformedProxyUri)
import           PostgREST.DbStructure

import           Control.AutoUpdate
import           Data.String                          (IsString (..))
import           Data.Text                            (stripPrefix)
import           Data.Text.IO                         (hPutStrLn)
import           Data.Function                        (id)
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
  return $ toInteger ver >= minimumPgVersion
 where
  pgVersion =
    H.statement "SHOW server_version_num"
      HE.unit (HD.singleRow $ HD.value HD.int4) True

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
                  $ defaultSettings

  when (isMalformedProxyUri $ toS <$> proxy) $ panic
    "Malformed proxy uri, a correct example: https://example.com:8443/basePath"

  putStrLn $ ("Listening on port " :: Text) <> show (configPort conf)

  pool <- P.acquire (configPool conf, 10, pgSettings)

  result <- P.use pool $ do
    supported <- isServerVersionSupported
    unless supported $ panic (
      "Cannot run in this PostgreSQL version, PostgREST needs at least "
      <> show minimumPgVersion)
    getDbStructure (toS $ configSchema conf)

  forM_ (lefts [result]) $ \e -> do
    hPutStrLn stderr (prettyUsageError e)
    exitFailure

  refDbStructure <- newIORef $ either (panic . show) id result

#ifndef mingw32_HOST_OS
  tid <- myThreadId
  forM_ [sigINT, sigTERM] $ \sig ->
    void $ installHandler sig (Catch $ do
        P.release pool
        throwTo tid UserInterrupt
      ) Nothing

  void $ installHandler sigHUP (
      Catch . void . P.use pool $ do
        s <- getDbStructure (toS $ configSchema conf)
        liftIO $ atomicWriteIORef refDbStructure s
   ) Nothing
#endif

  -- ask for the OS time at most once per second
  getTime <- mkAutoUpdate
    defaultUpdateSettings { updateAction = getPOSIXTime }

  runSettings appSettings $ postgrest conf refDbStructure pool getTime

loadSecretFile :: AppConfig -> IO AppConfig
loadSecretFile conf = do
  let s = configJwtSecret conf
  real <- case join (stripPrefix "@" <$> s) of
            Nothing -> return s -- the string is the secret, not a filename
            Just filename -> sequence . Just $ readFile (toS filename)
  return conf { configJwtSecret = real }
