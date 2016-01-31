{-# LANGUAGE CPP #-}

module Main where


import           PostgREST.App
import           PostgREST.Config                     (AppConfig (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)
import           PostgREST.DbStructure
import           PostgREST.Error                      (errResponse, pgErrResponse)
import           PostgREST.Middleware
import           PostgREST.QueryBuilder               (inTransaction, Isolation(..))

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Concurrent.MVar              (newEmptyMVar, putMVar)
import           Control.Monad                        (forever, unless, void)
import           Data.Monoid                          ((<>))
import           Data.Pool
import           Data.String.Conversions              (cs)
import           Data.Time.Clock.POSIX                (getPOSIXTime)
import qualified Hasql.Query                          as H
import qualified Hasql.Connection                     as H
import qualified Hasql.Session                        as H
import qualified Hasql.Decoders                       as HD
import qualified Hasql.Encoders                       as HE
import qualified Network.HTTP.Types.Status            as HT
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.IO                            (BufferMode (..),
                                                       hSetBuffering, stderr,
                                                       stdin, stdout)
import           Web.JWT                              (secret)

#ifndef mingw32_HOST_OS
import           System.Posix.Signals
import           Control.Concurrent                   (myThreadId)
import           Control.Exception.Base               (throwTo, AsyncException(..))
#endif

isServerVersionSupported :: H.Session Bool
isServerVersionSupported = do
  ver <- H.query () pgVersion
  return $ read (cs ver) >= minimumPgVersion
 where
  pgVersion =
    H.statement "SHOW server_version_num"
      HE.unit (HD.singleRow $ HD.value HD.text) True

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  conf <- readOptions
  let port = configPort conf

  unless (secret "secret" /= configJwtSecret conf) $
    putStrLn "WARNING, running in insecure mode, JWT secret is the default value"
  Prelude.putStrLn $ "Listening on port " ++
    (show $ configPort conf :: String)

  let pgSettings = cs (configDatabase conf)
      appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings
      middle = logStdout . defaultMiddle

  pool <- createPool (H.acquire pgSettings)
            (either (const $ return ()) H.release) 1 1 (configPool conf)

  withResource pool $ \case
    Left err -> error $ show err
    Right c -> do
      supported <- H.run isServerVersionSupported c
      case supported of
        Left e -> error $ show e
        Right good -> unless good $
          error (
            "Cannot run in this PostgreSQL version, PostgREST needs at least "
            <> show minimumPgVersion)

  dbStructure <- newEmptyMVar
  -- Fork thread to poll for schema changes
  -- TODO: convert to NOTIFY/LISTEN and ddl event triggers
  void . forkIO $
    withResource pool $ \case
      Left err -> error $ show err
      Right c -> forever $ do
        H.run (getDbStructure (cs $ configSchema conf)) c >>= \case
          Left err -> error $ show err
          Right s -> putMVar dbStructure s
        threadDelay (5 * 60 * 1000000)

#ifndef mingw32_HOST_OS
  tid <- myThreadId
  void $ installHandler keyboardSignal (Catch $ do
      destroyAllResources pool
      throwTo tid UserInterrupt
    ) Nothing
#endif

  runSettings appSettings $ middle $ \ req respond -> do
    time <- getPOSIXTime
    body <- strictRequestBody req
    let handleReq = H.run $ inTransaction ReadCommitted
          (runWithClaims conf time (app dbStructure conf body) req)
    withResource pool $ \case
      Left err -> respond $ errResponse HT.status500 (cs . show $ err)
      Right c -> do
        resOrError <- handleReq c
        either (respond . pgErrResponse) respond resOrError
