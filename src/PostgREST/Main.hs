{-# LANGUAGE CPP #-}

module PostgREST.Main where


import           PostgREST.App
import           PostgREST.Config                     (AppConfig (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)
import           PostgREST.DbStructure
import           PostgREST.Error                      (pgErrResponse)
import           PostgREST.Middleware
import           PostgREST.Types                      (DbStructure)

import           Control.Monad
import           Control.Monad.Error.Class            (catchError, throwError)
import           Data.Monoid                          ((<>))
import           Data.String.Conversions              (cs)
import           Data.Time.Clock.POSIX                (getPOSIXTime)
import qualified Hasql.Query                          as H
import qualified Hasql.Session                        as H
import qualified Hasql.Transaction                    as HT
import qualified Hasql.Decoders                       as HD
import qualified Hasql.Encoders                       as HE
import qualified Hasql.Pool                           as P
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
      pgSettings = cs (configDatabase conf)
      appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings

  unless (secret "secret" /= configJwtSecret conf) $
    putStrLn "WARNING, running in insecure mode, JWT secret is the default value"
  Prelude.putStrLn $ "Listening on port " ++
    (show $ configPort conf :: String)


  pool <- P.acquire (configPool conf, 10, pgSettings)

#ifndef mingw32_HOST_OS
  tid <- myThreadId
  void $ installHandler keyboardSignal (Catch $ do
      P.release pool
      throwTo tid UserInterrupt
    ) Nothing
#endif

  result <- P.use pool $ do
    supported <- isServerVersionSupported
    unless supported $ error (
      "Cannot run in this PostgreSQL version, PostgREST needs at least "
      <> show minimumPgVersion)
    getDbStructure (cs $ configSchema conf)

  let dbStructure = either (error.show) id result
  runSettings appSettings $ postgrest conf dbStructure pool

postgrest :: AppConfig -> DbStructure -> P.Pool -> Application
postgrest conf dbStructure pool =
  let middle = (if configQuiet conf then id else logStdout) . defaultMiddle in

  middle $ \ req respond -> do
    time <- getPOSIXTime
    body <- strictRequestBody req

    let handleReq = runWithClaims conf time (app dbStructure conf body) req
    resp <- either pgErrResponse id <$> P.use pool
      (rollbackOnError $ HT.run handleReq HT.ReadCommitted HT.Write)
    respond resp

 where
  rollbackOnError :: H.Session a -> H.Session a
  rollbackOnError = (`catchError` ((H.sql "rollback;" >>) . throwError))
