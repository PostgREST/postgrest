{-# LANGUAGE CPP #-}

module Main where


import           PostgREST.App            (handleRequest)
import           PostgREST.Config         (AppConfig (..), minimumPgVersion,
                                           prettyVersion, readOptions)
import           PostgREST.DbStructure

import           Control.Monad
import           Data.Monoid              ((<>))
import           Data.String.Conversions  (cs)

import qualified Hasql.Decoders           as HD
import qualified Hasql.Encoders           as HE
import qualified Hasql.Pool               as P
import qualified Hasql.Query              as H
import qualified Hasql.Session            as H
import           Network.Wai.Handler.Warp

import           System.IO                (BufferMode (..), hSetBuffering,
                                           stderr, stdin, stdout)
import           Web.JWT                  (secret)
#ifndef mingw32_HOST_OS
import           Control.Concurrent       (myThreadId)
import           Control.Exception.Base   (AsyncException (..), throwTo)
import           System.Posix.Signals
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
  runSettings appSettings $ handleRequest conf dbStructure pool
