{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Main where

import Paths_postgrest (version)

import PostgREST.App
import PostgREST.Middleware
import PostgREST.Error(errResponse)

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Network.Wai (strictRequestBody)
import Network.Wai.Handler.Warp hiding (Connection)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.List (intercalate)
import Data.Version (versionBranch)
import Data.Functor.Identity
import Data.Text(Text)
import qualified Hasql as H
import qualified Hasql.Postgres as P
import Options.Applicative hiding (columns)

import System.IO (stderr, stdin, stdout, hSetBuffering, BufferMode(..))

import PostgREST.Config (AppConfig(..), argParser)

isServerVersionSupported = do
  Identity (row :: Text) <- H.tx Nothing $ H.singleEx $ [H.stmt|SHOW server_version_num|]
  return $ read (cs row) >= 90200

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  let opts = info (helper <*> argParser) $
                fullDesc
                <> progDesc (
                    "PostgREST "
                    <> prettyVersion
                    <> " / create a REST API to an existing Postgres database"
                )
      parserPrefs = prefs showHelpOnError
  conf <- customExecParser parserPrefs opts
  let port = configPort conf

  unless (configSecure conf) $
    putStrLn "WARNING, running in insecure mode, auth will be in plaintext"
  unless ("secret" /= configJwtSecret conf) $
    putStrLn "WARNING, running in insecure mode, JWT secret is the default value"
  Prelude.putStrLn $ "Listening on port " ++
    (show $ configPort conf :: String)

  let pgSettings = P.ParamSettings (cs $ configDbHost conf)
                     (fromIntegral $ configDbPort conf)
                     (cs $ configDbUser conf)
                     (cs $ configDbPass conf)
                     (cs $ configDbName conf)
      appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings
      middle = logStdout . defaultMiddle (configSecure conf)

  poolSettings <- maybe (fail "Improper session settings") return $
                H.poolSettings (fromIntegral $ configPool conf) 30
  pool :: H.Pool P.Postgres
          <- H.acquirePool pgSettings poolSettings

  resOrError <- H.session pool isServerVersionSupported
  either (fail . show) (\supported -> unless supported $ fail "Cannot run in this PostgreSQL version, PostgREST needs at least 9.2.0") resOrError

  runSettings appSettings $ middle $ \req respond -> do
    body <- strictRequestBody req
    resOrError <- liftIO $ H.session pool $ H.tx Nothing $
      authenticated conf (app conf body) req
    either (respond . errResponse) respond resOrError

  where
    prettyVersion = intercalate "." $ map show $ versionBranch version
