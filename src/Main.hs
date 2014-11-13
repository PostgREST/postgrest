module Main where

import Paths_dbapi (version)

import App
import Middleware (inTransaction, authenticated, withSavepoint, clientErrors,
  redirectInsecure, withDBConnection, Environment(..))

import Control.Monad (unless)
import Control.Exception(bracket)
import Data.String.Conversions (cs)
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Handler.Warp hiding (Connection)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.Static (staticPolicy, only)
import Data.Pool(createPool, destroyAllResources)
import Data.List (intercalate)
import Data.Version (versionBranch)
import Database.PostgreSQL.Simple
import Options.Applicative hiding (columns)

import Config (AppConfig(..), argParser, corsPolicy)

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)
  bracket
    (createPool (connectPostgreSQL $ cs (configDbUri conf))
      close 1 600 (configPool conf))
    destroyAllResources
    (\pool -> do
      let port = configPort conf

      unless (configSecure conf) $
        putStrLn "WARNING, running in insecure mode, auth will be in plaintext"

      Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)
      let settings = setPort port
                   . setServerName (cs $ "dbapi/" <> prettyVersion)
                   $ defaultSettings
      runSettings settings $ (if configSecure conf then redirectInsecure else id)
        . gzip def . cors corsPolicy . clientErrors
        . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
        . withDBConnection pool . inTransaction Production
        . authenticated (cs $ configAnonRole conf) . Middleware.withSavepoint Production $ app
      )
  where
    describe = progDesc "create a REST API to an existing Postgres database"
    prettyVersion = intercalate "." $ map show $ versionBranch version
