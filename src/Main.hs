module Main where

import Paths_postgrest (version)

import App
import Middleware

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Data.String.Conversions (cs)
import Network.Wai (strictRequestBody)
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Handler.Warp hiding (Connection)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.Static (staticPolicy, only)
import Data.List (intercalate)
import Data.Version (versionBranch)
import qualified Hasql as H
import qualified Hasql.Postgres as H
import Options.Applicative hiding (columns)

import Config (AppConfig(..), argParser, corsPolicy)

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)
  let port = configPort conf

  unless (configSecure conf) $
    putStrLn "WARNING, running in insecure mode, auth will be in plaintext"
  Prelude.putStrLn $ "Listening on port " ++
    (show $ configPort conf :: String)

  let pgSettings = H.ParamSettings (cs $ configDbHost conf)
                     (fromIntegral $ configDbPort conf)
                     (cs $ configDbUser conf)
                     (cs $ configDbPass conf)
                     (cs $ configDbName conf)

  sessSettings <- maybe (fail "Improper session settings") return $
                    H.sessionSettings (fromIntegral $ configPool conf) 30

  let appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings
      middle =
        (if configSecure conf then redirectInsecure else id)
        . gzip def . cors corsPolicy
        . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
      anonRole = cs $ configAnonRole conf

  H.session pgSettings sessSettings $ H.sessionUnlifter >>= \unlift ->
    liftIO $ runSettings appSettings $ middle $ \req respond -> do
      body <- strictRequestBody req
      respond =<< catchJust isSqlError
        (unlift $ H.tx Nothing
                $ authenticated anonRole (app body) req)
        (return . sqlError)

  where
    describe = progDesc "create a REST API to an existing Postgres database"
    prettyVersion = intercalate "." $ map show $ versionBranch version
