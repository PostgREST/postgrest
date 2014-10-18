{-# LANGUAGE OverloadedStrings #-}

module Main where
import Dbapi
import Middleware (inTransaction, authenticated, withSavepoint, clientErrors,
  redirectInsecure, withDBConnection)
import Network.Wai.Handler.Warp hiding (Connection)
import Data.String.Conversions (cs)

import Control.Monad (unless)
import Control.Applicative
import Options.Applicative hiding (columns)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Middleware.Static (staticPolicy, only)
import Database.HDBC (disconnect)
import Database.HDBC.PostgreSQL(connectPostgreSQL')
import Data.Pool(createPool)

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db" <> short 'd' <> metavar "URI"
    <> help "database uri to expose, e.g. postgres://user:pass@host:port/database")
  <*> option (long "port" <> short 'p' <> metavar "NUMBER" <> value 3000
    <> help "port number on which to run HTTP server")
  <*> strOption (long "anonymous" <> short 'a' <> metavar "ROLE"
    <> help "postgres role to use for non-authenticated requests")
  <*> switch (long "secure" <> short 's'
    <> help "Redirect all requests to HTTPS" )

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)
  pool <- createPool (connectPostgreSQL' (configDbUri conf)) disconnect 1 600 10
  let port = configPort conf

  unless (configSecure conf) $
    putStrLn "WARNING, running in insecure mode, auth will be in plaintext"

  Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)
  run port $ (if configSecure conf then redirectInsecure else id)
    . gzip def . cors corsPolicy . clientErrors
    . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
    . withDBConnection pool . inTransaction
    . authenticated (cs $ configAnonRole conf) . withSavepoint $ app
  where
    describe = progDesc "create a REST API to an existing Postgres database"
