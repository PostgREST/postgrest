{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative

import Database.PostgreSQL.Simple

import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Network.HTTP.Types.Status

import Options.Applicative hiding (columns)

import PgStructure (printTables, printColumns)

data AppConfig = AppConfig {
    configDb   :: String
  , configPort :: Int }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db" <> short 'd' <> metavar "URI"
    <> help "database uri to expose, e.g. postgres://user:pass@host:port/database")
  <*> option (long "port" <> short 'p' <> metavar "NUMBER" <> value 3000
    <> help "port number on which to run HTTP server")

main :: IO ()
main = execParser (info (helper <*> argParser) describe) >>= exposeDb
  where describe = progDesc "create a REST API to an existing Postgres database"

exposeDb :: AppConfig -> IO ()
exposeDb conf = do
  Prelude.putStrLn $ "Listening on port " ++ show port
  run port app

  where
    port = configPort conf

app :: Application
app req respond =
  case path of
    []      -> respond =<< responseLBS status200 [] <$> (printTables =<< conn)
    [table] -> respond =<< responseLBS status200 [] <$> (printColumns table =<< conn)
    _       -> respond $   responseLBS status404 [] ""
  where
    path = pathInfo req
    conn = connect defaultConnectInfo {
      connectDatabase = "dbapi_test"
    }
