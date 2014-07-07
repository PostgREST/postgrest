{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative

import Database.PostgreSQL.Simple

import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method

import Data.Aeson (encode)

import Options.Applicative hiding (columns)

import PgStructure (printTables, printColumns, selectAll)

import Data.Text (unpack)
import Web.Heroku.Postgres (parseDatabaseUrl)

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
  run port $ app conf

  where
    port = configPort conf

connInfo :: AppConfig -> ConnectInfo
connInfo config = defaultConnectInfo {
    connectHost = host,
    connectUser = user,
    connectPassword = pass,
    connectDatabase = db
  }
  where
    opts = parseDatabaseUrl $ configDb config
    fromOpt key = maybe (connectHost defaultConnectInfo) unpack $ lookup key opts
    [host, user, pass, db] = map fromOpt ["host", "user", "password", "dbname"]

app ::  AppConfig -> Application
app config req respond =
  case path of
    []      -> respond =<< responseLBS status200 [json] <$> (printTables =<< conn)
    [table] -> if verb == methodOptions
              then respond =<< responseLBS status200 [json] <$> (printColumns table =<< conn)
              else respond =<< responseLBS status200 [json] <$> encode <$> (selectAll table =<< conn)
    _       -> respond $ responseLBS status404 [] ""
  where
    path = pathInfo req
    verb = requestMethod req
    json = (hContentType, "application/json")
    conn = connect $ connInfo config
