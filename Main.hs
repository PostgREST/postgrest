{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative

import Database.HDBC.PostgreSQL (connectPostgreSQL)

import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method

import Options.Applicative hiding (columns)

import PgStructure (printTables, printColumns)
import PgQuery     (selectWhere)

import Debug.Trace

traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x

data AppConfig = AppConfig {
    configDbUri :: String
  , configPort  :: Int }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db" <> short 'd' <> metavar "URI"
    <> help "database uri to expose, e.g. postgres://user:pass@host:port/database")
  <*> option (long "port" <> short 'p' <> metavar "NUMBER" <> value 3000
    <> help "port number on which to run HTTP server")

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)

  Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)
  run (configPort conf) $ app conf

  where
    describe = progDesc "create a REST API to an existing Postgres database"

app ::  AppConfig -> Application
app config req respond =
  case path of
    []      -> respond =<< responseLBS status200 [json] <$> (printTables =<< conn)
    [table] -> respond =<< (<$>) (responseLBS status200 [json])
                               ( if verb == methodOptions
                                 then printColumns table =<< conn
                                 else selectWhere table qq =<< conn )
    _       -> respond $ responseLBS status404 [] ""
  where
    path = pathInfo req
    verb = requestMethod req
    json = (hContentType, "application/json")
    conn = connectPostgreSQL $ configDbUri config
    qq   = queryString req
