{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports

module Main where
import Dbapi
import Network.Wai.Handler.Warp hiding (Connection)
import Database.HDBC.PostgreSQL (connectPostgreSQL')

import Control.Applicative
import Options.Applicative hiding (columns)

-- }}}

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db" <> short 'd' <> metavar "URI"
    <> help "database uri to expose, e.g. postgres://user:pass@host:port/database")
  <*> option (long "port" <> short 'p' <> metavar "NUMBER" <> value 3000
    <> help "port number on which to run HTTP server")

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)
  let port = configPort conf
  let dburi = configDbUri conf

  Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)
  conn <- connectPostgreSQL' dburi
  run port $ app conn

  where
    describe = progDesc "create a REST API to an existing Postgres database"
