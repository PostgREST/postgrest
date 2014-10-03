{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports

module Main where
import Dbapi
import Network.Wai.Handler.Warp hiding (Connection)
import Database.HDBC.PostgreSQL (connectPostgreSQL')
import Data.String.Conversions (cs)

import Control.Applicative
import Options.Applicative hiding (columns)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import Network.Wai.Middleware.Gzip (gzip, def)

-- }}}

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db" <> short 'd' <> metavar "URI"
    <> help "database uri to expose, e.g. postgres://user:pass@host:port/database")
  <*> option (long "port" <> short 'p' <> metavar "NUMBER" <> value 3000
    <> help "port number on which to run HTTP server")
  <*> strOption (long "sslcert" <> short 'c' <> metavar "PATH" <> value "test/test.crt"
    <> help "path to SSL cert file")
  <*> strOption (long "sslkey" <> short 'k' <> metavar "PATH" <> value "test/test.key"
    <> help "path to SSL key file")
  <*> strOption (long "anonymous" <> short 'a' <> metavar "ROLE"
    <> help "postgres role to use for non-authenticated requests")

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)
  let port = configPort conf
  let dburi = configDbUri conf

  let tls = tlsSettings (configSslCert conf) (configSslKey conf)
  let settings = setPort port defaultSettings

  Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)
  conn <- connectPostgreSQL' dburi
  runTLS tls settings $ gzip def $ app conn (cs $ configAnonRole conf)

  where
    describe = progDesc "create a REST API to an existing Postgres database"
