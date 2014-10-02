{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports

module Main where
import Dbapi
import Network.Wai.Handler.Warp hiding (Connection)
import Database.HDBC.PostgreSQL (connectPostgreSQL')
import Data.String.Conversions (cs)
import qualified Data.CaseInsensitive as CI
import Data.Text (strip);
import qualified Data.ByteString.Char8 as BS

import Control.Applicative
import Options.Applicative hiding (columns)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors)

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

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PUT", "PATCH", "DELETE"] ["authorization"] Nothing
  (Just $ 60*60*24) False False True

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      corsOrigins = Just ([origin], True),
      corsRequestHeaders = "authentication":accHeaders
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . cs . strip . cs) $ BS.split ',' hdrs
      Nothing -> []

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)
  let port = configPort conf
  let dburi = configDbUri conf

  let tls = tlsSettings (configSslCert conf) (configSslKey conf)
  let settings = setPort port defaultSettings

  Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)
  conn <- connectPostgreSQL' dburi
  runTLS tls settings $ gzip def $ cors corsPolicy $ app conn (cs $ configAnonRole conf)

  where
    describe = progDesc "create a REST API to an existing Postgres database"
