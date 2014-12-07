module Config where

import Network.Wai
import Control.Applicative
import Data.Text (strip)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BS
import Data.String.Conversions (cs)
import Options.Applicative hiding (columns)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..))

data AppConfig = AppConfig {
    configDbUri :: String
  , configPort  :: Int
  , configAnonRole :: String
  , configSecure :: Bool
  , configPool :: Int
  }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db" <> short 'd' <> metavar "URI"
    <> help "database uri to expose, e.g. postgres://user:pass@host:port/database")
  <*> option (long "port" <> short 'p' <> metavar "NUMBER" <> value 3000
    <> help "port number on which to run HTTP server")
  <*> strOption (long "anonymous" <> short 'a' <> metavar "ROLE"
    <> help "postgres role to use for non-authenticated requests")
  <*> switch (long "secure" <> short 's'
    <> help "Redirect all requests to HTTPS")
  <*> option (long "db-pool" <> metavar "NUMBER" <> value 10
    <> help "Max connections in database pool")

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      corsOrigins = Just ([origin], True)
    , corsRequestHeaders = "Authentication":accHeaders
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . cs . strip . cs) $ BS.split ',' hdrs
      Nothing -> []
