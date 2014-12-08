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
    configDbName :: String
  , configDbPort :: Int
  , configDbUser :: String
  , configDbPass :: String
  , configDbHost :: String

  , configPort  :: Int
  , configAnonRole :: String
  , configSecure :: Bool
  , configPool :: Int
  }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db-name" <> short 'd'                     <> help "name of database")
  <*> option    (long "db-port" <> short 'P' <> value 5432        <> help "postgres server port")
  <*> strOption (long "db-user" <> short 'U'                     <> help "postgres authenticator role")
  <*> strOption (long "db-pass"             <> value ""          <> help "password for authenticator role")
  <*> strOption (long "db-host" <> short 'h' <> value "localhost" <> help "postgres server hostname")

  <*> option    (long "port" <> short 'p' <> value 3000 <> help "port number on which to run HTTP server")
  <*> strOption (long "anonymous" <> short 'a' <> help "postgres role to use for non-authenticated requests")
  <*> switch    (long "secure" <> short 's' <> help "Redirect all requests to HTTPS")
  <*> option    (long "db-pool" <> value 10 <> help "Max connections in database pool")

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
