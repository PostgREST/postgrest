{-|
Module      : PostgREST.Config
Description : Manages PostgREST configuration options.

This module provides a helper function to read the command line arguments using the optparse-applicative
and the AppConfig type to store them.
It also can be used to define other middleware configuration that may be delegated to some sort of
external configuration.

It currently includes a hardcoded CORS policy but this could easly be turned in configurable behaviour if needed.

Other hardcoded options such as the minimum version number also belong here.
-}
module PostgREST.Config ( prettyVersion
                        , readOptions
                        , corsPolicy
                        , minimumPgVersion
                        , AppConfig (..)
                        )
       where

import           Control.Applicative
import qualified Data.ByteString.Char8       as BS
import qualified Data.CaseInsensitive        as CI
import           Data.List                   (intercalate)
import           Data.String.Conversions     (cs)
import           Data.Text                   (strip)
import           Data.Version                (versionBranch)
import           Network.Wai
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..))
import           Options.Applicative         hiding (columns)
import           Paths_postgrest             (version)
import           Prelude

-- | Data type to store all command line options
data AppConfig = AppConfig {
    configDbName    :: String
  , configDbPort    :: Int
  , configDbUser    :: String
  , configDbPass    :: String
  , configDbHost    :: String

  , configPort      :: Int
  , configAnonRole  :: String
  , configSecure    :: Bool
  , configPool      :: Int
  , configV1Schema  :: String
  , configJwtSecret :: String
  }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db-name" <> short 'd' <> metavar "NAME" <> help "name of database")
  <*> option auto (long "db-port" <> short 'P' <> metavar "PORT" <> value 5432 <> help "postgres server port" <> showDefault)
  <*> strOption (long "db-user" <> short 'U' <> metavar "ROLE" <> help "postgres authenticator role")
  <*> strOption (long "db-pass" <> metavar "PASS" <> value "" <> help "password for authenticator role")
  <*> strOption (long "db-host" <> metavar "HOST" <> value "localhost" <> help "postgres server hostname" <> showDefault)

  <*> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 3000 <> help "port number on which to run HTTP server" <> showDefault)
  <*> strOption (long "anonymous" <> short 'a' <> metavar "ROLE" <> help "postgres role to use for non-authenticated requests")
  <*> switch (long "secure" <> short 's' <> help "Redirect all requests to HTTPS")
  <*> option auto (long "db-pool" <> metavar "COUNT" <> value 10 <> help "Max connections in database pool" <> showDefault)
  <*> strOption (long "v1schema" <> metavar "NAME" <> value "1" <> help "Schema to use for nonspecified version (or explicit v1)" <> showDefault)
  <*> strOption (long "jwt-secret" <> metavar "SECRET" <> value "secret" <> help "Secret used to encrypt and decrypt JWT tokens)" <> showDefault)

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

-- | CORS policy to be used in by Wai Cors middleware
corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      corsOrigins = Just ([origin], True)
    , corsRequestHeaders = "Authentication":accHeaders
    , corsExposedHeaders = Just [
        "Content-Encoding", "Content-Location", "Content-Range", "Content-Type"
      , "Date", "Location", "Server", "Transfer-Encoding", "Range-Unit"
      ]
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . cs . strip . cs) $ BS.split ',' hdrs
      Nothing -> []

-- | User friendly version number
prettyVersion :: String
prettyVersion = intercalate "." $ map show $ versionBranch version

-- | Function to read and parse options from the command line
readOptions :: IO AppConfig
readOptions = customExecParser parserPrefs opts
  where
    opts = info (helper <*> argParser) $
                    fullDesc
                    <> progDesc (
                    "PostgREST "
                    <> prettyVersion
                    <> " / create a REST API to an existing Postgres database"
                    )
    parserPrefs = prefs showHelpOnError

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: Integer
minimumPgVersion = 90200
