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
import           Data.List                   (lookup)
import           Data.Text                   (strip, intercalate)
import           Data.Version                (versionBranch)
import           Network.Wai
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..))
import           Options.Applicative
import           Paths_postgrest             (version)
import           Protolude hiding            (intercalate)
import           Safe                        (readMay)
import           Web.JWT                     (Secret, secret)

-- | Data type to store all command line options
data AppConfig = AppConfig {
    configDatabase  :: Text
  , configAnonRole  :: Text
  , configProxyUri  :: Maybe Text
  , configSchema    :: Text
  , configHost      :: Text
  , configPort      :: Int
  , configJwtSecret :: Secret
  , configPool      :: Int
  , configMaxRows   :: Maybe Integer
  , configQuiet     :: Bool
  }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> (toS <$> argument str (help "(REQUIRED) database connection string, e.g. postgres://user:pass@host:port/db" <> metavar "DB_URL"))
  <*> (toS <$> strOption    (long "anonymous"  <> short 'a' <> help "(REQUIRED) postgres role to use for non-authenticated requests" <> metavar "ROLE"))
  <*> (optional . map toS . strOption) (long "proxy-uri"  <> short 'x' <> help "proxy uri of the HTTP server" <> metavar "PROXY")
  <*> (toS <$> strOption    (long "schema"     <> short 's' <> help "schema to use for API routes" <> metavar "NAME" <> value "public" <> showDefault))
  <*> (toS <$> strOption    (long "host"       <> short 'l' <> help "hostname or ip on which to run HTTP server" <> metavar "HOST" <> value "*4" <> showDefault))
  <*> option auto  (long "port"       <> short 'p' <> help "port number on which to run HTTP server" <> metavar "PORT" <> value 3000 <> showDefault)
  <*> (secret . toS <$>
      strOption    (long "jwt-secret" <> short 'j' <> help "secret used to encrypt and decrypt JWT tokens" <> metavar "SECRET" <> value "secret" <> showDefault))
  <*> option auto  (long "pool"       <> short 'o' <> help "max connections in database pool" <> metavar "COUNT" <> value 10 <> showDefault)
  <*> (readMay <$> strOption  (long "max-rows"   <> short 'm' <> help "max rows in response" <> metavar "COUNT" <> value "infinity" <> showDefault))
  <*> pure False

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PATCH", "DELETE", "OPTIONS"] ["Authorization"] Nothing
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
      Just hdrs -> map (CI.mk . toS . strip . toS) $ BS.split ',' hdrs
      Nothing -> []

-- | User friendly version number
prettyVersion :: Text
prettyVersion = intercalate "." $ map show $ versionBranch version

-- | Function to read and parse options from the command line
readOptions :: IO AppConfig
readOptions = customExecParser parserPrefs opts
  where
    opts = info (helper <*> argParser) $
                    fullDesc
                    <> progDesc (
                    "PostgREST "
                    <> toS prettyVersion
                    <> " / create a REST API to an existing Postgres database"
                    )
    parserPrefs = prefs showHelpOnError

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: Integer
minimumPgVersion = 90300
