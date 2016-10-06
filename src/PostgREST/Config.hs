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

import qualified Data.ByteString.Char8       as BS
import qualified Data.CaseInsensitive        as CI
import qualified Data.Configurator           as C
import           Data.List                   (lookup)
import           Data.Text                   (strip, intercalate)
import           Data.Version                (versionBranch)
import           Network.Wai
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..))
import           Options.Applicative
import           Paths_postgrest             (version)
import           Protolude hiding            (intercalate
                                             , (<>))
import           System.Environment.XDG.BaseDir

-- | Config file settings for the server
data AppConfig = AppConfig {
    configDatabase  :: Text
  , configAnonRole  :: Text
  , configProxyUri  :: Maybe Text
  , configSchema    :: Text
  , configHost      :: Text
  , configPort      :: Int
  , configJwtSecret :: Maybe Text
  , configPool      :: Int
  , configMaxRows   :: Maybe Integer
  , configReqCheck  :: Maybe Text
  , configQuiet     :: Bool
  }

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
readOptions = do
  confArg <- customExecParser parserPrefs opts
  defConf <- getUserConfigFile "postgrest" "config"

  conf <- C.load [C.Required $ fromMaybe defConf confArg]
  -- db ----------------
  cDbUri    <- C.require conf "db.uri"
  cDbSchema <- C.require conf "db.schema"
  cDbAnon   <- C.require conf "db.anon-role"
  cPool     <- C.lookupDefault 10 conf "db.pool"
  -- server ------------
  cHost     <- C.lookupDefault "*4" conf "server.host"
  cPort     <- C.lookupDefault 3000 conf "server.port"
  cProxy    <- C.lookup conf "server.proxy-uri"
  -- jwt ---------------
  cJwtSec   <- C.lookup conf "jwt.secret"
  -- safety ------------
  cMaxRows  <- C.lookup conf "safety.max-rows"
  cReqCheck <- C.lookup conf "safety.pre-request"

  return $ AppConfig cDbUri cDbAnon cProxy cDbSchema cHost cPort
        cJwtSec cPool cMaxRows cReqCheck False

 where
  opts = info (helper <*> argParser) $
                  fullDesc
                  <> progDesc (
                  "PostgREST "
                  <> toS prettyVersion
                  <> " / create a REST API to an existing Postgres database"
                  )
  parserPrefs = prefs showHelpOnError

  argParser :: Parser (Maybe FilePath)
  argParser =
    (optional . map toS <$> strOption)
      (short 'c' <> metavar "filename" <>
       help "Configuration file instead of default")

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: Integer
minimumPgVersion = 90300
