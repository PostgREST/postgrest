{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-|
Module      : PostgREST.Config
Description : Manages PostgREST configuration options.

This module provides a helper function to read the command line
arguments using the optparse-applicative and the AppConfig type to store
them.  It also can be used to define other middleware configuration that
may be delegated to some sort of external configuration.

It currently includes a hardcoded CORS policy but this could easly be
turned in configurable behaviour if needed.

Other hardcoded options such as the minimum version number also belong here.
-}
module PostgREST.Config ( prettyVersion
                        , readOptions
                        , corsPolicy
                        , minimumPgVersion
                        , PgVersion (..)
                        , AppConfig (..)
                        )
       where

import           System.IO.Error             (IOError)
import           Control.Applicative
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BS
import qualified Data.CaseInsensitive        as CI
import qualified Data.Configurator           as C
import qualified Data.Configurator.Parser    as C
import           Data.Configurator.Types     (Value(..))
import           Data.List                   (lookup)
import           Data.Monoid
import           Data.Scientific             (floatingOrInteger)
import           Data.Text                   (strip, intercalate, lines)
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Text.IO                (hPutStrLn)
import           Data.Version                (versionBranch)
import           Network.Wai
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..))
import           Options.Applicative hiding  (str)
import           Paths_postgrest             (version)
import           Text.Heredoc
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as L
import           Protolude hiding            (intercalate, (<>))

-- | Config file settings for the server
data AppConfig = AppConfig {
    configDatabase          :: Text
  , configAnonRole          :: Text
  , configProxyUri          :: Maybe Text
  , configSchema            :: Text
  , configHost              :: Text
  , configPort              :: Int

  , configJwtSecret         :: Maybe B.ByteString
  , configJwtSecretIsBase64 :: Bool

  , configPool              :: Int
  , configMaxRows           :: Maybe Integer
  , configReqCheck          :: Maybe Text
  , configQuiet             :: Bool
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
  -- First read the config file path from command line
  cfgPath <- customExecParser parserPrefs opts
  -- Now read the actual config file
  conf <- catch
    (C.readConfig =<< C.load [C.Required cfgPath])
    configNotfoundHint

  let (mAppConf, errs) = flip C.runParserA conf $
        AppConfig <$>
            C.key "db-uri"
          <*> C.key "db-anon-role"
          <*> C.key "server-proxy-uri"
          <*> C.key "db-schema"
          <*> (fromMaybe "*4" <$> C.key "server-host")
          <*> (fromMaybe 3000 . join . fmap coerceInt <$> C.key "server-port")
          <*> (fmap encodeUtf8 <$> C.key "jwt-secret")
          <*> (fromMaybe False <$> C.key "secret-is-base64")
          <*> (fromMaybe 10 . join . fmap coerceInt <$> C.key "db-pool")
          <*> (join . fmap coerceInt <$> C.key "max-rows")
          <*> C.key "pre-request"
          <*> pure False

  case mAppConf of
    Nothing -> do
      forM_ errs $ hPutStrLn stderr . show
      exitFailure
    Just appConf ->
      return appConf

 where
  coerceInt :: (Read i, Integral i) => Value -> Maybe i
  coerceInt (Number x) = rightToMaybe $ floatingOrInteger x
  coerceInt (String x) = readMaybe $ toS x
  coerceInt _ = Nothing

  opts = info (helper <*> pathParser) $
           fullDesc
           <> progDesc (
               "PostgREST "
               <> toS prettyVersion
               <> " / create a REST API to an existing Postgres database"
             )
           <> footerDoc (Just $
               text "Example Config File:"
               L.<> nest 2 (hardline L.<> exampleCfg)
             )

  parserPrefs = prefs showHelpOnError

  configNotfoundHint :: IOError -> IO a
  configNotfoundHint e = do
    hPutStrLn stderr $
      "Cannot open config file:\n\t" <> show e
    exitFailure

  exampleCfg :: Doc
  exampleCfg = vsep . map (text . toS) . lines $
    [str|db-uri = "postgres://user:pass@localhost:5432/dbname"
        |db-schema = "public"
        |db-anon-role = "postgres"
        |db-pool = 10
        |
        |server-host = "*4"
        |server-port = 3000
        |
        |## base url for swagger output
        |# server-proxy-uri = ""
        |
        |## choose a secret to enable JWT auth
        |## (use "@filename" to load from separate file)
        |# jwt-secret = "foo"
        |# secret-is-base64 = false
        |
        |## limit rows in response
        |# max-rows = 1000
        |
        |## stored proc to exec immediately after auth
        |# pre-request = "stored_proc_name"
        |]

pathParser :: Parser FilePath
pathParser =
  strArgument $
    metavar "FILENAME" <>
    help "Path to configuration file"

data PgVersion = PgVersion {
  pgvNum  :: Int32
, pgvName :: Text
}

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: PgVersion
minimumPgVersion = PgVersion 90300 "9.3"
