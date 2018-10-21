{-# LANGUAGE LambdaCase, TemplateHaskell #-}
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
                        , docsVersion
                        , readOptions
                        , corsPolicy
                        , AppConfig (..)
                        )
       where

import           Control.Applicative
import           Control.Monad                (fail)
import           Control.Lens                 (preview)
import           Crypto.JWT                   (StringOrURI,
                                               stringOrUri)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BS
import qualified Data.CaseInsensitive         as CI
import qualified Data.Configurator            as C
import qualified Data.Configurator.Parser     as C
import           Data.Configurator.Types      as C
import           Data.List                    (lookup)
import           Data.Monoid
import           Data.Scientific              (floatingOrInteger)
import           Data.String                  (String)
import           Data.Text                    (dropAround,
                                               intercalate, lines,
                                               strip, take)
import           Data.Text.Encoding           (encodeUtf8)
import           Data.Text.IO                 (hPutStrLn)
import           Data.Version                 (versionBranch)
import           Development.GitRev           (gitHash)
import           Network.Wai
import           Network.Wai.Middleware.Cors  (CorsResourcePolicy (..))
import           Options.Applicative          hiding (str)
import           Paths_postgrest              (version)
import           PostgREST.Parsers            (pRoleClaimKey)
import           PostgREST.Types              (ApiRequestError(..),
                                               JSPath, JSPathExp(..))
import           Protolude                    hiding (hPutStrLn, take,
                                               intercalate, (<>))
import           System.IO                    (hPrint)
import           System.IO.Error              (IOError)
import           Text.Heredoc
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as L

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
  , configJwtAudience       :: Maybe StringOrURI

  , configPool              :: Int
  , configMaxRows           :: Maybe Integer
  , configReqCheck          :: Maybe Text
  , configQuiet             :: Bool
  , configSettings          :: [(Text, Text)]
  , configRoleClaimKey      :: Either ApiRequestError JSPath
  }

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PATCH", "PUT", "DELETE", "OPTIONS"] ["Authorization"] Nothing
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
prettyVersion =
  intercalate "." (map show $ versionBranch version)
  <> " (" <> take 7 $(gitHash) <> ")"

-- | Version number used in docs
docsVersion :: Text
docsVersion = "v" <> dropAround (== '.') (dropAround (/= '.') prettyVersion)

-- | Function to read and parse options from the command line
readOptions :: IO AppConfig
readOptions = do
  -- First read the config file path from command line
  cfgPath <- customExecParser parserPrefs opts
  -- Now read the actual config file
  conf <- catch
    (C.readConfig =<< C.load [C.Required cfgPath])
    configNotfoundHint

  let (mAppConf, errs) = flip C.runParserM conf $
        AppConfig
          <$> C.key "db-uri"
          <*> C.key "db-anon-role"
          <*> (mfilter (/= "") <$> C.key "server-proxy-uri")
          <*> C.key "db-schema"
          <*> (fromMaybe "127.0.0.1" . mfilter (/= "") <$> C.key "server-host")
          <*> (fromMaybe 3000 . join . fmap coerceInt <$> C.key "server-port")
          <*> (fmap encodeUtf8 . mfilter (/= "") <$> C.key "jwt-secret")
          <*> (fromMaybe False . join . fmap coerceBool <$> C.key "secret-is-base64")
          <*> parseJwtAudience "jwt-aud"
          <*> (fromMaybe 10 . join . fmap coerceInt <$> C.key "db-pool")
          <*> (join . fmap coerceInt <$> C.key "max-rows")
          <*> (mfilter (/= "") <$> C.key "pre-request")
          <*> pure False
          <*> (fmap (fmap coerceText) <$> C.subassocs "app.settings")
          <*> (maybe (Right [JSPKey "role"]) parseRoleClaimKey <$> C.key "role-claim-key")

  case mAppConf of
    Nothing -> do
      forM_ errs $ hPrint stderr
      exitFailure
    Just appConf ->
      return appConf

  where
    parseJwtAudience :: Name -> C.ConfigParserM (Maybe StringOrURI)
    parseJwtAudience k =
      C.key k >>= \case
        Nothing -> pure Nothing -- no audience in config file
        Just aud -> case preview stringOrUri (aud :: String) of
          Nothing -> fail "Invalid Jwt audience. Check your configuration."
          (Just "") -> pure Nothing
          aud' -> pure aud'

    coerceText :: Value -> Text
    coerceText (String s) = s
    coerceText v = show v

    coerceInt :: (Read i, Integral i) => Value -> Maybe i
    coerceInt (Number x) = rightToMaybe $ floatingOrInteger x
    coerceInt (String x) = readMaybe $ toS x
    coerceInt _          = Nothing

    coerceBool :: Value -> Maybe Bool
    coerceBool (Bool b)   = Just b
    coerceBool (String b) = readMaybe $ toS b
    coerceBool _          = Nothing

    parseRoleClaimKey :: Value -> Either ApiRequestError JSPath
    parseRoleClaimKey (String s) = pRoleClaimKey s
    parseRoleClaimKey v = pRoleClaimKey $ show v

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
          |server-host = "127.0.0.1"
          |server-port = 3000
          |
          |## base url for swagger output
          |# server-proxy-uri = ""
          |
          |## choose a secret to enable JWT auth
          |## (use "@filename" to load from separate file)
          |# jwt-secret = "foo"
          |# secret-is-base64 = false
          |# jwt-aud = "your_audience_claim"
          |
          |## limit rows in response
          |# max-rows = 1000
          |
          |## stored proc to exec immediately after auth
          |# pre-request = "stored_proc_name"
          |
          |## jspath to the role claim key
          |# role-claim-key = ".role"
          |]

pathParser :: Parser FilePath
pathParser =
  strArgument $
    metavar "FILENAME" <>
    help "Path to configuration file"
