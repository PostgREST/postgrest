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
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module PostgREST.Config ( prettyVersion
                        , docsVersion
                        , AppConfig (..)
                        , configPoolTimeout'
                        , readPathShowHelp
                        , readValidateConfig
                        )
       where

import qualified Data.ByteString              as B
import qualified Data.ByteString.Base64       as B64
import qualified Data.ByteString.Char8        as BS
import qualified Data.Configurator            as C
import           Data.Either.Combinators      (whenLeft)
import qualified Text.PrettyPrint.ANSI.Leijen as L

import Control.Lens       (preview)
import Control.Monad      (fail)
import Crypto.JWT         (JWKSet, StringOrURI, stringOrUri)
import Data.List.NonEmpty (fromList)
import Data.Scientific    (floatingOrInteger)
import Data.Text          (dropEnd, dropWhileEnd, intercalate, pack,
                           replace, splitOn, strip, stripPrefix, take,
                           unpack)
import Data.Text.IO       (hPutStrLn)
import Data.Version       (versionBranch)
import Development.GitRev (gitHash)
import Numeric            (readOct)
import Paths_postgrest    (version)
import System.IO.Error    (IOError)
import System.Posix.Types (FileMode)

import Control.Applicative
import Data.Monoid
import Options.Applicative          hiding (str)
import Text.Heredoc
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

import PostgREST.Auth             (parseSecret)
import PostgREST.Parsers          (pRoleClaimKey)
import PostgREST.Private.ProxyUri (isMalformedProxyUri)
import PostgREST.Types            (JSPath, JSPathExp (..),
                                   LogLevel (..))
import Protolude                  hiding (concat, hPutStrLn,
                                   intercalate, null, replace, take,
                                   toS, (<>))
import Protolude.Conv             (toS)


-- | Config file settings for the server
data AppConfig = AppConfig {
    configDbUri             :: Text
  , configAnonRole          :: Text
  , configOpenAPIProxyUri   :: Maybe Text
  , configSchemas           :: NonEmpty Text
  , configHost              :: Text
  , configPort              :: Int
  , configSocket            :: Maybe FilePath
  , configSocketMode        :: Either Text FileMode
  , configDbChannel         :: Text
  , configDbChannelEnabled  :: Bool

  , configJwtSecret         :: Maybe B.ByteString
  , configJwtSecretIsBase64 :: Bool
  , configJwtAudience       :: Maybe StringOrURI

  , configPoolSize          :: Int
  , configPoolTimeout       :: Int
  , configMaxRows           :: Maybe Integer
  , configPreReq            :: Maybe Text
  , configSettings          :: [(Text, Text)]
  , configRoleClaimKey      :: Either Text JSPath
  , configExtraSearchPath   :: [Text]

  , configRootSpec          :: Maybe Text
  , configRawMediaTypes     :: [B.ByteString]

  , configJWKS              :: Maybe JWKSet

  , configLogLevel          :: LogLevel
  }

configPoolTimeout' :: (Fractional a) => AppConfig -> a
configPoolTimeout' =
  fromRational . toRational . configPoolTimeout

-- | User friendly version number
prettyVersion :: Text
prettyVersion =
  intercalate "." (map show $ versionBranch version) <> gitRev
  where
    gitRev =
      if $(gitHash) == "UNKNOWN"
        then mempty
        else " (" <> take 7 $(gitHash) <> ")"

-- | Version number used in docs
docsVersion :: Text
docsVersion = "v" <> dropEnd 1 (dropWhileEnd (/= '.') prettyVersion)

-- | Read config the file path from the command line. Also prints help.
readPathShowHelp :: IO FilePath
readPathShowHelp = customExecParser parserPrefs opts
  where
    parserPrefs = prefs showHelpOnError

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

    pathParser :: Parser FilePath
    pathParser =
      strArgument $
        metavar "FILENAME" <>
        help "Path to configuration file"

    exampleCfg :: Doc
    exampleCfg = vsep . map (text . toS) . lines $
      [str|db-uri = "postgres://user:pass@localhost:5432/dbname"
          |db-schema = "public" # this schema gets added to the search_path of every request
          |db-anon-role = "postgres"
          |# number of open connections in the pool
          |db-pool = 10
          |# Time to live, in seconds, for an idle database pool connection.
          |db-pool-timeout = 10
          |
          |server-host = "!4"
          |server-port = 3000
          |
          |## unix socket location
          |## if specified it takes precedence over server-port
          |# server-unix-socket = "/tmp/pgrst.sock"
          |## unix socket file mode
          |## when none is provided, 660 is applied by default
          |# server-unix-socket-mode = "660"
          |
          |## Notification channel for reloading the schema cache
          |# db-channel = "pgrst"
          |## Enable or disable the notification channel
          |# db-channel-enabled = false
          |
          |## base url for swagger output
          |# openapi-server-proxy-uri = ""
          |
          |## choose a secret, JSON Web Key (or set) to enable JWT auth
          |## (use "@filename" to load from separate file)
          |# jwt-secret = "secret_with_at_least_32_characters"
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
          |
          |## extra schemas to add to the search_path of every request
          |# db-extra-search-path = "extensions, util"
          |
          |## stored proc that overrides the root "/" spec
          |## it must be inside the db-schema
          |# root-spec = "stored_proc_name"
          |
          |## content types to produce raw output
          |# raw-media-types="image/png, image/jpg"
          |
          |## logging level, the admitted values are: crit, error, warn and info.
          |# log-level = "error"
          |]

-- | Parse the config file
readAppConfig :: FilePath -> IO AppConfig
readAppConfig cfgPath = do
  -- Now read the actual config file
  conf <- catches (C.load cfgPath)
    [ Handler (\(ex :: IOError)    -> exitErr $ "Cannot open config file:\n\t" <> show ex)
    , Handler (\(C.ParseError err) -> exitErr $ "Error parsing config file:\n" <> err)
    ]

  case C.runParser parseConfig conf of
    Left err ->
      exitErr $ "Error parsing config file:\n\t" <> err
    Right appConf ->
      return appConf

  where
    parseConfig =
      AppConfig
        <$> reqString "db-uri"
        <*> reqString "db-anon-role"
        <*> optString "openapi-server-proxy-uri"
        <*> (fromList . splitOnCommas <$> reqValue "db-schema")
        <*> (fromMaybe "!4" <$> optString "server-host")
        <*> (fromMaybe 3000 <$> optInt "server-port")
        <*> (fmap unpack <$> optString "server-unix-socket")
        <*> parseSocketFileMode "server-unix-socket-mode"
        <*> (fromMaybe "pgrst" <$> optString "db-channel")
        <*> ((Just True ==) <$> optBool "db-channel-enabled")
        <*> (fmap encodeUtf8 <$> optString "jwt-secret")
        <*> ((Just True ==) <$> optBool "secret-is-base64")
        <*> parseJwtAudience "jwt-aud"
        <*> (fromMaybe 10 <$> optInt "db-pool")
        <*> (fromMaybe 10 <$> optInt "db-pool-timeout")
        <*> optInt "max-rows"
        <*> optString "pre-request"
        <*> (fmap (fmap coerceText) <$> C.subassocs "app.settings" C.value)
        <*> (maybe (Right [JSPKey "role"]) parseRoleClaimKey <$> optValue "role-claim-key")
        <*> (maybe ["public"] splitOnCommas <$> optValue "db-extra-search-path")
        <*> optString "root-spec"
        <*> (maybe [] (fmap encodeUtf8 . splitOnCommas) <$> optValue "raw-media-types")
        <*> pure Nothing
        <*> parseLogLevel "log-level"

    parseSocketFileMode :: C.Key -> C.Parser C.Config (Either Text FileMode)
    parseSocketFileMode k =
      C.optional k C.string >>= \case
        Nothing -> pure $ Right 432 -- return default 660 mode if no value was provided
        Just fileModeText ->
          case (readOct . unpack) fileModeText of
            []              ->
              pure $ Left "Invalid server-unix-socket-mode: not an octal"
            (fileMode, _):_ ->
              if fileMode < 384 || fileMode > 511
                then pure $ Left "Invalid server-unix-socket-mode: needs to be between 600 and 777"
                else pure $ Right fileMode

    parseJwtAudience :: C.Key -> C.Parser C.Config (Maybe StringOrURI)
    parseJwtAudience k =
      C.optional k C.string >>= \case
        Nothing -> pure Nothing -- no audience in config file
        Just aud -> case preview stringOrUri (unpack aud) of
          Nothing -> fail "Invalid Jwt audience. Check your configuration."
          (Just "") -> pure Nothing
          aud' -> pure aud'

    parseLogLevel :: C.Key -> C.Parser C.Config LogLevel
    parseLogLevel k =
      C.optional k C.string >>= \case
        Nothing      -> pure LogError
        Just ""      -> pure LogError
        Just "crit"  -> pure LogCrit
        Just "error" -> pure LogError
        Just "warn"  -> pure LogWarn
        Just "info"  -> pure LogInfo
        Just _       -> fail "Invalid logging level. Check your configuration."

    reqString :: C.Key -> C.Parser C.Config Text
    reqString k = C.required k C.string

    reqValue :: C.Key -> C.Parser C.Config C.Value
    reqValue k = C.required k C.value

    optString :: C.Key -> C.Parser C.Config (Maybe Text)
    optString k = mfilter (/= "") <$> C.optional k C.string

    optValue :: C.Key -> C.Parser C.Config (Maybe C.Value)
    optValue k = C.optional k C.value

    optInt :: (Read i, Integral i) => C.Key -> C.Parser C.Config (Maybe i)
    optInt k = join <$> C.optional k (coerceInt <$> C.value)

    optBool :: C.Key -> C.Parser C.Config (Maybe Bool)
    optBool k = join <$> C.optional k (coerceBool <$> C.value)

    coerceText :: C.Value -> Text
    coerceText (C.String s) = s
    coerceText v            = show v

    coerceInt :: (Read i, Integral i) => C.Value -> Maybe i
    coerceInt (C.Number x) = rightToMaybe $ floatingOrInteger x
    coerceInt (C.String x) = readMaybe $ toS x
    coerceInt _            = Nothing

    coerceBool :: C.Value -> Maybe Bool
    coerceBool (C.Bool b)   = Just b
    coerceBool (C.String b) = readMaybe $ toS b
    coerceBool _            = Nothing

    parseRoleClaimKey :: C.Value -> Either Text JSPath
    parseRoleClaimKey (C.String s) = pRoleClaimKey s
    parseRoleClaimKey v            = pRoleClaimKey $ show v

    splitOnCommas :: C.Value -> [Text]
    splitOnCommas (C.String s) = strip <$> splitOn "," s
    splitOnCommas _            = []

    exitErr :: Text -> IO a
    exitErr err = do
      hPutStrLn stderr err
      exitFailure

-- | Parse the AppConfig and validate it. Panic on invalid config options.
readValidateConfig :: FilePath -> IO AppConfig
readValidateConfig path = do
  conf <- loadDbUriFile =<< loadSecretFile =<< readAppConfig path
  -- Checks that the provided proxy uri is formated correctly
  when (isMalformedProxyUri $ toS <$> configOpenAPIProxyUri conf) $
    panic
      "Malformed proxy uri, a correct example: https://example.com:8443/basePath"
  -- Checks that the provided jspath is valid
  whenLeft (configRoleClaimKey conf) panic
  -- Check the file mode is valid
  whenLeft (configSocketMode conf) panic
  return $ conf { configJWKS = parseSecret <$> configJwtSecret conf}

{-|
  The purpose of this function is to load the JWT secret from a file if
  configJwtSecret is actually a filepath and replaces some characters if the JWT
  is base64 encoded.

  The reason some characters need to be replaced is because JWT is actually
  base64url encoded which must be turned into just base64 before decoding.

  To check if the JWT secret is provided is in fact a file path, it must be
  decoded as 'Text' to be processed.

  decodeUtf8: Decode a ByteString containing UTF-8 encoded text that is known to
  be valid.
-}
loadSecretFile :: AppConfig -> IO AppConfig
loadSecretFile conf = extractAndTransform mSecret
  where
    mSecret = decodeUtf8 <$> configJwtSecret conf
    isB64 = configJwtSecretIsBase64 conf
    --
    -- The Text (variable name secret) here is mSecret from above which is the JWT
    -- decoded as Utf8
    --
    -- stripPrefix: Return the suffix of the second string if its prefix matches
    -- the entire first string.
    --
    -- The configJwtSecret is a filepath instead of the JWT secret itself if the
    -- secret has @ as its prefix.
    extractAndTransform :: Maybe Text -> IO AppConfig
    extractAndTransform Nothing = return conf
    extractAndTransform (Just secret) =
      fmap setSecret $
      transformString isB64 =<<
      case stripPrefix "@" secret of
        Nothing       -> return . encodeUtf8 $ secret
        Just filename -> chomp <$> BS.readFile (toS filename)
      where
        chomp bs = fromMaybe bs (BS.stripSuffix "\n" bs)
    --
    -- Turns the Base64url encoded JWT into Base64
    transformString :: Bool -> ByteString -> IO ByteString
    transformString False t = return t
    transformString True t =
      case B64.decode $ encodeUtf8 $ strip $ replaceUrlChars $ decodeUtf8 t of
        Left errMsg -> panic $ pack errMsg
        Right bs    -> return bs
    setSecret bs = conf {configJwtSecret = Just bs}
    --
    -- replace: Replace every occurrence of one substring with another
    replaceUrlChars =
      replace "_" "/" . replace "-" "+" . replace "." "="

{-
  Load database uri from a separate file if `db-uri` is a filepath.
-}
loadDbUriFile :: AppConfig -> IO AppConfig
loadDbUriFile conf = extractDbUri mDbUri
  where
    mDbUri = configDbUri conf
    extractDbUri :: Text -> IO AppConfig
    extractDbUri dbUri =
      fmap setDbUri $
      case stripPrefix "@" dbUri of
        Nothing       -> return dbUri
        Just filename -> strip <$> readFile (toS filename)
    setDbUri dbUri = conf {configDbUri = dbUri}
