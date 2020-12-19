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
                        , CLI (..)
                        , Command (..)
                        , AppConfig (..)
                        , configDbPoolTimeout'
                        , dumpAppConfig
                        , readCLIShowHelp
                        , readValidateConfig
                        )
       where

import qualified Data.ByteString              as B
import qualified Data.ByteString.Base64       as B64
import qualified Data.ByteString.Char8        as BS
import qualified Data.Configurator            as C
import qualified Text.PrettyPrint.ANSI.Leijen as L

import Control.Lens            (preview)
import Control.Monad           (fail)
import Crypto.JWT              (JWKSet, StringOrURI, stringOrUri)
import Data.Aeson              (encode, toJSON)
import Data.Either.Combinators (fromRight', whenLeft)
import Data.List.NonEmpty      (fromList, toList)
import Data.Scientific         (floatingOrInteger)
import Data.Text               (dropEnd, dropWhileEnd, filter,
                                intercalate, pack, replace, splitOn,
                                strip, stripPrefix, take, toLower,
                                toTitle, unpack)
import Data.Text.IO            (hPutStrLn)
import Data.Version            (versionBranch)
import Development.GitRev      (gitHash)
import Numeric                 (readOct, showOct)
import Paths_postgrest         (version)
import System.IO.Error         (IOError)
import System.Posix.Types      (FileMode)

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
import Protolude                  hiding (concat, filter, hPutStrLn,
                                   intercalate, null, replace, take,
                                   toList, toLower, toS, toTitle,
                                   (<>))
import Protolude.Conv             (toS)

-- | Command line interface options
data CLI = CLI
  { cliCommand :: Command
  , cliPath    :: FilePath }

data Command
  = CmdRun
  | CmdDumpConfig
  | CmdDumpSchema
  deriving (Eq)

-- | Config file settings for the server
data AppConfig = AppConfig {
    configAppSettings           :: [(Text, Text)]
  , configDbAnonRole            :: Text
  , configDbChannel             :: Text
  , configDbChannelEnabled      :: Bool
  , configDbExtraSearchPath     :: [Text]
  , configDbMaxRows             :: Maybe Integer
  , configDbPoolSize            :: Int
  , configDbPoolTimeout         :: Int
  , configDbPreRequest          :: Maybe Text
  , configDbPreparedStatements  :: Bool
  , configDbRootSpec            :: Maybe Text
  , configDbSchemas             :: NonEmpty Text
  , configDbTxAllowOverride     :: Bool
  , configDbTxRollbackAll       :: Bool
  , configDbUri                 :: Text
  , configJWKS                  :: Maybe JWKSet
  , configJwtAudience           :: Maybe StringOrURI
  , configJwtRoleClaimKey       :: Either Text JSPath
  , configJwtSecret             :: Maybe B.ByteString
  , configJwtSecretIsBase64     :: Bool
  , configLogLevel              :: LogLevel
  , configOpenApiServerProxyUri :: Maybe Text
  , configRawMediaTypes         :: [B.ByteString]
  , configServerHost            :: Text
  , configServerPort            :: Int
  , configServerUnixSocket      :: Maybe FilePath
  , configServerUnixSocketMode  :: Either Text FileMode
  }
  deriving (Show)

configDbPoolTimeout' :: (Fractional a) => AppConfig -> a
configDbPoolTimeout' =
  fromRational . toRational . configDbPoolTimeout

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

-- | Read command line interface options. Also prints help.
readCLIShowHelp :: IO CLI
readCLIShowHelp = customExecParser parserPrefs opts
  where
    parserPrefs = prefs showHelpOnError

    opts = info (helper <*> cliParser) $
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

    cliParser :: Parser CLI
    cliParser = CLI <$>
      (
        flag CmdRun CmdDumpConfig (
          long "dump-config" <>
          help "Dump loaded configuration and exit"
        )
        <|>
        flag CmdRun CmdDumpSchema (
          long "dump-schema" <>
          help "Dump loaded schema as JSON and exit (for debugging, output structure is unstable)"
        )
      )
      <*>
      strArgument (
        metavar "FILENAME" <>
        help "Path to configuration file"
      )

    exampleCfg :: Doc
    exampleCfg = vsep . map (text . toS) . lines $
      [str|### REQUIRED:
          |db-uri = "postgres://user:pass@localhost:5432/dbname"
          |db-schema = "public"
          |db-anon-role = "postgres"
          |
          |### OPTIONAL:
          |## number of open connections in the pool
          |db-pool = 10
          |
          |## Time to live, in seconds, for an idle database pool connection.
          |db-pool-timeout = 10
          |
          |## extra schemas to add to the search_path of every request
          |db-extra-search-path = "public"
          |
          |## limit rows in response
          |# db-max-rows = 1000
          |
          |## stored proc to exec immediately after auth
          |# db-pre-request = "stored_proc_name"
          |
          |## stored proc that overrides the root "/" spec
          |## it must be inside the db-schema
          |# db-root-spec = "stored_proc_name"
          |
          |## Notification channel for reloading the schema cache
          |db-channel = "pgrst"
          |
          |## Enable or disable the notification channel
          |db-channel-enabled = false
          |
          |## how to terminate database transactions
          |## possible values are:
          |## commit (default)
          |##   transaction is always committed, this can not be overriden
          |## commit-allow-override
          |##   transaction is committed, but can be overriden with Prefer tx=rollback header
          |## rollback
          |##   transaction is always rolled back, this can not be overriden
          |## rollback-allow-override
          |##   transaction is rolled back, but can be overriden with Prefer tx=commit header
          |db-tx-end = "commit"
          |
          |## enable or disable prepared statements. disabling is only necessary when behind a connection pooler.
          |## when disabled, statements will be parametrized but won't be prepared.
          |db-prepared-statements = true
          |
          |server-host = "!4"
          |server-port = 3000
          |
          |## unix socket location
          |## if specified it takes precedence over server-port
          |# server-unix-socket = "/tmp/pgrst.sock"
          |
          |## unix socket file mode
          |## when none is provided, 660 is applied by default
          |# server-unix-socket-mode = "660"
          |
          |## base url for swagger output
          |openapi-server-proxy-uri = ""
          |
          |## choose a secret, JSON Web Key (or set) to enable JWT auth
          |## (use "@filename" to load from separate file)
          |# jwt-secret = "secret_with_at_least_32_characters"
          |# jwt-aud = "your_audience_claim"
          |jwt-secret-is-base64 = false
          |
          |## jspath to the role claim key
          |jwt-role-claim-key = ".role"
          |
          |## content types to produce raw output
          |# raw-media-types="image/png, image/jpg"
          |
          |## logging level, the admitted values are: crit, error, warn and info.
          |log-level = "error"
          |]

-- | Dump the config
dumpAppConfig :: AppConfig -> Text
dumpAppConfig conf =
  unlines $ (\(k, v) -> k <> " = " <> v) <$>
    pgrstSettings ++ appSettings
  where
    -- apply conf to all pgrst settings
    pgrstSettings = (\(k, v) -> (k, v conf)) <$>
      [("db-anon-role",              q . configDbAnonRole)
      ,("db-channel",                q . configDbChannel)
      ,("db-channel-enabled",            toLower . show . configDbChannelEnabled)
      ,("db-extra-search-path",      q . intercalate "," . configDbExtraSearchPath)
      ,("db-max-rows",                   maybe "\"\"" show . configDbMaxRows)
      ,("db-pool",                       show . configDbPoolSize)
      ,("db-pool-timeout",               show . configDbPoolTimeout)
      ,("db-pre-request",            q . fromMaybe mempty . configDbPreRequest)
      ,("db-prepared-statements",        toLower . show . configDbPreparedStatements)
      ,("db-root-spec",              q . fromMaybe mempty . configDbRootSpec)
      ,("db-schemas",                q . intercalate "," . toList . configDbSchemas)
      ,("db-tx-end",                 q . showTxEnd)
      ,("db-uri",                    q . configDbUri)
      ,("jwt-aud",                       toS . encode . maybe "" toJSON . configJwtAudience)
      ,("jwt-role-claim-key",        q . intercalate mempty . fmap show . fromRight' . configJwtRoleClaimKey)
      ,("jwt-secret",                q . toS . showJwtSecret)
      ,("jwt-secret-is-base64",          toLower . show . configJwtSecretIsBase64)
      ,("log-level",                 q . show . configLogLevel)
      ,("openapi-server-proxy-uri",  q . fromMaybe mempty . configOpenApiServerProxyUri)
      ,("raw-media-types",           q . toS . B.intercalate "," . configRawMediaTypes)
      ,("server-host",               q . configServerHost)
      ,("server-port",                   show . configServerPort)
      ,("server-unix-socket",        q . maybe mempty pack . configServerUnixSocket)
      ,("server-unix-socket-mode",   q . pack . showSocketMode)
      ]

    -- quote all app.settings
    appSettings = second q <$> configAppSettings conf

    -- quote strings and replace " with \"
    q s = "\"" <> replace "\"" "\\\"" s <> "\""

    showTxEnd c = case (configDbTxRollbackAll c, configDbTxAllowOverride c) of
      ( False, False ) -> "commit"
      ( False, True  ) -> "commit-allow-override"
      ( True , False ) -> "rollback"
      ( True , True  ) -> "rollback-allow-override"
    showJwtSecret c
      | configJwtSecretIsBase64 c = B64.encode secret
      | otherwise                 = toS secret
      where
        secret = fromMaybe mempty $ configJwtSecret c
    showSocketMode c = showOct (fromRight' $ configServerUnixSocketMode c) ""

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
        <$> (fmap (fmap coerceText) <$> C.subassocs "app.settings" C.value)
        <*> reqString "db-anon-role"
        <*> (fromMaybe "pgrst" <$> optString "db-channel")
        <*> (fromMaybe False <$> optBool "db-channel-enabled")
        <*> (maybe ["public"] splitOnCommas <$> optValue "db-extra-search-path")
        <*> optWithAlias (optInt "db-max-rows")
                         (optInt "max-rows")
        <*> (fromMaybe 10 <$> optInt "db-pool")
        <*> (fromMaybe 10 <$> optInt "db-pool-timeout")
        <*> optWithAlias (optString "db-pre-request")
                         (optString "pre-request")
        <*> (fromMaybe True <$>  optBool "db-prepared-statements")
        <*> optWithAlias (optString "db-root-spec")
                         (optString "root-spec")
        <*> (fromList . splitOnCommas <$> reqWithAlias (optValue "db-schemas")
                                                       (optValue "db-schema")
                                                       "missing key: either db-schemas or db-schema must be set")
        <*> parseTxEnd "db-tx-end" snd
        <*> parseTxEnd "db-tx-end" fst
        <*> reqString "db-uri"
        <*> pure Nothing
        <*> parseJwtAudience "jwt-aud"
        <*> (maybe (Right [JSPKey "role"]) parseRoleClaimKey <$> optWithAlias (optValue "jwt-role-claim-key")
                                                                              (optValue "role-claim-key"))
        <*> (fmap encodeUtf8 <$> optString "jwt-secret")
        <*> (fromMaybe False <$> optWithAlias (optBool "jwt-secret-is-base64")
                                              (optBool "secret-is-base64"))
        <*> parseLogLevel "log-level"
        <*> optString "openapi-server-proxy-uri"
        <*> (maybe [] (fmap encodeUtf8 . splitOnCommas) <$> optValue "raw-media-types")
        <*> (fromMaybe "!4" <$> optString "server-host")
        <*> (fromMaybe 3000 <$> optInt "server-port")
        <*> (fmap unpack <$> optString "server-unix-socket")
        <*> parseSocketFileMode "server-unix-socket-mode"

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

    parseTxEnd :: C.Key -> ((Bool, Bool) -> Bool) -> C.Parser C.Config Bool
    parseTxEnd k f =
      C.optional k C.string >>= \case
        --                                          RollbackAll AllowOverride
        Nothing                        -> pure $ f (False,      False)
        Just ""                        -> pure $ f (False,      False)
        Just "commit"                  -> pure $ f (False,      False)
        Just "commit-allow-override"   -> pure $ f (False,      True)
        Just "rollback"                -> pure $ f (True,       False)
        Just "rollback-allow-override" -> pure $ f (True,       True)
        Just _                         -> fail "Invalid transaction termination. Check your configuration."

    reqWithAlias :: C.Parser C.Config (Maybe a) -> C.Parser C.Config (Maybe a) -> [Char] -> C.Parser C.Config a
    reqWithAlias orig alias err =
      orig >>= \case
        Just v  -> pure v
        Nothing ->
          alias >>= \case
            Just v  -> pure v
            Nothing -> fail err

    optWithAlias :: C.Parser C.Config (Maybe a) -> C.Parser C.Config (Maybe a) -> C.Parser C.Config (Maybe a)
    optWithAlias orig alias =
      orig >>= \case
        Just v  -> pure $ Just v
        Nothing -> alias

    reqString :: C.Key -> C.Parser C.Config Text
    reqString k = C.required k C.string

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
    coerceBool (C.String s) =
      -- parse all kinds of text: True, true, TRUE, "true", ...
      case readMaybe . toS $ toTitle $ filter isAlpha $ toS s of
        Just b  -> Just b
        -- numeric instead?
        Nothing -> (> 0) <$> (readMaybe $ toS s :: Maybe Integer)
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
  when (isMalformedProxyUri $ toS <$> configOpenApiServerProxyUri conf) $
    panic
      "Malformed proxy uri, a correct example: https://example.com:8443/basePath"
  -- Checks that the provided jspath is valid
  whenLeft (configJwtRoleClaimKey conf) panic
  -- Check the file mode is valid
  whenLeft (configServerUnixSocketMode conf) panic
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
