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
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module PostgREST.Config
  ( prettyVersion
  , docsVersion
  , LogLevel(..)
  , CLI (..)
  , Command (..)
  , AppConfig (..)
  , configDbPoolTimeout'
  , dumpAppConfig
  , Environment
  , readCLIShowHelp
  , readEnvironment
  , readAppConfig
  , readDbUriFile
  , readSecretFile
  , parseSecret
  ) where

import qualified Crypto.JOSE.Types      as JOSE
import qualified Crypto.JWT             as JWT
import qualified Data.Aeson             as JSON
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BS
import qualified Data.Configurator      as C
import qualified Data.Map.Strict        as M

import qualified GHC.Show (show)

import Control.Lens            (preview)
import Control.Monad           (fail)
import Crypto.JWT              (JWK, JWKSet, StringOrURI, stringOrUri)
import Data.Aeson              (encode, toJSON)
import Data.Either.Combinators (mapLeft)
import Data.List               (lookup)
import Data.List.NonEmpty      (fromList, toList)
import Data.Maybe              (fromJust)
import Data.Scientific         (floatingOrInteger)
import Data.Text               (dropEnd, dropWhileEnd, filter,
                                intercalate, pack, replace, splitOn,
                                strip, stripPrefix, take, toLower,
                                toTitle, unpack)
import Data.Version            (versionBranch)
import Development.GitRev      (gitHash)
import Numeric                 (readOct, showOct)
import Paths_postgrest         (version)
import System.Environment      (getEnvironment)
import System.Posix.Types      (FileMode)

import Control.Applicative
import Data.Monoid
import Options.Applicative hiding (str)
import Text.Heredoc        (str)

import PostgREST.Parsers          (pRoleClaimKey)
import PostgREST.Private.ProxyUri (isMalformedProxyUri)
import PostgREST.Types            (JSPath, JSPathExp (..))
import Protolude                  hiding (concat, filter, hPutStrLn,
                                   intercalate, null, replace, take,
                                   toList, toLower, toS, toTitle,
                                   (<>))
import Protolude.Conv             (toS)

-- | Command line interface options
data CLI = CLI
  { cliCommand :: Command
  , cliPath    :: Maybe FilePath }

data Command
  = CmdRun
  | CmdDumpConfig
  | CmdDumpSchema

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
  , configDbConfig              :: Bool
  , configDbTxAllowOverride     :: Bool
  , configDbTxRollbackAll       :: Bool
  , configDbUri                 :: Text
  , configJWKS                  :: Maybe JWKSet
  , configJwtAudience           :: Maybe StringOrURI
  , configJwtRoleClaimKey       :: JSPath
  , configJwtSecret             :: Maybe B.ByteString
  , configJwtSecretIsBase64     :: Bool
  , configLogLevel              :: LogLevel
  , configOpenApiServerProxyUri :: Maybe Text
  , configRawMediaTypes         :: [B.ByteString]
  , configServerHost            :: Text
  , configServerPort            :: Int
  , configServerUnixSocket      :: Maybe FilePath
  , configServerUnixSocketMode  :: FileMode
  }

configDbPoolTimeout' :: (Fractional a) => AppConfig -> a
configDbPoolTimeout' =
  fromRational . toRational . configDbPoolTimeout

data LogLevel = LogCrit | LogError | LogWarn | LogInfo

instance Show LogLevel where
  show LogCrit  = "crit"
  show LogError = "error"
  show LogWarn  = "warn"
  show LogInfo  = "info"


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
readCLIShowHelp :: Environment -> IO CLI
readCLIShowHelp env = customExecParser parserPrefs opts
  where
    parserPrefs = prefs $ showHelpOnError <> showHelpOnEmpty

    opts = info (helper <*> exampleParser <*> cliParser) $
             fullDesc
             <> progDesc (
                 "PostgREST "
                 <> toS prettyVersion
                 <> " / create a REST API to an existing Postgres database"
               )
             <> footer "To run PostgREST, please pass the FILENAME argument or set PGRST_ environment variables."

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
      optionalWithEnvironment (strArgument (
        metavar "FILENAME" <>
        help "Path to configuration file (optional with PGRST_ environment variables)"
      ))

    optionalWithEnvironment :: Alternative f => f a -> f (Maybe a)
    optionalWithEnvironment v
      | M.null env = Just <$> v
      | otherwise  = optional v

    exampleParser :: Parser (a -> a)
    exampleParser =
      infoOption example (
        long "example" <>
        short 'e' <>
        help "Show an example configuration file"
      )

    example =
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
          |## Enable in-database configuration
          |db-config = true
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
      ,("db-config",                 q . toLower . show . configDbConfig)
      ,("db-tx-end",                 q . showTxEnd)
      ,("db-uri",                    q . configDbUri)
      ,("jwt-aud",                       toS . encode . maybe "" toJSON . configJwtAudience)
      ,("jwt-role-claim-key",        q . intercalate mempty . fmap show . configJwtRoleClaimKey)
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
    showSocketMode c = showOct (configServerUnixSocketMode c) mempty

-- This class is needed for the polymorphism of overrideFromDbOrEnvironment
-- because C.required and C.optional have different signatures
class JustIfMaybe a b where
  justIfMaybe :: a -> b

instance JustIfMaybe a a where
  justIfMaybe a = a

instance JustIfMaybe a (Maybe a) where
  justIfMaybe a = Just a

-- | Reads and parses the config and overrides its parameters from env vars, files or db settings.
readAppConfig :: [(Text, Text)] -> Environment -> Maybe FilePath -> Maybe Text -> Maybe B.ByteString -> IO (Either Text AppConfig)
readAppConfig dbSettings env optPath dbUriFile secretFile = do
  -- Now read the actual config file
  conf <- case optPath of
    -- Both C.ParseError and IOError are shown here
    Just cfgPath -> mapLeft show <$> (try $ C.load cfgPath :: IO (Either SomeException C.Config))
    -- if no filename provided, start with an empty map to read config from environment
    Nothing -> return $ Right M.empty

  pure $ mapLeft ("Error in config: " <>) $ C.runParser parseConfig =<< conf

  where
    parseConfig =
      let pB64 = fromMaybe False <$> optWithAlias (optBool "jwt-secret-is-base64")
                                                  (optBool "secret-is-base64")
          pSec = parseJwtSecret "jwt-secret" =<< pB64
      in
      AppConfig
        <$> parseAppSettings "app.settings"
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
        <*> (fromMaybe True <$> optBool "db-prepared-statements")
        <*> optWithAlias (optString "db-root-spec")
                         (optString "root-spec")
        <*> (fromList . splitOnCommas <$> reqWithAlias (optValue "db-schemas")
                                                       (optValue "db-schema")
                                                       "missing key: either db-schemas or db-schema must be set")
        <*> (fromMaybe True <$> optBool "db-config")
        <*> parseTxEnd "db-tx-end" snd
        <*> parseTxEnd "db-tx-end" fst
        <*> parseDbUri "db-uri"
        <*> (fmap parseSecret <$> pSec)
        <*> parseJwtAudience "jwt-aud"
        <*> parseRoleClaimKey "jwt-role-claim-key" "role-claim-key"
        <*> pSec
        <*> pB64
        <*> parseLogLevel "log-level"
        <*> parseOpenAPIServerProxyURI "openapi-server-proxy-uri"
        <*> (maybe [] (fmap encodeUtf8 . splitOnCommas) <$> optValue "raw-media-types")
        <*> (fromMaybe "!4" <$> optString "server-host")
        <*> (fromMaybe 3000 <$> optInt "server-port")
        <*> (fmap unpack <$> optString "server-unix-socket")
        <*> parseSocketFileMode "server-unix-socket-mode"

    parseDbUri :: C.Key -> C.Parser C.Config Text
    parseDbUri k = flip fromMaybe dbUriFile <$> reqString k

    parseJwtSecret :: C.Key -> Bool -> C.Parser C.Config (Maybe B.ByteString)
    parseJwtSecret k isB64 = optString k >>= \case
      Nothing -> pure Nothing
      Just sec  ->
        let secStr = encodeUtf8 sec
            secFile = fromMaybe secStr secretFile
            -- replace because the JWT is actually base64url encoded which must be turned into just base64 before decoding.
            replaceUrlChars = replace "_" "/" . replace "-" "+" . replace "." "="
            willBeFile = isPrefixOf "@" (toS secStr) && isNothing secretFile
        in
        if isB64 && not willBeFile -- don't decode in bas64 if the secret will be a file or it will err. The secFile will be filled with the file contents in a later stage.
          then case B64.decode $ encodeUtf8 $ strip $ replaceUrlChars $ decodeUtf8 secFile of
            Left errMsg -> fail errMsg
            Right bs    -> pure $ Just bs
          else pure $ Just secFile

    parseAppSettings :: C.Key -> C.Parser C.Config [(Text, Text)]
    parseAppSettings key = addFromEnv . fmap (fmap coerceText) <$> C.subassocs key C.value
      where
        addFromEnv f = M.toList $ M.union fromEnv $ M.fromList f
        fromEnv = M.mapKeys fromJust $ M.filterWithKey (\k _ -> isJust k) $ M.mapKeys normalize env
        normalize k = ("app.settings." <>) <$> stripPrefix "PGRST_APP_SETTINGS_" (toS k)

    parseSocketFileMode :: C.Key -> C.Parser C.Config FileMode
    parseSocketFileMode k =
      optString k >>= \case
        Nothing -> pure 432 -- return default 660 mode if no value was provided
        Just fileModeText ->
          case (readOct . unpack) fileModeText of
            []              ->
              fail "Invalid server-unix-socket-mode: not an octal"
            (fileMode, _):_ ->
              if fileMode < 384 || fileMode > 511
                then fail "Invalid server-unix-socket-mode: needs to be between 600 and 777"
                else pure fileMode

    parseOpenAPIServerProxyURI :: C.Key -> C.Parser C.Config (Maybe Text)
    parseOpenAPIServerProxyURI k =
      optString k >>= \case
        Nothing                            -> pure Nothing
        Just val | isMalformedProxyUri val -> fail "Malformed proxy uri, a correct example: https://example.com:8443/basePath"
                 | otherwise               -> pure $ Just val

    parseJwtAudience :: C.Key -> C.Parser C.Config (Maybe StringOrURI)
    parseJwtAudience k =
      optString k >>= \case
        Nothing -> pure Nothing -- no audience in config file
        Just aud -> case preview stringOrUri (unpack aud) of
          Nothing -> fail "Invalid Jwt audience. Check your configuration."
          aud' -> pure aud'

    parseLogLevel :: C.Key -> C.Parser C.Config LogLevel
    parseLogLevel k =
      optString k >>= \case
        Nothing      -> pure LogError
        Just "crit"  -> pure LogCrit
        Just "error" -> pure LogError
        Just "warn"  -> pure LogWarn
        Just "info"  -> pure LogInfo
        Just _       -> fail "Invalid logging level. Check your configuration."

    parseTxEnd :: C.Key -> ((Bool, Bool) -> Bool) -> C.Parser C.Config Bool
    parseTxEnd k f =
      optString k >>= \case
        --                                          RollbackAll AllowOverride
        Nothing                        -> pure $ f (False,      False)
        Just "commit"                  -> pure $ f (False,      False)
        Just "commit-allow-override"   -> pure $ f (False,      True)
        Just "rollback"                -> pure $ f (True,       False)
        Just "rollback-allow-override" -> pure $ f (True,       True)
        Just _                         -> fail "Invalid transaction termination. Check your configuration."

    parseRoleClaimKey :: C.Key -> C.Key -> C.Parser C.Config JSPath
    parseRoleClaimKey k al =
      optWithAlias (optString k) (optString al) >>= \case
        Nothing  -> pure [JSPKey "role"]
        Just rck -> either (fail . show) pure $ pRoleClaimKey rck

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
    reqString k = overrideFromDbOrEnvironment C.required k coerceText

    optString :: C.Key -> C.Parser C.Config (Maybe Text)
    optString k = mfilter (/= "") <$> overrideFromDbOrEnvironment C.optional k coerceText

    optValue :: C.Key -> C.Parser C.Config (Maybe C.Value)
    optValue k = overrideFromDbOrEnvironment C.optional k identity

    optInt :: (Read i, Integral i) => C.Key -> C.Parser C.Config (Maybe i)
    optInt k = join <$> overrideFromDbOrEnvironment C.optional k coerceInt

    optBool :: C.Key -> C.Parser C.Config (Maybe Bool)
    optBool k = join <$> overrideFromDbOrEnvironment C.optional k coerceBool

    overrideFromDbOrEnvironment :: JustIfMaybe a b =>
                               (C.Key -> C.Parser C.Value a -> C.Parser C.Config b) ->
                               C.Key -> (C.Value -> a) -> C.Parser C.Config b
    overrideFromDbOrEnvironment necessity key coercion =
      case reloadableDbSetting <|> M.lookup envVarName env of
        Just dbOrEnvVal -> pure $ justIfMaybe $ coercion $ C.String dbOrEnvVal
        Nothing  -> necessity key (coercion <$> C.value)
      where
        dashToUnderscore '-' = '_'
        dashToUnderscore c   = c
        envVarName = "PGRST_" <> (toUpper . dashToUnderscore <$> toS key)
        reloadableDbSetting =
          let dbSettingName = pack $ dashToUnderscore <$> toS key in
          if dbSettingName `notElem` [
            "server_host", "server_port", "server_unix_socket", "server_unix_socket_mode", "log_level",
            "db_anon_role", "db_uri", "db_channel_enabled", "db_channel", "db_pool", "db_pool_timeout", "db_config"]
          then lookup dbSettingName dbSettings
          else Nothing

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

    splitOnCommas :: C.Value -> [Text]
    splitOnCommas (C.String s) = strip <$> splitOn "," s
    splitOnCommas _            = []

{-|
  Parse `jwt-secret` configuration option and turn into a JWKSet.

  There are three ways to specify `jwt-secret`: text secret, JSON Web Key
  (JWK), or JSON Web Key Set (JWKS). The first two are converted into a JWKSet
  with one key and the last is converted as is.
-}
parseSecret :: ByteString -> JWKSet
parseSecret bytes =
  fromMaybe (maybe secret (\jwk' -> JWT.JWKSet [jwk']) maybeJWK)
    maybeJWKSet
 where
  maybeJWKSet = JSON.decode (toS bytes) :: Maybe JWKSet
  maybeJWK = JSON.decode (toS bytes) :: Maybe JWK
  secret = JWT.JWKSet [JWT.fromKeyMaterial keyMaterial]
  keyMaterial = JWT.OctKeyMaterial . JWT.OctKeyParameters $ JOSE.Base64Octets bytes

type Environment = M.Map [Char] Text

readEnvironment :: IO Environment
readEnvironment = getEnvironment <&> pgrst
  where
    pgrst env = M.filterWithKey (\k _ -> "PGRST_" `isPrefixOf` k) $ M.map pack $ M.fromList env

-- | Read the JWT secret from a file if configJwtSecret is actually a filepath(has @ as its prefix).
-- | To check if the JWT secret is provided is in fact a file path, it must be decoded as 'Text' to be processed.
readSecretFile :: Maybe B.ByteString -> IO (Maybe B.ByteString)
readSecretFile mSecret =
  case (stripPrefix "@" . decodeUtf8) =<< mSecret of
    Nothing       -> return Nothing
    Just filename -> Just . chomp <$> BS.readFile (toS filename)
  where
    chomp bs = fromMaybe bs (BS.stripSuffix "\n" bs)

-- | Read database uri from a separate file if `db-uri` is a filepath.
readDbUriFile :: Text -> IO (Maybe Text)
readDbUriFile dbUri = case stripPrefix "@" dbUri of
  Nothing       -> return Nothing
  Just filename -> Just . strip <$> readFile (toS filename)
