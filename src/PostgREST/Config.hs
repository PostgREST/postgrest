{-|
Module      : PostgREST.Config
Description : Manages PostgREST configuration type and parser.

-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module PostgREST.Config
  ( AppConfig (..)
  , Environment
  , JSPath
  , JSPathExp(..)
  , FilterExp(..)
  , LogLevel(..)
  , LogQuery(..)
  , OpenAPIMode(..)
  , Proxy(..)
  , toText
  , isMalformedProxyUri
  , readAppConfig
  , readPGRSTEnvironment
  , toURI
  , parseSecret
  , addFallbackAppName
  , addTargetSessionAttrs
  ) where

import qualified Data.Aeson             as JSON
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.CaseInsensitive   as CI
import qualified Data.Configurator      as C
import qualified Data.Map.Strict        as M
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Jose.Jwa               as JWT
import qualified Jose.Jwk               as JWT

import Control.Monad           (fail)
import Data.Either.Combinators (mapLeft)
import Data.List               (lookup)
import Data.List.NonEmpty      (fromList, toList)
import Data.Maybe              (fromJust)
import Data.Scientific         (floatingOrInteger)
import Jose.Jwk                (Jwk, JwkSet)
import Network.URI             (escapeURIString,
                                isUnescapedInURIComponent)
import Numeric                 (readOct, showOct)
import System.Environment      (getEnvironment)
import System.Posix.Types      (FileMode)

import PostgREST.Config.Database         (RoleIsolationLvl,
                                          RoleSettings)
import PostgREST.Config.JSPath           (FilterExp (..), JSPath,
                                          JSPathExp (..), dumpJSPath,
                                          pRoleClaimKey)
import PostgREST.Config.Proxy            (Proxy (..),
                                          isMalformedProxyUri, toURI)
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier, dumpQi,
                                          toQi)

import Protolude hiding (Proxy, toList)


data AppConfig = AppConfig
  { configAppSettings              :: [(Text, Text)]
  , configDbAggregates             :: Bool
  , configDbAnonRole               :: Maybe BS.ByteString
  , configDbChannel                :: Text
  , configDbChannelEnabled         :: Bool
  , configDbExtraSearchPath        :: [Text]
  , configDbHoistedTxSettings      :: [Text]
  , configDbMaxRows                :: Maybe Integer
  , configDbPlanEnabled            :: Bool
  , configDbPoolSize               :: Int
  , configDbPoolAcquisitionTimeout :: Int
  , configDbPoolMaxLifetime        :: Int
  , configDbPoolMaxIdletime        :: Int
  , configDbPoolAutomaticRecovery  :: Bool
  , configDbPreRequest             :: Maybe QualifiedIdentifier
  , configDbPreparedStatements     :: Bool
  , configDbRootSpec               :: Maybe QualifiedIdentifier
  , configDbSchemas                :: NonEmpty Text
  , configDbConfig                 :: Bool
  , configDbPreConfig              :: Maybe QualifiedIdentifier
  , configDbTxAllowOverride        :: Bool
  , configDbTxRollbackAll          :: Bool
  , configDbUri                    :: Text
  , configFilePath                 :: Maybe FilePath
  , configJWKS                     :: Maybe JwkSet
  , configJwtAudience              :: Maybe Text
  , configJwtRoleClaimKey          :: JSPath
  , configJwtSecret                :: Maybe BS.ByteString
  , configJwtSecretIsBase64        :: Bool
  , configJwtCacheMaxLifetime      :: Int
  , configLogLevel                 :: LogLevel
  , configLogQuery                 :: LogQuery
  , configOpenApiMode              :: OpenAPIMode
  , configOpenApiSecurityActive    :: Bool
  , configOpenApiServerProxyUri    :: Maybe Text
  , configServerCorsAllowedOrigins :: Maybe [Text]
  , configServerHost               :: Text
  , configServerPort               :: Int
  , configServerTraceHeader        :: Maybe (CI.CI BS.ByteString)
  , configServerTimingEnabled      :: Bool
  , configServerUnixSocket         :: Maybe FilePath
  , configServerUnixSocketMode     :: FileMode
  , configAdminServerHost          :: Text
  , configAdminServerPort          :: Maybe Int
  , configRoleSettings             :: RoleSettings
  , configRoleIsoLvl               :: RoleIsolationLvl
  , configInternalSCSleep          :: Maybe Int32
  }

data LogLevel = LogCrit | LogError | LogWarn | LogInfo | LogDebug
  deriving (Eq, Ord)

dumpLogLevel :: LogLevel -> Text
dumpLogLevel = \case
  LogCrit  -> "crit"
  LogError -> "error"
  LogWarn  -> "warn"
  LogInfo  -> "info"
  LogDebug -> "debug"

data LogQuery = LogQueryMain | LogQueryDisabled
  deriving (Eq)

dumpLogQuery :: LogQuery -> Text
dumpLogQuery = \case
  LogQueryMain     -> "main-query"
  LogQueryDisabled -> "disabled"

data OpenAPIMode = OAFollowPriv | OAIgnorePriv | OADisabled
  deriving Eq

dumpOpenApiMode :: OpenAPIMode -> Text
dumpOpenApiMode = \case
  OAFollowPriv -> "follow-privileges"
  OAIgnorePriv -> "ignore-privileges"
  OADisabled   -> "disabled"

-- | Dump the config
toText :: AppConfig -> Text
toText conf =
  unlines $ (\(k, v) -> k <> " = " <> v) <$> pgrstSettings ++ appSettings
  where
    -- apply conf to all pgrst settings
    pgrstSettings = (\(k, v) -> (k, v conf)) <$>
      [("db-aggregates-enabled",         T.toLower . show . configDbAggregates)
      ,("db-anon-role",              q . T.decodeUtf8 . fromMaybe "" . configDbAnonRole)
      ,("db-channel",                q . configDbChannel)
      ,("db-channel-enabled",            T.toLower . show . configDbChannelEnabled)
      ,("db-extra-search-path",      q . T.intercalate "," . configDbExtraSearchPath)
      ,("db-hoisted-tx-settings",    q . T.intercalate "," . configDbHoistedTxSettings)
      ,("db-max-rows",                   maybe "\"\"" show . configDbMaxRows)
      ,("db-plan-enabled",               T.toLower . show . configDbPlanEnabled)
      ,("db-pool",                       show . configDbPoolSize)
      ,("db-pool-acquisition-timeout",   show . configDbPoolAcquisitionTimeout)
      ,("db-pool-max-lifetime",          show . configDbPoolMaxLifetime)
      ,("db-pool-max-idletime",          show . configDbPoolMaxIdletime)
      ,("db-pool-automatic-recovery",    T.toLower . show . configDbPoolAutomaticRecovery)
      ,("db-pre-request",            q . maybe mempty dumpQi . configDbPreRequest)
      ,("db-prepared-statements",        T.toLower . show . configDbPreparedStatements)
      ,("db-root-spec",              q . maybe mempty dumpQi . configDbRootSpec)
      ,("db-schemas",                q . T.intercalate "," . toList . configDbSchemas)
      ,("db-config",                     T.toLower . show . configDbConfig)
      ,("db-pre-config",             q . maybe mempty dumpQi . configDbPreConfig)
      ,("db-tx-end",                 q . showTxEnd)
      ,("db-uri",                    q . configDbUri)
      ,("jwt-aud",                   q . fromMaybe mempty . configJwtAudience)
      ,("jwt-role-claim-key",        q . T.intercalate mempty . fmap dumpJSPath . configJwtRoleClaimKey)
      ,("jwt-secret",                q . T.decodeUtf8 . showJwtSecret)
      ,("jwt-secret-is-base64",          T.toLower . show . configJwtSecretIsBase64)
      ,("jwt-cache-max-lifetime",                   show . configJwtCacheMaxLifetime)
      ,("log-level",                 q . dumpLogLevel . configLogLevel)
      ,("log-query",                 q . dumpLogQuery . configLogQuery)
      ,("openapi-mode",              q . dumpOpenApiMode . configOpenApiMode)
      ,("openapi-security-active",       T.toLower . show . configOpenApiSecurityActive)
      ,("openapi-server-proxy-uri",  q . fromMaybe mempty . configOpenApiServerProxyUri)
      ,("server-cors-allowed-origins",      q . maybe "" (T.intercalate ",") . configServerCorsAllowedOrigins)
      ,("server-host",               q . configServerHost)
      ,("server-port",                   show . configServerPort)
      ,("server-trace-header",       q . T.decodeUtf8 . maybe mempty CI.original . configServerTraceHeader)
      ,("server-timing-enabled",         T.toLower . show . configServerTimingEnabled)
      ,("server-unix-socket",        q . maybe mempty T.pack . configServerUnixSocket)
      ,("server-unix-socket-mode",   q . T.pack . showSocketMode)
      ,("admin-server-host",         q . configAdminServerHost)
      ,("admin-server-port",             maybe "\"\"" show . configAdminServerPort)
      ]

    -- quote all app.settings
    appSettings = second q <$> configAppSettings conf

    -- quote strings and replace " with \"
    q s = "\"" <> T.replace "\"" "\\\"" s <> "\""

    showTxEnd c = case (configDbTxRollbackAll c, configDbTxAllowOverride c) of
      ( False, False ) -> "commit"
      ( False, True  ) -> "commit-allow-override"
      ( True , False ) -> "rollback"
      ( True , True  ) -> "rollback-allow-override"
    showJwtSecret c
      | configJwtSecretIsBase64 c = B64.encode secret
      | otherwise                 = secret
      where
        secret = fromMaybe mempty $ configJwtSecret c
    showSocketMode c = showOct (configServerUnixSocketMode c) mempty

-- This class is needed for the polymorphism of overrideFromDbOrEnvironment
-- because C.required and C.optional have different signatures
class JustIfMaybe a b where
  justIfMaybe :: a -> b

instance JustIfMaybe a a where
  justIfMaybe = identity

instance JustIfMaybe a (Maybe a) where
  justIfMaybe = Just

-- | Reads and parses the config and overrides its parameters from env vars,
-- files or db settings.
readAppConfig :: [(Text, Text)] -> Maybe FilePath -> Maybe Text -> RoleSettings -> RoleIsolationLvl -> IO (Either Text AppConfig)
readAppConfig dbSettings optPath prevDbUri roleSettings roleIsolationLvl = do
  env <- readPGRSTEnvironment
  -- if no filename provided, start with an empty map to read config from environment
  conf <- maybe (return $ Right M.empty) loadConfig optPath

  case C.runParser (parser optPath env dbSettings roleSettings roleIsolationLvl) =<< mapLeft show conf of
    Left err ->
      return . Left $ "Error in config " <> err
    Right parsedConfig ->
      mapLeft show <$> decodeLoadFiles parsedConfig
  where
    -- Both C.ParseError and IOError are shown here
    loadConfig :: FilePath -> IO (Either SomeException C.Config)
    loadConfig = try . C.load

    decodeLoadFiles :: AppConfig -> IO (Either IOException AppConfig)
    decodeLoadFiles parsedConfig = try $
      decodeJWKS =<<
      decodeSecret =<<
      readSecretFile =<<
      readDbUriFile prevDbUri parsedConfig

parser :: Maybe FilePath -> Environment -> [(Text, Text)] -> RoleSettings -> RoleIsolationLvl -> C.Parser C.Config AppConfig
parser optPath env dbSettings roleSettings roleIsolationLvl =
  AppConfig
    <$> parseAppSettings "app.settings"
    <*> (fromMaybe False <$> optBool "db-aggregates-enabled")
    <*> (fmap encodeUtf8 <$> optString "db-anon-role")
    <*> (fromMaybe "pgrst" <$> optString "db-channel")
    <*> (fromMaybe True <$> optBool "db-channel-enabled")
    <*> (maybe ["public"] splitOnCommas <$> optValue "db-extra-search-path")
    <*> (maybe defaultHoistedAllowList splitOnCommas <$> optValue "db-hoisted-tx-settings")
    <*> optWithAlias (optInt "db-max-rows")
                     (optInt "max-rows")
    <*> (fromMaybe False <$> optBool "db-plan-enabled")
    <*> (fromMaybe 10 <$> optInt "db-pool")
    <*> (fromMaybe 10 <$> optInt "db-pool-acquisition-timeout")
    <*> (fromMaybe 1800 <$> optInt "db-pool-max-lifetime")
    <*> (fromMaybe 30 <$> optWithAlias (optInt "db-pool-timeout")
                                       (optInt "db-pool-max-idletime"))
    <*> (fromMaybe True <$> optBool "db-pool-automatic-recovery")
    <*> (fmap toQi <$> optWithAlias (optString "db-pre-request")
                                    (optString "pre-request"))
    <*> (fromMaybe True <$> optBool "db-prepared-statements")
    <*> (fmap toQi <$> optWithAlias (optString "db-root-spec")
                                    (optString "root-spec"))
    <*> (fromList . maybe ["public"] splitOnCommas <$> optWithAlias (optValue "db-schemas")
                                                                    (optValue "db-schema"))
    <*> (fromMaybe True <$> optBool "db-config")
    <*> (fmap toQi <$> optString "db-pre-config")
    <*> parseTxEnd "db-tx-end" snd
    <*> parseTxEnd "db-tx-end" fst
    <*> (fromMaybe "postgresql://" <$> optString "db-uri")
    <*> pure optPath
    <*> pure Nothing
    <*> optString "jwt-aud"
    <*> parseRoleClaimKey "jwt-role-claim-key" "role-claim-key"
    <*> (fmap encodeUtf8 <$> optString "jwt-secret")
    <*> (fromMaybe False <$> optWithAlias
          (optBool "jwt-secret-is-base64")
          (optBool "secret-is-base64"))
    <*> (fromMaybe 0 <$> optInt "jwt-cache-max-lifetime")
    <*> parseLogLevel "log-level"
    <*> parseLogQuery "log-query"
    <*> parseOpenAPIMode "openapi-mode"
    <*> (fromMaybe False <$> optBool "openapi-security-active")
    <*> parseOpenAPIServerProxyURI "openapi-server-proxy-uri"
    <*> parseCORSAllowedOrigins "server-cors-allowed-origins"
    <*> (defaultServerHost <$> optString "server-host")
    <*> parseServerPort "server-port"
    <*> (fmap (CI.mk . encodeUtf8) <$> optString "server-trace-header")
    <*> (fromMaybe False <$> optBool "server-timing-enabled")
    <*> (fmap T.unpack <$> optString "server-unix-socket")
    <*> parseSocketFileMode "server-unix-socket-mode"
    <*> (defaultServerHost <$> optWithAlias (optString "admin-server-host")
                                            (optString "server-host"))
    <*> parseAdminServerPort "admin-server-port"
    <*> pure roleSettings
    <*> pure roleIsolationLvl
    <*> optInt "internal-schema-cache-sleep"
  where
    parseAppSettings :: C.Key -> C.Parser C.Config [(Text, Text)]
    parseAppSettings key = addFromEnv . fmap (fmap coerceText) <$> C.subassocs key C.value
      where
        addFromEnv f = M.toList $ M.union fromEnv $ M.fromList f
        fromEnv = M.mapKeys fromJust $ M.filterWithKey (\k _ -> isJust k) $ M.mapKeys normalize env
        normalize k = ("app.settings." <>) <$> T.stripPrefix "PGRST_APP_SETTINGS_" (toS k)

    parseServerPort :: C.Key -> C.Parser C.Config Int
    parseServerPort k = fromMaybe 3000 <$> optInt k

    parseAdminServerPort :: C.Key -> C.Parser C.Config (Maybe Int)
    parseAdminServerPort k = do
      serverPort <- parseServerPort "server-port"
      optInt k >>= \case
        Nothing -> pure Nothing
        Just asp | asp == serverPort -> fail "admin-server-port cannot be the same as server-port"
                 | otherwise         -> pure $ Just asp

    parseSocketFileMode :: C.Key -> C.Parser C.Config FileMode
    parseSocketFileMode k =
      optString k >>= \case
        Nothing -> pure 432 -- return default 660 mode if no value was provided
        Just fileModeText ->
          case readOct $ T.unpack fileModeText of
            []              ->
              fail "Invalid server-unix-socket-mode: not an octal"
            (fileMode, _):_ ->
              if fileMode < 384 || fileMode > 511
                then fail "Invalid server-unix-socket-mode: needs to be between 600 and 777"
                else pure fileMode

    parseOpenAPIMode :: C.Key -> C.Parser C.Config OpenAPIMode
    parseOpenAPIMode k =
      optString k >>= \case
        Nothing                  -> pure OAFollowPriv
        Just "follow-privileges" -> pure OAFollowPriv
        Just "ignore-privileges" -> pure OAIgnorePriv
        Just "disabled"          -> pure OADisabled
        Just _                   -> fail "Invalid openapi-mode. Check your configuration."

    parseOpenAPIServerProxyURI :: C.Key -> C.Parser C.Config (Maybe Text)
    parseOpenAPIServerProxyURI k =
      optString k >>= \case
        Nothing                            -> pure Nothing
        Just val | isMalformedProxyUri val -> fail "Malformed proxy uri, a correct example: https://example.com:8443/basePath"
                 | otherwise               -> pure $ Just val

    parseLogLevel :: C.Key -> C.Parser C.Config LogLevel
    parseLogLevel k =
      optString k >>= \case
        Nothing      -> pure LogError
        Just "crit"  -> pure LogCrit
        Just "error" -> pure LogError
        Just "warn"  -> pure LogWarn
        Just "info"  -> pure LogInfo
        Just "debug" -> pure LogDebug
        Just _       -> fail "Invalid logging level. Check your configuration."

    parseLogQuery :: C.Key -> C.Parser C.Config LogQuery
    parseLogQuery k =
      optString k >>= \case
        Nothing           -> pure  LogQueryDisabled
        Just "disabled"   -> pure  LogQueryDisabled
        Just "main-query" -> pure  LogQueryMain
        Just _            -> fail "Invalid SQL logging value. Check your configuration."

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

    parseCORSAllowedOrigins k =
      optString k >>= \case
        Nothing   -> pure Nothing
        Just orig -> pure $ Just (T.strip <$> T.splitOn "," orig)

    optWithAlias :: C.Parser C.Config (Maybe a) -> C.Parser C.Config (Maybe a) -> C.Parser C.Config (Maybe a)
    optWithAlias orig alias =
      orig >>= \case
        Just v  -> pure $ Just v
        Nothing -> alias

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
      case dbConf <|> M.lookup envVarName env of
        Just dbOrEnvVal -> pure $ justIfMaybe $ coercion $ C.String dbOrEnvVal
        Nothing         -> necessity key (coercion <$> C.value)
      where
        dashToUnderscore '-' = '_'
        dashToUnderscore c   = c
        envVarName = "PGRST_" <> (toUpper . dashToUnderscore <$> toS key)
        dbConf = lookup (T.pack $ dashToUnderscore <$> toS key) dbSettings

    coerceText :: C.Value -> Text
    coerceText (C.String s) = s
    coerceText v            = show v

    coerceInt :: (Read i, Integral i) => C.Value -> Maybe i
    coerceInt (C.Number x) = rightToMaybe $ floatingOrInteger x
    coerceInt (C.String x) = readMaybe x
    coerceInt _            = Nothing

    coerceBool :: C.Value -> Maybe Bool
    coerceBool (C.Bool b)   = Just b
    coerceBool (C.String s) =
      -- parse all kinds of text: True, true, TRUE, "true", ...
      case readMaybe $ T.toTitle $ T.filter isAlpha $ toS s of
        Just b  -> Just b
        -- numeric instead?
        Nothing -> (> 0) <$> (readMaybe s :: Maybe Integer)
    coerceBool _            = Nothing

    splitOnCommas :: C.Value -> [Text]
    splitOnCommas (C.String s) = T.strip <$> T.splitOn "," s
    splitOnCommas _            = []

    defaultHoistedAllowList = ["statement_timeout","plan_filter.statement_cost_limit","default_transaction_isolation"]

    defaultServerHost :: Maybe Text -> Text
    defaultServerHost = fromMaybe "!4"

-- | Read the JWT secret from a file if configJwtSecret is actually a
-- filepath(has @ as its prefix). To check if the JWT secret is provided is
-- in fact a file path, it must be decoded as 'Text' to be processed.
readSecretFile :: AppConfig -> IO AppConfig
readSecretFile conf =
  maybe (return conf) readSecret maybeFilename
  where
    maybeFilename = T.stripPrefix "@" . decodeUtf8 =<< configJwtSecret conf
    readSecret filename = do
      jwtSecret <- chomp <$> BS.readFile (toS filename)
      return $ conf { configJwtSecret = Just jwtSecret }
    chomp bs = fromMaybe bs (BS.stripSuffix "\n" bs)

decodeSecret :: AppConfig -> IO AppConfig
decodeSecret conf@AppConfig{..} =
  case (configJwtSecretIsBase64, configJwtSecret) of
    (True, Just secret) ->
      either fail (return . updateSecret) $ decodeB64 secret
    _ -> return conf
  where
    updateSecret bs = conf { configJwtSecret = Just bs }
    decodeB64 = B64.decode . encodeUtf8 . T.strip . replaceUrlChars . decodeUtf8
    replaceUrlChars = T.replace "_" "/" . T.replace "-" "+" . T.replace "." "="

-- | Parse `jwt-secret` configuration option and turn into a JWKS.
--
-- There are three ways to specify `jwt-secret`: text secret, JSON Web Key
-- (JWK), or JSON Web Key Set (JWKS). The first two are converted into a JwkSet
-- with one key and the last is converted as is.
decodeJWKS :: AppConfig -> IO AppConfig
decodeJWKS conf = do
  jwks <- case configJwtSecret conf of
    Just s  -> either fail (pure . Just) $ parseSecret s
    Nothing -> pure Nothing
  return $ conf { configJWKS = jwks }

parseSecret :: ByteString -> Either [Char] JwkSet
parseSecret bytes =
  case maybeJWKSet of
    Just jwk -> Right jwk
    Nothing  -> maybe validateSecret (\jwk' -> Right $ JWT.JwkSet [jwk']) maybeJWK
  where
    maybeJWKSet = JSON.decodeStrict bytes :: Maybe JwkSet
    maybeJWK = JSON.decodeStrict bytes :: Maybe Jwk
    secret = JWT.JwkSet [JWT.SymmetricJwk bytes Nothing (Just JWT.Sig) (Just $ JWT.Signed JWT.HS256)]
    validateSecret
      | BS.length bytes < 32 = Left "The JWT secret must be at least 32 characters long."
      | otherwise = Right secret

-- | Read database uri from a separate file if `db-uri` is a filepath.
readDbUriFile :: Maybe Text -> AppConfig -> IO AppConfig
readDbUriFile maybeDbUri conf =
  case maybeDbUri of
    Just prevDbUri ->
      pure $ conf { configDbUri = prevDbUri }
    Nothing ->
      case T.stripPrefix "@" $ configDbUri conf of
        Nothing -> return conf
        Just filename -> do
          dbUri <- T.strip <$> readFile (toS filename)
          return $ conf { configDbUri = dbUri }

type Environment = M.Map [Char] Text

-- | Read environment variables that start with PGRST_
readPGRSTEnvironment :: IO Environment
readPGRSTEnvironment =
  M.map T.pack . M.fromList . filter (isPrefixOf "PGRST_" . fst) <$> getEnvironment

data PGConnString = PGURI | PGKeyVal

-- Uses same logic as libpq recognized_connection_string
-- https://github.com/postgres/postgres/blob/5eafacd2797dc0b04a0bde25fbf26bf79903e7c2/src/interfaces/libpq/fe-connect.c#L5923-L5936
pgConnString :: Text -> Maybe PGConnString
pgConnString conn | uriDesignator `T.isPrefixOf` conn || shortUriDesignator `T.isPrefixOf` conn = Just PGURI
                  | "=" `T.isInfixOf` conn                                                      = Just PGKeyVal
                  | otherwise                                                                   = Nothing
  where
    uriDesignator = "postgresql://"
    shortUriDesignator = "postgres://"

-- | Adds a `fallback_application_name` value to the connection string. This allows querying the PostgREST version on pg_stat_activity.
--
-- >>> let ver = "11.1.0 (5a04ec7)"::ByteString
-- >>> let strangeVer = "11'1&0@#$%,.:\"[]{}?+^()=asdfqwer"::ByteString
--
-- >>> addFallbackAppName ver "postgres://user:pass@host:5432/postgres"
-- "postgres://user:pass@host:5432/postgres?fallback_application_name=PostgREST%2011.1.0%20%285a04ec7%29"
--
-- >>> addFallbackAppName ver "postgres://user:pass@host:5432/postgres?"
-- "postgres://user:pass@host:5432/postgres?fallback_application_name=PostgREST%2011.1.0%20%285a04ec7%29"
--
-- >>> addFallbackAppName ver "postgres:///postgres?host=server&port=5432"
-- "postgres:///postgres?host=server&port=5432&fallback_application_name=PostgREST%2011.1.0%20%285a04ec7%29"
--
-- >>> addFallbackAppName ver "postgresql://"
-- "postgresql://?fallback_application_name=PostgREST%2011.1.0%20%285a04ec7%29"
--
-- >>> addFallbackAppName strangeVer "postgres:///postgres?host=server&port=5432"
-- "postgres:///postgres?host=server&port=5432&fallback_application_name=PostgREST%2011%271%260%40%23%24%25%2C.%3A%22%5B%5D%7B%7D%3F%2B%5E%28%29%3Dasdfqwer"
--
-- >>> addFallbackAppName ver "postgres://user:invalid_chars[]#@host:5432/postgres"
-- "postgres://user:invalid_chars[]#@host:5432/postgres?fallback_application_name=PostgREST%2011.1.0%20%285a04ec7%29"
--
-- >>> addFallbackAppName ver "host=localhost port=5432 dbname=postgres"
-- "host=localhost port=5432 dbname=postgres fallback_application_name='PostgREST 11.1.0 (5a04ec7)'"
--
-- >>> addFallbackAppName strangeVer "host=localhost port=5432 dbname=postgres"
-- "host=localhost port=5432 dbname=postgres fallback_application_name='PostgREST 11\\'1&0@#$%,.:\"[]{}?+^()=asdfqwer'"
--
-- works with passwords containing `?`
-- >>> addFallbackAppName ver "postgres://admin2:?pass?special?@localhost:5432/postgres"
-- "postgres://admin2:?pass?special?@localhost:5432/postgres?fallback_application_name=PostgREST%2011.1.0%20%285a04ec7%29"
--
-- addFallbackAppName ver "postgresql://?dbname=postgres&host=/run/user/1000/postgrest/postgrest-with-postgresql-16-BuR/socket&user=some_protected_user&password=invalid_pass"
-- "postgresql://?dbname=postgres&host=/run/user/1000/postgrest/postgrest-with-postgresql-16-BuR/socket&user=some_protected_user&password=invalid_pass&fallback_application_name=PostgREST%2011.1.0%20%285a04ec7%29"
--
-- addFallbackAppName ver "postgresql:///postgres?host=/run/user/1000/postgrest/postgrest-with-postgresql-16-BuR/socket&user=some_protected_user&password=invalid_pass"
-- "postgresql:///postgres?host=/run/user/1000/postgrest/postgrest-with-postgresql-16-BuR/socket&user=some_protected_user&password=invalid_pass&fallback_application_name=PostgREST%2011.1.0%20%285a04ec7%29"
addFallbackAppName :: ByteString -> Text -> Text
addFallbackAppName version dbUri = addConnStringOption dbUri "fallback_application_name" pgrstVer
  where
    pgrstVer = "PostgREST " <> T.decodeUtf8 version

-- | Adds `target_session_attrs=read-write` to the connection string. This allows using PostgREST listener when multiple hosts are specified in the connection string.
--
-- >>> addTargetSessionAttrs "postgres:///postgres?host=/dir/0kN/socket_replica_24378,/dir/0kN/socket"
-- "postgres:///postgres?host=/dir/0kN/socket_replica_24378,/dir/0kN/socket&target_session_attrs=read-write"
--
-- >>> addTargetSessionAttrs "postgresql://host1:123,host2:456/somedb"
-- "postgresql://host1:123,host2:456/somedb?target_session_attrs=read-write"
--
-- >>> addTargetSessionAttrs "postgresql://host1:123,host2:456/somedb?fallback_application_name=foo"
-- "postgresql://host1:123,host2:456/somedb?fallback_application_name=foo&target_session_attrs=read-write"
--
-- adds target_session_attrs despite one existing
-- >>> addTargetSessionAttrs "postgresql://host1:123,host2:456/somedb?target_session_attrs=read-only"
-- "postgresql://host1:123,host2:456/somedb?target_session_attrs=read-only&target_session_attrs=read-write"
--
-- >>> addTargetSessionAttrs "host=localhost port=5432 dbname=postgres"
-- "host=localhost port=5432 dbname=postgres target_session_attrs='read-write'"
addTargetSessionAttrs :: Text -> Text
addTargetSessionAttrs dbUri = addConnStringOption dbUri "target_session_attrs" "read-write"

addConnStringOption :: Text -> Text -> Text -> Text
addConnStringOption dbUri key val = dbUri <>
  case pgConnString dbUri of
    Nothing  -> mempty
    Just PGKeyVal -> " " <> keyValFmt
    Just PGURI    -> case lookAtOptions dbUri of
      (_, "")  -> "?" <> uriFmt
      (_, "?") -> uriFmt
      (_, _)   -> "&" <> uriFmt
  where
    uriFmt = key <> "=" <> toS (escapeURIString isUnescapedInURIComponent $ toS val)
    keyValFmt = key <> "=" <> "'" <> T.replace "'" "\\'" val <> "'"
    lookAtOptions x =  T.breakOn "?" . snd $ T.breakOnEnd "@" x -- start from after `@` to not mess passwords that include `?`, see https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING-URIS
