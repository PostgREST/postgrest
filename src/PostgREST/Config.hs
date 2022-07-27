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
  , LogLevel(..)
  , OpenAPIMode(..)
  , Proxy(..)
  , toText
  , isMalformedProxyUri
  , readAppConfig
  , readPGRSTEnvironment
  , toURI
  , parseSecret
  ) where

import qualified Crypto.JOSE.Types      as JOSE
import qualified Crypto.JWT             as JWT
import qualified Data.Aeson             as JSON
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Configurator      as C
import qualified Data.Map.Strict        as M
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

import Control.Lens            (preview)
import Control.Monad           (fail)
import Crypto.JWT              (JWK, JWKSet, StringOrURI, stringOrUri)
import Data.Aeson              (toJSON)
import Data.Either.Combinators (mapLeft)
import Data.List               (lookup)
import Data.List.NonEmpty      (fromList, toList)
import Data.Maybe              (fromJust)
import Data.Scientific         (floatingOrInteger)
import Data.Time.Clock         (NominalDiffTime)
import Numeric                 (readOct, showOct)
import System.Environment      (getEnvironment)
import System.Posix.Types      (FileMode)

import PostgREST.Config.JSPath           (JSPath, JSPathExp (..),
                                          dumpJSPath, pRoleClaimKey)
import PostgREST.Config.Proxy            (Proxy (..),
                                          isMalformedProxyUri, toURI)
import PostgREST.DbStructure.Identifiers (QualifiedIdentifier, dumpQi,
                                          toQi)
import PostgREST.MediaType               (MediaType (..), toMime)

import Protolude hiding (Proxy, toList)


data AppConfig = AppConfig
  { configAppSettings           :: [(Text, Text)]
  , configDbAnonRole            :: Maybe Text
  , configDbChannel             :: Text
  , configDbChannelEnabled      :: Bool
  , configDbExtraSearchPath     :: [Text]
  , configDbMaxRows             :: Maybe Integer
  , configDbPlanEnabled         :: Bool
  , configDbPoolSize            :: Int
  , configDbPoolTimeout         :: NominalDiffTime
  , configDbPreRequest          :: Maybe QualifiedIdentifier
  , configDbPreparedStatements  :: Bool
  , configDbRootSpec            :: Maybe QualifiedIdentifier
  , configDbSchemas             :: NonEmpty Text
  , configDbConfig              :: Bool
  , configDbTxAllowOverride     :: Bool
  , configDbTxRollbackAll       :: Bool
  , configDbUri                 :: Text
  , configDbUseLegacyGucs       :: Bool
  , configFilePath              :: Maybe FilePath
  , configJWKS                  :: Maybe JWKSet
  , configJwtAudience           :: Maybe StringOrURI
  , configJwtRoleClaimKey       :: JSPath
  , configJwtSecret             :: Maybe BS.ByteString
  , configJwtSecretIsBase64     :: Bool
  , configLogLevel              :: LogLevel
  , configOpenApiMode           :: OpenAPIMode
  , configOpenApiSecurityActive :: Bool
  , configOpenApiServerProxyUri :: Maybe Text
  , configRawMediaTypes         :: [MediaType]
  , configServerHost            :: Text
  , configServerPort            :: Int
  , configServerUnixSocket      :: Maybe FilePath
  , configServerUnixSocketMode  :: FileMode
  , configAdminServerPort       :: Maybe Int
  }

data LogLevel = LogCrit | LogError | LogWarn | LogInfo

dumpLogLevel :: LogLevel -> Text
dumpLogLevel = \case
  LogCrit  -> "crit"
  LogError -> "error"
  LogWarn  -> "warn"
  LogInfo  -> "info"

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
      [("db-anon-role",              q . fromMaybe "" . configDbAnonRole)
      ,("db-channel",                q . configDbChannel)
      ,("db-channel-enabled",            T.toLower . show . configDbChannelEnabled)
      ,("db-extra-search-path",      q . T.intercalate "," . configDbExtraSearchPath)
      ,("db-max-rows",                   maybe "\"\"" show . configDbMaxRows)
      ,("db-plan-enabled",               T.toLower . show . configDbPlanEnabled)
      ,("db-pool",                       show . configDbPoolSize)
      ,("db-pool-timeout",               show . floor . configDbPoolTimeout)
      ,("db-pre-request",            q . maybe mempty dumpQi . configDbPreRequest)
      ,("db-prepared-statements",        T.toLower . show . configDbPreparedStatements)
      ,("db-root-spec",              q . maybe mempty dumpQi . configDbRootSpec)
      ,("db-schemas",                q . T.intercalate "," . toList . configDbSchemas)
      ,("db-config",                     T.toLower . show . configDbConfig)
      ,("db-tx-end",                 q . showTxEnd)
      ,("db-uri",                    q . configDbUri)
      ,("db-use-legacy-gucs",            T.toLower . show . configDbUseLegacyGucs)
      ,("jwt-aud",                       T.decodeUtf8 . LBS.toStrict . JSON.encode . maybe "" toJSON . configJwtAudience)
      ,("jwt-role-claim-key",        q . T.intercalate mempty . fmap dumpJSPath . configJwtRoleClaimKey)
      ,("jwt-secret",                q . T.decodeUtf8 . showJwtSecret)
      ,("jwt-secret-is-base64",          T.toLower . show . configJwtSecretIsBase64)
      ,("log-level",                 q . dumpLogLevel . configLogLevel)
      ,("openapi-mode",              q . dumpOpenApiMode . configOpenApiMode)
      ,("openapi-security-active",       T.toLower . show . configOpenApiSecurityActive)
      ,("openapi-server-proxy-uri",  q . fromMaybe mempty . configOpenApiServerProxyUri)
      ,("raw-media-types",           q . T.decodeUtf8 . BS.intercalate "," . fmap toMime . configRawMediaTypes)
      ,("server-host",               q . configServerHost)
      ,("server-port",                   show . configServerPort)
      ,("server-unix-socket",        q . maybe mempty T.pack . configServerUnixSocket)
      ,("server-unix-socket-mode",   q . T.pack . showSocketMode)
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
readAppConfig :: [(Text, Text)] -> Maybe FilePath -> Maybe Text -> IO (Either Text AppConfig)
readAppConfig dbSettings optPath prevDbUri = do
  env <- readPGRSTEnvironment
  -- if no filename provided, start with an empty map to read config from environment
  conf <- maybe (return $ Right M.empty) loadConfig optPath

  case C.runParser (parser optPath env dbSettings) =<< mapLeft show conf of
    Left err ->
      return . Left $ "Error in config " <> err
    Right parsedConfig ->
      Right <$> decodeLoadFiles parsedConfig
  where
    -- Both C.ParseError and IOError are shown here
    loadConfig :: FilePath -> IO (Either SomeException C.Config)
    loadConfig = try . C.load

    decodeLoadFiles :: AppConfig -> IO AppConfig
    decodeLoadFiles parsedConfig =
      decodeJWKS <$>
        (decodeSecret =<< readSecretFile =<< readDbUriFile prevDbUri parsedConfig)

parser :: Maybe FilePath -> Environment -> [(Text, Text)] -> C.Parser C.Config AppConfig
parser optPath env dbSettings =
  AppConfig
    <$> parseAppSettings "app.settings"
    <*> optString "db-anon-role"
    <*> (fromMaybe "pgrst" <$> optString "db-channel")
    <*> (fromMaybe True <$> optBool "db-channel-enabled")
    <*> (maybe ["public"] splitOnCommas <$> optValue "db-extra-search-path")
    <*> optWithAlias (optInt "db-max-rows")
                     (optInt "max-rows")
    <*> (fromMaybe False <$> optBool "db-plan-enabled")
    <*> (fromMaybe 10 <$> optInt "db-pool")
    <*> (fromIntegral . fromMaybe 3600 <$> optInt "db-pool-timeout")
    <*> (fmap toQi <$> optWithAlias (optString "db-pre-request")
                                    (optString "pre-request"))
    <*> (fromMaybe True <$> optBool "db-prepared-statements")
    <*> (fmap toQi <$> optWithAlias (optString "db-root-spec")
                                    (optString "root-spec"))
    <*> (fromList . maybe ["public"] splitOnCommas <$> optWithAlias (optValue "db-schemas")
                                                                    (optValue "db-schema"))
    <*> (fromMaybe True <$> optBool "db-config")
    <*> parseTxEnd "db-tx-end" snd
    <*> parseTxEnd "db-tx-end" fst
    <*> (fromMaybe "postgresql://" <$> optString "db-uri")
    <*> (fromMaybe True <$> optBool "db-use-legacy-gucs")
    <*> pure optPath
    <*> pure Nothing
    <*> parseJwtAudience "jwt-aud"
    <*> parseRoleClaimKey "jwt-role-claim-key" "role-claim-key"
    <*> (fmap encodeUtf8 <$> optString "jwt-secret")
    <*> (fromMaybe False <$> optWithAlias
          (optBool "jwt-secret-is-base64")
          (optBool "secret-is-base64"))
    <*> parseLogLevel "log-level"
    <*> parseOpenAPIMode "openapi-mode"
    <*> (fromMaybe False <$> optBool "openapi-security-active")
    <*> parseOpenAPIServerProxyURI "openapi-server-proxy-uri"
    <*> (maybe [] (fmap (MTOther . encodeUtf8) . splitOnCommas) <$> optValue "raw-media-types")
    <*> (fromMaybe "!4" <$> optString "server-host")
    <*> (fromMaybe 3000 <$> optInt "server-port")
    <*> (fmap T.unpack <$> optString "server-unix-socket")
    <*> parseSocketFileMode "server-unix-socket-mode"
    <*> optInt "admin-server-port"
  where
    parseAppSettings :: C.Key -> C.Parser C.Config [(Text, Text)]
    parseAppSettings key = addFromEnv . fmap (fmap coerceText) <$> C.subassocs key C.value
      where
        addFromEnv f = M.toList $ M.union fromEnv $ M.fromList f
        fromEnv = M.mapKeys fromJust $ M.filterWithKey (\k _ -> isJust k) $ M.mapKeys normalize env
        normalize k = ("app.settings." <>) <$> T.stripPrefix "PGRST_APP_SETTINGS_" (toS k)

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

    parseJwtAudience :: C.Key -> C.Parser C.Config (Maybe StringOrURI)
    parseJwtAudience k =
      optString k >>= \case
        Nothing -> pure Nothing -- no audience in config file
        Just aud -> case preview stringOrUri (T.unpack aud) of
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
      case reloadableDbSetting <|> M.lookup envVarName env of
        Just dbOrEnvVal -> pure $ justIfMaybe $ coercion $ C.String dbOrEnvVal
        Nothing  -> necessity key (coercion <$> C.value)
      where
        dashToUnderscore '-' = '_'
        dashToUnderscore c   = c
        envVarName = "PGRST_" <> (toUpper . dashToUnderscore <$> toS key)
        reloadableDbSetting =
          let dbSettingName = T.pack $ dashToUnderscore <$> toS key in
          if dbSettingName `notElem` [
            "server_host", "server_port", "server_unix_socket", "server_unix_socket_mode", "admin_server_port", "log_level",
            "db_uri", "db_channel_enabled", "db_channel", "db_pool", "db_pool_timeout", "db_config"]
          then lookup dbSettingName dbSettings
          else Nothing

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

-- | Parse `jwt-secret` configuration option and turn into a JWKSet.
--
-- There are three ways to specify `jwt-secret`: text secret, JSON Web Key
-- (JWK), or JSON Web Key Set (JWKS). The first two are converted into a JWKSet
-- with one key and the last is converted as is.
decodeJWKS :: AppConfig -> AppConfig
decodeJWKS conf =
  conf { configJWKS = parseSecret <$> configJwtSecret conf }

parseSecret :: ByteString -> JWKSet
parseSecret bytes =
  fromMaybe (maybe secret (\jwk' -> JWT.JWKSet [jwk']) maybeJWK)
    maybeJWKSet
  where
    maybeJWKSet = JSON.decodeStrict bytes :: Maybe JWKSet
    maybeJWK = JSON.decodeStrict bytes :: Maybe JWK
    secret = JWT.JWKSet [JWT.fromKeyMaterial keyMaterial]
    keyMaterial = JWT.OctKeyMaterial . JWT.OctKeyParameters $ JOSE.Base64Octets bytes

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
