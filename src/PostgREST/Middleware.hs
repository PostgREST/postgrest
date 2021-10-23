{-|
Module      : PostgREST.Middleware
Description : Sets CORS policy. Also the PostgreSQL GUCs, role, search_path and pre-request function.
-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Middleware
  ( runPgLocals
  , pgrstFormat
  , pgrstMiddleware
  , defaultCorsPolicy
  , corsPolicy
  , optionalRollback
  , getJwtClaims
  , getRoleClaim
  ) where

import qualified Data.Aeson                           as JSON
import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy.Char8           as LBS
import qualified Data.CaseInsensitive                 as CI
import qualified Data.HashMap.Strict                  as M
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Vault.Lazy                      as Vault
import qualified Hasql.Decoders                       as HD
import qualified Hasql.DynamicStatements.Snippet      as SQL hiding
                                                             (sql)
import qualified Hasql.DynamicStatements.Statement    as SQL
import qualified Hasql.Transaction                    as SQL
import qualified Network.HTTP.Types.Header            as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Logger                   as Wai
import qualified Network.Wai.Middleware.Cors          as Wai
import qualified Network.Wai.Middleware.Gzip          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.Wai.Middleware.Static        as Wai


import Control.Arrow ((***))

import Data.Function             (id)
import Data.List                 (lookup)
import Data.Scientific           (FPFormat (..), formatScientific,
                                  isInteger)
import Network.HTTP.Types.Status (Status, status200, status400, status500,
                                  statusCode)
import System.IO.Unsafe          (unsafePerformIO)
import System.Log.FastLogger     (toLogStr)

import PostgREST.AppState           (AppState, getConfig, getTime)
import PostgREST.Auth               (JWTClaims, jwtClaims)
import PostgREST.Config             (AppConfig (..), LogLevel (..))
import PostgREST.Config.PgVersion   (PgVersion (..), pgVersion140)
import PostgREST.Error              (Error, errorResponseFor)
import PostgREST.GucHeader          (addHeadersIfNotIncluded)
import PostgREST.Query.SqlFragment  (fromQi, intercalateSnippet,
                                     unknownEncoder)
import PostgREST.Request.ApiRequest (ApiRequest (..), Target (..))

import PostgREST.Request.Preferences

import Protolude

-- | Runs local(transaction scoped) GUCs for every request, plus the pre-request function
runPgLocals :: AppConfig   -> M.HashMap Text JSON.Value ->
               (ApiRequest -> ExceptT Error SQL.Transaction Wai.Response) ->
               ApiRequest  -> ByteString -> PgVersion -> ExceptT Error SQL.Transaction Wai.Response
runPgLocals conf claims app req jsonDbS actualPgVersion = do
  lift $ SQL.statement mempty $ SQL.dynamicallyParameterized
    ("select " <> intercalateSnippet ", " (searchPathSql : roleSql ++ claimsSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ appSettingsSql ++ specSql))
    HD.noResult (configDbPreparedStatements conf)
  lift $ traverse_ SQL.sql preReqSql
  app req
  where
    methodSql = setConfigLocal mempty ("request.method", iMethod req)
    pathSql = setConfigLocal mempty ("request.path", iPath req)
    headersSql = if usesLegacyGucs
                   then setConfigLocal "request.header." <$> iHeaders req
                   else setConfigLocalJson "request.headers" (iHeaders req)
    cookiesSql = if usesLegacyGucs
                   then setConfigLocal "request.cookie." <$> iCookies req
                   else setConfigLocalJson "request.cookies" (iCookies req)
    claimsWithRole =
      let anon = JSON.String . toS $ configDbAnonRole conf in -- role claim defaults to anon if not specified in jwt
      M.union claims (M.singleton "role" anon)
    claimsSql = if usesLegacyGucs
                  then setConfigLocal "request.jwt.claim." <$> [(toUtf8 c, toUtf8 $ unquoted v) | (c,v) <- M.toList claimsWithRole]
                  else [setConfigLocal mempty ("request.jwt.claims", LBS.toStrict $ JSON.encode claimsWithRole)]
    roleSql = maybeToList $ (\x -> setConfigLocal mempty ("role", toUtf8 $ unquoted x)) <$> M.lookup "role" claimsWithRole
    appSettingsSql = setConfigLocal mempty <$> (join bimap toUtf8 <$> configAppSettings conf)
    searchPathSql =
      let schemas = T.intercalate ", " (iSchema req : configDbExtraSearchPath conf) in
      setConfigLocal mempty ("search_path", toUtf8 schemas)
    preReqSql = (\f -> "select " <> fromQi f <> "();") <$> configDbPreRequest conf
    specSql = case iTarget req of
      TargetProc{tpIsRootSpec=True} -> [setConfigLocal mempty ("request.spec", jsonDbS)]
      _                             -> mempty
    usesLegacyGucs = configDbUseLegacyGucs conf && actualPgVersion < pgVersion140

-- | Log in apache format. Only requests that have a status greater than minStatus are logged.
-- | There's no way to filter logs in the apache format on wai-extra: https://hackage.haskell.org/package/wai-extra-3.0.29.2/docs/Network-Wai-Middleware-RequestLogger.html#t:OutputFormat.
-- | So here we copy wai-logger apacheLogStr function: https://github.com/kazu-yamamoto/logger/blob/a4f51b909a099c51af7a3f75cf16e19a06f9e257/wai-logger/Network/Wai/Logger/Apache.hs#L45
-- | TODO: Add the ability to filter apache logs on wai-extra and remove this function.
pgrstFormat :: Status -> Wai.OutputFormatter
pgrstFormat minStatus date req status responseSize =
  if status < minStatus
    then mempty
  else toLogStr (getSourceFromSocket req)
    <> " - "
    <> toLogStr (maybe "-" unquoted (getRoleClaim req))
    <> " ["
    <> toLogStr date
    <> "] \""
    <> toLogStr (Wai.requestMethod req)
    <> " "
    <> toLogStr (Wai.rawPathInfo req <> Wai.rawQueryString req)
    <> " "
    <> toLogStr (show (Wai.httpVersion req)::Text)
    <> "\" "
    <> toLogStr (show (statusCode status)::Text)
    <> " "
    <> toLogStr (maybe "-" show responseSize::Text)
    <> " \""
    <> toLogStr (fromMaybe mempty $ Wai.requestHeaderReferer req)
    <> "\" \""
    <> toLogStr (fromMaybe mempty $ Wai.requestHeaderUserAgent req)
    <> "\"\n"
  where
    getSourceFromSocket = BS.pack . Wai.showSockAddr . Wai.remoteHost

pgrstMiddleware :: LogLevel -> AppState -> Wai.Application -> Wai.Application
pgrstMiddleware logLevel appState =
    authMiddleware appState logLevel
  . loggerMiddleware logLevel
  . Wai.cors corsPolicy
  . Wai.staticPolicy (Wai.only [("favicon.ico", "static/favicon.ico")])

loggerMiddleware :: LogLevel -> Wai.Application -> Wai.Application
loggerMiddleware LogCrit  = id
loggerMiddleware LogError = unsafePerformIO $ Wai.mkRequestLogger Wai.def { Wai.outputFormat = Wai.CustomOutputFormat $ pgrstFormat status500}
loggerMiddleware LogWarn  = unsafePerformIO $ Wai.mkRequestLogger Wai.def { Wai.outputFormat = Wai.CustomOutputFormat $ pgrstFormat status400}
loggerMiddleware LogInfo  = unsafePerformIO $ Wai.mkRequestLogger Wai.def { Wai.outputFormat = Wai.CustomOutputFormat $ pgrstFormat status200}

defaultCorsPolicy :: Wai.CorsResourcePolicy
defaultCorsPolicy =  Wai.CorsResourcePolicy Nothing
  ["GET", "POST", "PATCH", "PUT", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

-- | CORS policy to be used in by Wai Cors middleware
corsPolicy :: Wai.Request -> Maybe Wai.CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      Wai.corsOrigins = Just ([origin], True)
    , Wai.corsRequestHeaders = "Authentication" : accHeaders
    , Wai.corsExposedHeaders = Just [
        "Content-Encoding", "Content-Location", "Content-Range", "Content-Type"
      , "Date", "Location", "Server", "Transfer-Encoding", "Range-Unit"
      ]
    }
  Nothing -> Nothing
  where
    headers = Wai.requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . BS.strip) $ BS.split ',' hdrs
      Nothing   -> []

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = show b
unquoted v = T.decodeUtf8 . LBS.toStrict $ JSON.encode v

-- | Set a transaction to eventually roll back if requested and set respective
-- headers on the response.
optionalRollback
  :: AppConfig
  -> ApiRequest
  -> ExceptT Error SQL.Transaction Wai.Response
  -> ExceptT Error SQL.Transaction Wai.Response
optionalRollback AppConfig{..} ApiRequest{..} transaction = do
  resp <- catchError transaction $ return . errorResponseFor
  when (shouldRollback || (configDbTxRollbackAll && not shouldCommit))
    (lift SQL.condemn)
  return $ Wai.mapResponseHeaders preferenceApplied resp
  where
    shouldCommit =
      configDbTxAllowOverride && iPreferTransaction == Just Commit
    shouldRollback =
      configDbTxAllowOverride && iPreferTransaction == Just Rollback
    preferenceApplied
      | shouldCommit =
          addHeadersIfNotIncluded
            [toAppliedHeader Commit]
      | shouldRollback =
          addHeadersIfNotIncluded
            [toAppliedHeader Rollback]
      | otherwise =
          identity

-- | Do a pg set_config(setting, value, true) call. This is equivalent to a SET LOCAL.
setConfigLocal :: ByteString -> (ByteString, ByteString) -> SQL.Snippet
setConfigLocal prefix (k, v) =
  "set_config(" <> unknownEncoder (prefix <> k) <> ", " <> unknownEncoder v <> ", true)"

-- | Starting from PostgreSQL v14, some characters are not allowed for config names (mostly affecting headers with "-").
-- | A JSON format string is used to avoid this problem. See https://github.com/PostgREST/postgrest/issues/1857
setConfigLocalJson :: ByteString -> [(ByteString, ByteString)] -> [SQL.Snippet]
setConfigLocalJson prefix keyVals = [setConfigLocal mempty (prefix, gucJsonVal keyVals)]
  where
    gucJsonVal :: [(ByteString, ByteString)] -> ByteString
    gucJsonVal = LBS.toStrict . JSON.encode . M.fromList . arrayByteStringToText
    arrayByteStringToText :: [(ByteString, ByteString)] -> [(Text,Text)]
    arrayByteStringToText keyVal = (T.decodeUtf8 *** T.decodeUtf8) <$> keyVal

-- | Validate authorization header.
--   Parse and store JWT claims for future use in the request.
authMiddleware :: AppState -> LogLevel -> Wai.Application -> Wai.Application
authMiddleware appState app req respond = do
  conf <- getConfig appState
  time <- getTime appState

  let auth = fromMaybe "" $ lookup HTTP.hAuthorization (Wai.requestHeaders req)
  let tokenStr = case BS.split ' ' auth of
        ("Bearer" : t : _) -> t
        ("bearer" : t : _) -> t
        _                  -> ""

  mclaims <- runExceptT $ jwtClaims conf (LBS.fromStrict tokenStr) time
  case mclaims of
    Right claims ->
      let req' = req { Wai.vault = Vault.insert jwtKey claims $ Wai.vault req }
      in app req' respond
    Left error -> loggerMiddleware logLevel (\_req' respond' -> respond' $ errorResponseFor error) req respond

jwtKey :: Vault.Key JWTClaims
jwtKey = unsafePerformIO Vault.newKey
{-# NOINLINE jwtKey #-}

getJwtClaims :: Wai.Request -> Maybe JWTClaims
getJwtClaims = Vault.lookup jwtKey . Wai.vault

getRoleClaim :: Wai.Request -> Maybe JSON.Value
getRoleClaim req = M.lookup "role" =<< getJwtClaims req
