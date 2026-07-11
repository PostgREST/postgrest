{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : PostgREST.Query.PreQuery
Description : Builds queries that run prior to the main query
-}
module PostgREST.Query.PreQuery
  ( txVarQuery
  , preReqQuery
  ) where

import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.KeyMap               as KM
import qualified Data.ByteString.Lazy.Char8      as LBS
import qualified Data.HashMap.Strict             as HM
import qualified Data.Text                       as T
import qualified Hasql.DynamicStatements.Snippet as SQL hiding (sql)



import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.ApiRequest.Preferences  (PreferTimezone (..),
                                          Preferences (..))
import PostgREST.ApiRequest.Types        (Resource (..))
import PostgREST.Auth.Types              (AuthResult (..))
import PostgREST.Config                  (AppConfig (..))
import PostgREST.Plan                    (CrudPlan (..),
                                          DbActionPlan (..))
import PostgREST.Query.SqlFragment       (escapeIdentList, fromQi,
                                          intercalateSnippet,
                                          setConfigWithConstantName,
                                          setConfigWithConstantNameJSON,
                                          setConfigWithDynamicName)
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..))
import PostgREST.SchemaCache.Routine     (Routine (..))
import PostgREST.Version                 (prettyVersion)

import Protolude hiding (Handler)

-- sets transaction variables
txVarQuery :: DbActionPlan -> AppConfig -> AuthResult -> ApiRequest -> SQL.Snippet
txVarQuery dbActPlan AppConfig{..} AuthResult{..} ApiRequest{..} =
    -- To ensure `GRANT SET ON PARAMETER <superuser_setting> TO authenticator` works, the role settings must be set before the impersonated role.
    -- Otherwise the GRANT SET would have to be applied to the impersonated role. See https://github.com/PostgREST/postgrest/issues/3045
    "select " <> intercalateSnippet ", " (
      searchPathSql : roleSettingsSql ++ roleSql ++ claimsSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ timezoneSql ++ funcSettingsSql ++ appSettingsSql ++ rootSpecSettingsSql
    )
  where
    methodSql = setConfigWithConstantName ("request.method", iMethod)
    pathSql = setConfigWithConstantName ("request.path", iPath)
    headersSql = setConfigWithConstantNameJSON "request.headers" iHeaders
    cookiesSql = setConfigWithConstantNameJSON "request.cookies" iCookies
    claimsSql = [setConfigWithConstantName ("request.jwt.claims", LBS.toStrict $ JSON.encode claims)]
      where
        claims = authClaims & KM.insert "role" (JSON.String $ decodeUtf8 authRole) -- insert "role" to claims as well

    roleSql = [setConfigWithConstantName ("role", authRole)]
    roleSettingsSql = setConfigWithDynamicName <$> HM.toList (fromMaybe mempty $ HM.lookup authRole configRoleSettings)
    appSettingsSql = setConfigWithDynamicName . join bimap toUtf8 <$> configAppSettings
    rootSpecSettingsSql
      | isRootSpec iResource =
          [ setConfigWithConstantName ("pgrst.server_host", toUtf8 configServerHost)
          , setConfigWithConstantName ("pgrst.server_port", toUtf8 (show configServerPort :: Text))
          , setConfigWithConstantName ("pgrst.openapi_server_proxy_uri", maybe mempty toUtf8 configOpenApiServerProxyUri)
          , setConfigWithConstantName ("pgrst.db_schemas", toUtf8 $ T.intercalate "," $ toList configDbSchemas)
          , setConfigWithConstantName ("pgrst.version", prettyVersion)
          ]
      | otherwise = mempty
    timezoneSql = maybe mempty (\(PreferTimezone tz) -> [setConfigWithConstantName ("timezone", tz)]) $ preferTimezone iPreferences
    funcSettingsSql = setConfigWithDynamicName . join bimap toUtf8 <$> funcSettings
    searchPathSql =
      let schemas = escapeIdentList (iSchema : configDbExtraSearchPath) in
      setConfigWithConstantName ("search_path", schemas)
    funcSettings = case dbActPlan of
      DbCrud _ CallReadPlan{crProc} -> pdFuncSettings crProc
      _                             -> mempty
    isRootSpec resource = case resource of { ResourceRoutine _ True -> True; _ -> False }

-- runs the pre-request function
preReqQuery :: QualifiedIdentifier -> SQL.Snippet
preReqQuery preRequest = "select " <> fromQi preRequest <> "()"
