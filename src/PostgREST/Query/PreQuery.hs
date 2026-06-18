{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import           Data.Functor.Contravariant
import qualified Data.HashMap.Strict             as HM
import           Data.Profunctor
import           Data.Tuple.Extra
import qualified Hasql.Decoders                  as HD
import qualified Hasql.DynamicStatements.Snippet as SQL hiding (sql)
import qualified Hasql.Encoders                  as HE
import qualified Hasql.Statement                 as SQL
import           NeatInterpolation               (trimming)
import qualified PostgreSQL.Binary.Encoding      as PGBinary
import           Unsafe.Coerce                   (unsafeCoerce)

import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.ApiRequest.Preferences  (PreferTimezone (..),
                                          Preferences (..))
import PostgREST.Auth.Types              (AuthResult (..))
import PostgREST.Config                  (AppConfig (..))
import PostgREST.Plan                    (CrudPlan (..),
                                          DbActionPlan (..))
import PostgREST.Query.SqlFragment       (escapeIdentList, fromQi)
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..))
import PostgREST.SchemaCache.Routine     (Routine (..))

import Protolude

-- sets transaction variables
txVarQuery :: DbActionPlan -> AppConfig -> AuthResult -> ApiRequest -> SQL.Statement () ()
txVarQuery dbActPlan AppConfig{..} AuthResult{..} ApiRequest{..} =
    if configDbConfig then
      lmap (const roleSettingsParams) $ SQL.Statement (txSettingsSqlWithRoleSettings configDbHasParameterPrivilege) txRoleSettingsParams HD.noResult configDbPreparedStatements
    else
      lmap (const settings) $ SQL.Statement txSettingsSqlWithoutRoleSettings txSettingsParams HD.noResult configDbPreparedStatements
  where
    settings =
      preRoleSettings ++ postRoleSettings

    roleSettingsParams =
      (preRoleSettings, authRole, postRoleSettings)

    preRoleSettings =
      [ searchPathSetting ]

    postRoleSettings =
      [ roleSetting
      , claimsSetting
      , methodSetting
      , pathSetting
      , headersSetting
      , cookiesSetting
      ] ++
      timezoneSetting ++
      funcSettings ++
      appSettings

    methodSetting = ("request.method", iMethod)
    pathSetting = ("request.path", iPath)
    headersSetting = ("request.headers", gucJsonVal iHeaders)
    cookiesSetting = ("request.cookies", gucJsonVal iCookies)
    claimsSetting = ("request.jwt.claims", LBS.toStrict $ JSON.encode claims)
      where
        claims = authClaims & KM.insert "role" (JSON.String $ decodeUtf8 authRole) -- insert "role" to claims as well

    roleSetting = ("role", authRole)
    appSettings = join bimap toUtf8 <$> configAppSettings
    timezoneSetting = maybe mempty (\(PreferTimezone tz) -> [("timezone", tz)]) $ preferTimezone iPreferences
    funcSettings = case dbActPlan of
      DbCrud _ CallReadPlan{crProc} -> join bimap toUtf8 <$> pdFuncSettings crProc
      _                             -> mempty
    searchPathSetting =
      let schemas = escapeIdentList (iSchema : configDbExtraSearchPath) in
      ("search_path", schemas)

txSettingsSqlWithoutRoleSettings :: ByteString
txSettingsSqlWithoutRoleSettings =
  encodeUtf8 [trimming|
    select set_config(setting, value, true)
    from unnest($$1::text[], $$2::text[]) with ordinality as _(setting, value, ordinality)
    order by ordinality
  |]

txSettingsSqlWithRoleSettings :: Bool -> ByteString
txSettingsSqlWithRoleSettings hasParameterPrivilege =
  encodeUtf8 [trimming|
    with pre_settings as (
      select 1::int as sort_order, ordinality, setting, value
      from unnest($$1::text[], $$2::text[]) with ordinality as _(setting, value, ordinality)
    ),
    role_setting as (
      select unnest(r.rolconfig) as setting
      from pg_catalog.pg_roles r
      where r.rolname = $$3
    ),
    kv_settings as (
      select setting_parts[1] as key, array_to_string(setting_parts[2:], '=') as value
      from role_setting
      cross join lateral string_to_array(setting, '=') as parsed(setting_parts)
    ),
    role_settings as (
      select 2::int as sort_order, row_number() over (order by kv.key) as ordinality, kv.key as setting, kv.value
      from kv_settings kv
      join pg_catalog.pg_settings ps on ps.name = kv.key and ${parameterPrivilegeCheck}
      where kv.key <> 'default_transaction_isolation'
    ),
    post_settings as (
      select 3::int as sort_order, ordinality, setting, value
      from unnest($$4::text[], $$5::text[]) with ordinality as _(setting, value, ordinality)
    ),
    settings as (
      select * from pre_settings
      union all select * from role_settings
      union all select * from post_settings
    )
    select set_config(setting, value, true)
    from settings
    order by sort_order, ordinality
  |]
  where
    parameterPrivilegeCheck =
      if hasParameterPrivilege then
        [trimming|(ps.context = 'user' or has_parameter_privilege(quote_ident(current_user)::regrole::oid, ps.name, 'set'))|] :: Text
      else
        [trimming|(ps.context = 'user')|] :: Text

txSettingsParams :: HE.Params [(ByteString, ByteString)]
txSettingsParams =
  unzip >$< ((fst >$< txSettingsParameter) <> (snd >$< txSettingsParameter))

txRoleSettingsParams :: HE.Params ([(ByteString, ByteString)], ByteString, [(ByteString, ByteString)])
txRoleSettingsParams =
    ((\(preSettings, _, _) -> preSettings) >$< txSettingsParams) <>
    ((\(_, role, _) -> role) >$< txRoleParameter) <>
    ((\(_, _, postSettings) -> postSettings) >$< txSettingsParams)

txRoleParameter :: HE.Params ByteString
txRoleParameter =
  HE.param $ HE.nonNullable HE.unknown

txSettingsParameter :: HE.Params [ByteString]
txSettingsParameter =
  HE.param $ HE.nonNullable txSettingsArray

data HasqlValue a = HasqlValue !Any !Any !(Bool -> a -> PGBinary.Encoding) !Any

txSettingsArray :: HE.Value [ByteString]
txSettingsArray =
  -- hasql-1.9.3.1 does not expose a custom Value constructor; this mirrors its hidden Value/OID layout.
  unsafeCoerce $ HasqlValue textArrayValueOid textArrayArrayOid textArrayEncoding textArrayRender
  where
    hasqlTextArrayValue :: HE.Value [Text]
    hasqlTextArrayValue = HE.foldableArray $ HE.nonNullable HE.text

    HasqlValue textArrayValueOid textArrayArrayOid _ textArrayRender = unsafeCoerce hasqlTextArrayValue

textArrayEncoding :: Bool -> [ByteString] -> PGBinary.Encoding
textArrayEncoding _ =
  PGBinary.array_foldable textOid (Just . PGBinary.bytea_strict)

textOid :: Word32
textOid = 25

-- Starting from PostgreSQL v14, some characters are not allowed for config names (mostly affecting headers with "-").
-- A JSON format string is used to avoid this problem. See https://github.com/PostgREST/postgrest/issues/1857
gucJsonVal :: [(ByteString, ByteString)] -> ByteString
gucJsonVal =
  LBS.toStrict . JSON.encode . HM.fromList . fmap (both decodeUtf8)

-- runs the pre-request function
preReqQuery :: QualifiedIdentifier -> SQL.Snippet
preReqQuery preRequest = "select " <> fromQi preRequest <> "()"
