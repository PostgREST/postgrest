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
import           Data.Functor.Contravariant
import qualified Data.HashMap.Strict             as HM
import           Data.Profunctor
import           Data.Tuple.Extra
import qualified Hasql.Decoders                  as HD
import qualified Hasql.DynamicStatements.Snippet as SQL hiding (sql)
import qualified Hasql.Encoders                  as HE
import qualified Hasql.Statement                 as SQL
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
    lmap (const settings) $ SQL.Statement txSettingsSql txSettingsParams HD.noResult configDbPreparedStatements
  where
    settings = unzip $
      -- To ensure `GRANT SET ON PARAMETER <superuser_setting> TO authenticator` works, the role settings must be set before the impersonated role.
      -- Otherwise the GRANT SET would have to be applied to the impersonated role. See https://github.com/PostgREST/postgrest/issues/3045
      searchPathSetting : roleSettings ++
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
    roleSettings = HM.toList $ fromMaybe mempty $ HM.lookup authRole configRoleSettings
    appSettings = join bimap toUtf8 <$> configAppSettings
    timezoneSetting = maybe mempty (\(PreferTimezone tz) -> [("timezone", tz)]) $ preferTimezone iPreferences
    funcSettings = case dbActPlan of
      DbCrud _ CallReadPlan{crProc} -> join bimap toUtf8 <$> pdFuncSettings crProc
      _                             -> mempty
    searchPathSetting =
      let schemas = escapeIdentList (iSchema : configDbExtraSearchPath) in
      ("search_path", schemas)

txSettingsSql :: ByteString
txSettingsSql =
  "select set_config(setting, value, true) " <>
  "from unnest($1::text[], $2::text[]) with ordinality as _(setting, value, ordinality) " <>
  "order by ordinality"
txSettingsParams :: HE.Params ([ByteString], [ByteString])
txSettingsParams =
    (fst >$< txSettingsParameter) <>
    (snd >$< txSettingsParameter)
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
