{-# LANGUAGE QuasiQuotes #-}

module PostgREST.Config.Database
  ( pgVersionStatement
  , queryDbSettings
  , queryRoleSettings
  , queryPgVersion
  , RoleSettings
  ) where

import Control.Arrow ((***))

import PostgREST.Config.PgVersion (PgVersion (..))

import qualified Data.HashMap.Strict as HM

import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import           Hasql.Session              (Session, statement)
import qualified Hasql.Statement            as SQL
import qualified Hasql.Transaction          as SQL
import qualified Hasql.Transaction.Sessions as SQL

import Text.InterpolatedString.Perl6 (q, qc)

import Protolude

type RoleSettings = (HM.HashMap ByteString (HM.HashMap ByteString ByteString))

prefix :: Text
prefix = "pgrst."

-- | In-db settings names
dbSettingsNames :: [Text]
dbSettingsNames =
  (prefix <>) <$>
  ["db_anon_role"
  ,"db_pre_config"
  ,"db_extra_search_path"
  ,"db_max_rows"
  ,"db_plan_enabled"
  ,"db_pre_request"
  ,"db_prepared_statements"
  ,"db_root_spec"
  ,"db_schemas"
  ,"db_tx_end"
  ,"db_use_legacy_gucs"
  ,"jwt_aud"
  ,"jwt_role_claim_key"
  ,"jwt_secret"
  ,"jwt_secret_is_base64"
  ,"openapi_mode"
  ,"openapi_security_active"
  ,"openapi_server_proxy_uri"
  ,"raw_media_types"
  ,"server_trace_header"
  ]

queryPgVersion :: Bool -> Session PgVersion
queryPgVersion prepared = statement mempty $ pgVersionStatement prepared

pgVersionStatement :: Bool -> SQL.Statement () PgVersion
pgVersionStatement = SQL.Statement sql HE.noParams versionRow
  where
    sql = "SELECT current_setting('server_version_num')::integer, current_setting('server_version')"
    versionRow = HD.singleRow $ PgVersion <$> column HD.int4 <*> column HD.text

-- | Query the in-database configuration. The settings have the following priorities:
--
-- 1. Role + with database-specific settings:
--    ALTER ROLE authenticator IN DATABASE postgres SET <prefix>jwt_aud = 'val';
-- 2. Role + with settings:
--    ALTER ROLE authenticator SET <prefix>jwt_aud = 'overridden';
-- 3. pre-config function:
--    CREATE FUNCTION pre_config() .. PERFORM set_config(<prefix>jwt_aud, 'pre_config_aud'..)
--
-- The example above will result in <prefix>jwt_aud = 'val'
-- A setting on the database only will have no effect: ALTER DATABASE postgres SET <prefix>jwt_aud = 'xx'
queryDbSettings :: Maybe Text -> Bool -> Session [(Text, Text)]
queryDbSettings preConfFunc prepared =
  let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction in
  transaction SQL.ReadCommitted SQL.Read $ SQL.statement dbSettingsNames $ SQL.Statement sql (arrayParam HE.text) decodeSettings prepared
  where
    sql = [qc|
      WITH
      role_setting AS (
        SELECT setdatabase as database,
               unnest(setconfig) as setting
        FROM pg_catalog.pg_db_role_setting
        WHERE setrole = CURRENT_USER::regrole::oid
          AND setdatabase IN (0, (SELECT oid FROM pg_catalog.pg_database WHERE datname = CURRENT_CATALOG))
      ),
      kv_settings AS (
        SELECT database,
               substr(setting, 1, strpos(setting, '=') - 1) as k,
               substr(setting, strpos(setting, '=') + 1) as v
        FROM role_setting
        {preConfigF}
      )
      SELECT DISTINCT ON (key)
             replace(k, '{prefix}', '') AS key,
             v AS value
      FROM kv_settings
      WHERE k = ANY($1) AND v IS NOT NULL
      ORDER BY key, database DESC NULLS LAST;
    |]
    preConfigF = case preConfFunc of
      Nothing   -> mempty
      Just func -> [qc|
          UNION
          SELECT
            null as database,
            x as k,
            current_setting(x, true) as v
          FROM unnest($1) x
          JOIN {func}() _ ON TRUE
      |]::Text
    decodeSettings = HD.rowList $ (,) <$> column HD.text <*> column HD.text

queryRoleSettings :: Bool -> Session RoleSettings
queryRoleSettings prepared =
  let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction in
  transaction SQL.ReadCommitted SQL.Read $ SQL.statement mempty $ roleSettingsStatement prepared

roleSettingsStatement :: Bool -> SQL.Statement () RoleSettings
roleSettingsStatement = SQL.Statement sql HE.noParams decodeRoleSettings
  where
    sql = [q|
      with
      role_setting as (
        select r.rolname, unnest(r.rolconfig) as setting
        from pg_auth_members m
        join pg_roles r on r.oid = m.roleid
        where member = current_user::regrole::oid
      ),
      kv_settings AS (
        SELECT
          rolname,
          substr(setting, 1, strpos(setting, '=') - 1) as key,
          lower(substr(setting, strpos(setting, '=') + 1)) as value
        FROM role_setting
      )
      select rolname, array_agg(row(key, value))
      from kv_settings
      group by rolname;
    |]
    decodeRoleSettings = HM.fromList . map (bimap encodeUtf8 (HM.fromList . ((encodeUtf8 *** encodeUtf8) <$>))) <$> HD.rowList aRow
    aRow :: HD.Row (Text, [(Text, Text)])
    aRow = (,) <$> column HD.text <*> compositeArrayColumn ((,) <$> compositeField HD.text <*> compositeField HD.text)

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

compositeField :: HD.Value a -> HD.Composite a
compositeField = HD.field . HD.nonNullable

compositeArrayColumn :: HD.Composite a -> HD.Row [a]
compositeArrayColumn = arrayColumn . HD.composite

arrayColumn :: HD.Value a -> HD.Row [a]
arrayColumn = column . HD.listArray . HD.nonNullable

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable

arrayParam :: HE.Value a -> HE.Params [a]
arrayParam = param . HE.foldableArray . HE.nonNullable
