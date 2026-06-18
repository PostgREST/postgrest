{-# LANGUAGE QuasiQuotes #-}

module PostgREST.Config.Database
  ( pgVersionStatement
  , queryDbSettings
  , queryPgVersion
  , queryRoleSettings
  , RoleIsolationLvl
  , TimezoneNames
  , toIsolationLevel
  ) where

import PostgREST.Config.PgVersion (PgVersion (..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import           Hasql.Session              (Session, statement)
import qualified Hasql.Statement            as SQL
import qualified Hasql.Transaction          as SQL
import qualified Hasql.Transaction.Sessions as SQL

import NeatInterpolation (trimming)

import Protolude

type RoleIsolationLvl = HM.HashMap ByteString SQL.IsolationLevel
type TimezoneNames    = Set Text -- cache timezone names for prefer timezone=

toIsolationLevel :: Text -> SQL.IsolationLevel
toIsolationLevel a = case T.toLower a of
  "repeatable read" -> SQL.RepeatableRead
  "serializable"    -> SQL.Serializable
  _                 -> SQL.ReadCommitted

prefix :: Text
prefix = "pgrst."

-- | In-db settings names
dbSettingsNames :: [Text]
dbSettingsNames =
  (prefix <>) <$>
  ["db_aggregates_enabled"
  ,"client_error_verbosity"
  ,"db_anon_role"
  ,"db_pre_config"
  ,"db_extra_search_path"
  ,"db_max_rows"
  ,"db_plan_enabled"
  ,"db_pre_request"
  ,"db_prepared_statements"
  ,"db_root_spec"
  ,"db_schemas"
  ,"db_timezone_enabled"
  ,"db_tx_end"
  ,"db_hoisted_tx_settings"
  ,"jwt_aud"
  ,"jwt_role_claim_key"
  ,"jwt_secret"
  ,"jwt_secret_is_base64"
  ,"jwt_cache_max_lifetime"
  ,"openapi_mode"
  ,"openapi_security_active"
  ,"openapi_server_proxy_uri"
  ,"server_cors_allowed_origins"
  ,"server_trace_header"
  ,"server_timing_enabled"
  ]

queryPgVersion :: Session PgVersion
queryPgVersion = statement mempty $ pgVersionStatement False

pgVersionStatement :: Bool -> SQL.Statement () PgVersion
pgVersionStatement = SQL.Statement sql HE.noParams versionRow
  where
    sql = "SELECT current_setting('server_version_num')::integer, current_setting('server_version'), version()"
    versionRow = HD.singleRow $ PgVersion <$> column HD.int4 <*> column HD.text <*> column HD.text

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
queryDbSettings :: Maybe Text -> Session [(Text, Text)]
queryDbSettings preConfFunc =
  SQL.transactionNoRetry SQL.ReadCommitted SQL.Read $ SQL.statement dbSettingsNames $ SQL.Statement sql (arrayParam HE.text) decodeSettings True
  where
    sql = encodeUtf8 [trimming|
      WITH
      role_setting AS (
        SELECT setdatabase as database,
               unnest(setconfig) as setting
        FROM pg_catalog.pg_db_role_setting
        WHERE setrole = quote_ident(CURRENT_USER)::regrole::oid
          AND setdatabase IN (0, (SELECT oid FROM pg_catalog.pg_database WHERE datname = CURRENT_CATALOG))
      ),
      kv_settings AS (
        SELECT database,
               substr(setting, 1, strpos(setting, '=') - 1) as k,
               substr(setting, strpos(setting, '=') + 1) as v
        FROM role_setting
        ${preConfigF}
      )
      SELECT DISTINCT ON (key)
             replace(k, '${prefix}', '') AS key,
             v AS value
      FROM kv_settings
      WHERE k = ANY($$1) AND v IS NOT NULL
      ORDER BY key, database DESC NULLS LAST;
    |]
    preConfigF = case preConfFunc of
      Nothing   -> mempty
      Just func -> [trimming|
          UNION
          SELECT
            null as database,
            x as k,
            current_setting(x, true) as v
          FROM unnest($$1) x
          JOIN ${func}() _ ON TRUE
      |]::Text
    decodeSettings = HD.rowList $ (,) <$> column HD.text <*> column HD.text

queryRoleSettings :: Session RoleIsolationLvl
queryRoleSettings =
  SQL.transactionNoRetry SQL.ReadCommitted SQL.Read $ SQL.statement mempty $ SQL.Statement sql HE.noParams (processRows <$> rows) True
  where
    sql = encodeUtf8 [trimming|
      with
      role_setting as (
        select r.rolname, unnest(r.rolconfig) as setting
        from pg_auth_members m
        join pg_roles r on r.oid = m.roleid
        where member = quote_ident(current_user)::regrole::oid
      ),
      kv_settings AS (
        SELECT
          rolname,
          substr(setting, 1, strpos(setting, '=') - 1) as key,
          substr(setting, strpos(setting, '=') + 1) as value
        FROM role_setting
      ),
      iso_setting AS (
        SELECT rolname, value
        FROM kv_settings
        WHERE key = 'default_transaction_isolation'
      )
      select rolname, value
      from iso_setting;
    |]

    processRows :: [(Text, Text)] -> RoleIsolationLvl
    processRows =
      HM.fromList . fmap (bimap encodeUtf8 toIsolationLevel)

    rows :: HD.Result [(Text, Text)]
    rows = HD.rowList $ (,) <$> column HD.text <*> column HD.text

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable

arrayParam :: HE.Value a -> HE.Params [a]
arrayParam = param . HE.foldableArray . HE.nonNullable
