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

import Text.InterpolatedString.Perl6 (q)

import Protolude

type RoleSettings = (HM.HashMap ByteString (HM.HashMap ByteString ByteString))

queryPgVersion :: Bool -> Session PgVersion
queryPgVersion prepared = statement mempty $ pgVersionStatement prepared

pgVersionStatement :: Bool -> SQL.Statement () PgVersion
pgVersionStatement = SQL.Statement sql HE.noParams versionRow
  where
    sql = "SELECT current_setting('server_version_num')::integer, current_setting('server_version')"
    versionRow = HD.singleRow $ PgVersion <$> column HD.int4 <*> column HD.text

queryDbSettings :: Bool -> Session [(Text, Text)]
queryDbSettings prepared =
  let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction in
  transaction SQL.ReadCommitted SQL.Read $ SQL.statement mempty $ dbSettingsStatement prepared

-- | Get db settings from the connection role. Global settings will be overridden by database specific settings.
dbSettingsStatement :: Bool -> SQL.Statement () [(Text, Text)]
dbSettingsStatement = SQL.Statement sql HE.noParams decodeSettings
  where
    sql = [q|
      WITH
      role_setting (database, setting) AS (
        SELECT setdatabase,
               unnest(setconfig)
          FROM pg_catalog.pg_db_role_setting
         WHERE setrole = CURRENT_USER::regrole::oid
           AND setdatabase IN (0, (SELECT oid FROM pg_catalog.pg_database WHERE datname = CURRENT_CATALOG))
      ),
      kv_settings (database, k, v) AS (
        SELECT database,
               substr(setting, 1, strpos(setting, '=') - 1),
               substr(setting, strpos(setting, '=') + 1)
          FROM role_setting
         WHERE setting LIKE 'pgrst.%'
      )
      SELECT DISTINCT ON (key)
             replace(k, 'pgrst.', '') AS key,
             v AS value
        FROM kv_settings
       ORDER BY key, database DESC;
    |]
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
