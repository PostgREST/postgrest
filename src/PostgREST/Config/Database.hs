{-# LANGUAGE QuasiQuotes #-}

module PostgREST.Config.Database
  ( pgVersionStatement
  , queryDbSettings
  , queryPgVersion
  ) where

import PostgREST.Config.PgVersion (PgVersion (..))

import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import           Hasql.Session              (Session, statement)
import qualified Hasql.Statement            as SQL
import qualified Hasql.Transaction          as SQL
import qualified Hasql.Transaction.Sessions as SQL

import Text.InterpolatedString.Perl6 (q)

import Protolude

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

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable
