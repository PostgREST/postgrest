{-# LANGUAGE QuasiQuotes #-}

module PostgREST.Config.Database
  ( queryDbSettings
  , queryPgVersion
  ) where

import PostgREST.Config.PgVersion (PgVersion (..))

import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as P
import qualified Hasql.Session              as H
import qualified Hasql.Statement            as H
import qualified Hasql.Transaction          as HT
import qualified Hasql.Transaction.Sessions as HT

import Text.InterpolatedString.Perl6 (q)

import Protolude

queryPgVersion :: H.Session PgVersion
queryPgVersion = H.statement mempty $ H.Statement sql HE.noParams versionRow False
  where
    sql = "SELECT current_setting('server_version_num')::integer, current_setting('server_version')"
    versionRow = HD.singleRow $ PgVersion <$> column HD.int4 <*> column HD.text

queryDbSettings :: P.Pool -> Bool -> IO (Either P.UsageError [(Text, Text)])
queryDbSettings pool prepared =
  let transaction = if prepared then HT.transaction else HT.unpreparedTransaction in
  P.use pool . transaction HT.ReadCommitted HT.Read $
    HT.statement mempty dbSettingsStatement

-- | Get db settings from the connection role. Global settings will be overridden by database specific settings.
dbSettingsStatement :: H.Statement () [(Text, Text)]
dbSettingsStatement = H.Statement sql HE.noParams decodeSettings False
  where
    sql = [q|
      with
      role_setting as (
        select setdatabase, unnest(setconfig) as setting from pg_catalog.pg_db_role_setting
        where setrole = current_user::regrole::oid
          and setdatabase in (0, (select oid from pg_catalog.pg_database where datname = current_catalog))
      ),
      kv_settings as (
        select setdatabase, split_part(setting, '=', 1) as k, split_part(setting, '=', 2) as value from role_setting
        where setting like 'pgrst.%'
      )
      select distinct on (key) replace(k, 'pgrst.', '') as key, value
      from kv_settings
      order by key, setdatabase desc;
    |]
    decodeSettings = HD.rowList $ (,) <$> column HD.text <*> column HD.text

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable
