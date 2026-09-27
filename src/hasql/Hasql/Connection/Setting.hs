module Hasql.Connection.Setting
  ( Setting
  , connection
  , usePreparedStatements
  )
where

import Data.Text.Encoding qualified

import Hasql.Prelude

import Hasql.Connection.Config qualified as Config

-- | Setting of a client handle.
newtype Setting = Setting (Config.Config -> Config.Config)

instance Config.Updates Setting where
  update (Setting update) = update

-- | Connection details like address of the remote service and authentication info.
connection :: Text -> Setting
connection =
  Setting . Config.setConnectionString . Data.Text.Encoding.encodeUtf8

-- | Whether prepared statements are allowed.
--
-- When 'False', even the statements marked as preparable will be executed without preparation.
--
-- This is useful when dealing with proxying applications like @pgbouncer@, which may be incompatible with prepared statements.
-- Consult their docs or just set it to 'False' to stay on the safe side.
-- It should be noted that starting from version @1.21.0@ @pgbouncer@ now does provide support for prepared statements.
--
-- 'True' by default.
usePreparedStatements :: Bool -> Setting
usePreparedStatements =
  Setting . Config.setUsePreparedStatements
