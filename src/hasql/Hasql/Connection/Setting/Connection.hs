module Hasql.Connection.Setting.Connection
  ( Connection,
    string,
    params,
  )
where

import qualified Hasql.Connection.Config.ConnectionString        as Config.ConnectionString
import qualified Hasql.Connection.Config.ConnectionString.Params as Config.ConnectionString.Params
import qualified Hasql.Connection.Setting.Connection.Param       as Param
import           Hasql.Prelude

-- | Instructions on how to connect to the database.
newtype Connection = Connection ByteString

instance Config.ConnectionString.Constructs Connection where
  construct = coerce

-- | Preconstructed connection string according to <https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-CONNSTRING the PostgreSQL format>.
string :: Text -> Connection
string =
  Connection . Config.ConnectionString.fromText

-- | Structured parameters.
params :: [Param.Param] -> Connection
params =
  Connection . Config.ConnectionString.fromParams . Config.ConnectionString.Params.fromUpdates
