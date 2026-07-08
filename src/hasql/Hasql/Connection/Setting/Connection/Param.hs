module Hasql.Connection.Setting.Connection.Param
  ( Param,
    host,
    port,
    user,
    password,
    dbname,
    other,
  )
where

import qualified Data.ByteString.Builder                         as BB
import qualified Data.ByteString.Lazy                            as BL
import qualified Data.Text.Encoding                              as Text
import qualified Hasql.Connection.Config.ConnectionString.Params as Config
import           Hasql.Prelude

-- | Parameter of the connection instructions.
newtype Param = Param (Config.Params -> Config.Params)

instance Config.Updates Param where
  update = coerce

-- | Host domain name or IP-address.
host :: Text -> Param
host =
  Param . Config.setKeyValue "host" . Text.encodeUtf8

-- | Port number.
port :: Word16 -> Param
port =
  Param . Config.setKeyValue "port" . BL.toStrict . BB.toLazyByteString . BB.word16Dec

-- | User name.
user :: Text -> Param
user =
  Param . Config.setKeyValue "user" . Text.encodeUtf8

-- | Password.
password :: Text -> Param
password =
  Param . Config.setKeyValue "password" . Text.encodeUtf8

-- | Database name.
dbname :: Text -> Param
dbname =
  Param . Config.setKeyValue "dbname" . Text.encodeUtf8

-- | Any other parameter under the provided name according to <https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-CONNSTRING the PostgreSQL format>.
other :: Text -> Text -> Param
other name =
  Param . Config.setKeyValue (Text.encodeUtf8 name) . Text.encodeUtf8
