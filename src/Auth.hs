import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import Crypto.BCrypt
import Control.Monad (mzero)
import Control.Applicative ( (<$>), (<*>) )
import Database.PostgreSQL.Simple

data AuthUser = AuthUser {
    userId :: String
  , userPass :: String
  , userRole :: String
  }

instance JSON.FromJSON AuthUser where
  parseJSON (JSON.Object v) = AuthUser <$>
                         v JSON..: "id" <*>
                         v JSON..: "pass" <*>
                         v JSON..: "role"
  parseJSON _          = mzero

type DbRole = BS.ByteString

data LoginAttempt =
    NoCredentials
  | MalformedAuth
  | LoginFailed
  | LoginSuccess DbRole
  deriving (Eq, Show)

checkPass :: BS.ByteString -> BS.ByteString -> Bool
checkPass = validatePassword

setRole :: Connection -> DbRole -> IO ()
setRole conn role = execute conn "set role ?" (Only role)

resetRole :: Connection -> IO ()
resetRole = flip execute_ "reset role"
