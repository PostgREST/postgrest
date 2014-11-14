module Auth where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import Control.Monad (mzero)
import Control.Applicative ( (<*>), (<$>) )
import Crypto.BCrypt
import qualified Hasql as H
import qualified Hasql.Postgres as H
import GHC.Int

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

setRole :: Connection -> DbRole -> IO Int64
setRole conn role = execute conn "set role ?" (Only role)

resetRole :: Connection -> IO Int64
resetRole = flip execute_ "reset role"

addUser :: Connection -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO Int64
addUser c identity pass role = do
  Just hashed <- hashPasswordUsingPolicy fastBcryptHashingPolicy pass
  execute c
    "insert into dbapi.auth (id, pass, rolname) values (?, ?, ?)"
    (identity, hashed, role)

signInRole :: Connection -> BS.ByteString -> BS.ByteString -> IO LoginAttempt
signInRole c user pass = do
  u <- query c "select pass, rolname from dbapi.auth where id = ?" $ Only user
  return $ case u of
    [[hashed, role]] ->
      if checkPass hashed pass
         then LoginSuccess role
         else LoginFailed
    _ -> LoginFailed
