{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Auth where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import Control.Monad (mzero)
import Control.Applicative ( (<*>), (<$>) )
import Crypto.BCrypt
import qualified Hasql as H
import qualified Hasql.Postgres as H

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
  parseJSON _ = mzero

type DbRole = BS.ByteString

data LoginAttempt =
    NoCredentials
  | MalformedAuth
  | LoginFailed
  | LoginSuccess DbRole
  deriving (Eq, Show)

checkPass :: BS.ByteString -> BS.ByteString -> Bool
checkPass = validatePassword

setRole :: BS.ByteString -> H.Tx H.Postgres s ()
setRole role = H.unit $ [H.q| set role ?|] role

resetRole :: H.Tx H.Postgres s ()
resetRole = H.unit [H.q|reset role|]

addUser :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO(H.Tx H.Postgres s ())
addUser identity pass role = do
  Just hashed <- hashPasswordUsingPolicy fastBcryptHashingPolicy pass
  return $ H.unit $
    [H.q|insert into dbapi.auth (id, pass, rolname) values (?, ?, ?)|]
      identity hashed role

signInRole :: BS.ByteString -> BS.ByteString -> H.Tx H.Postgres s LoginAttempt
signInRole user pass = do
  u <- H.single $ [H.q|select pass, rolname from dbapi.auth where id = ?|] user
  return $ maybe LoginFailed (\r ->
      let (hashed, role) = r in
      if checkPass hashed pass
         then LoginSuccess role
         else LoginFailed
    ) u
