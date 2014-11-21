{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Auth where

import qualified Data.Aeson as JSON
import Control.Monad (mzero)
import Control.Applicative ( (<*>), (<$>) )
import Crypto.BCrypt
import Data.Text
import qualified Hasql as H
import qualified Hasql.Postgres as H
import Data.String.Conversions (cs)

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

type DbRole = Text

data LoginAttempt =
    NoCredentials
  | MalformedAuth
  | LoginFailed
  | LoginSuccess DbRole
  deriving (Eq, Show)

checkPass :: Text -> Text -> Bool
checkPass = (. cs) . validatePassword . cs

setRole :: Text -> H.Tx H.Postgres s ()
setRole role = H.unit $ [H.q| set role ?|] role

resetRole :: H.Tx H.Postgres s ()
resetRole = H.unit [H.q|reset role|]

addUser :: Text -> Text -> Text -> IO(H.Tx H.Postgres s ())
addUser identity pass role = do
  Just hashed <- hashPasswordUsingPolicy fastBcryptHashingPolicy (cs pass)
  return $ H.unit $
    [H.q|insert into dbapi.auth (id, pass, rolname) values (?, ?, ?)|]
      identity hashed role

signInRole :: Text -> Text -> H.Tx H.Postgres s LoginAttempt
signInRole user pass = do
  u <- H.single $ [H.q|select pass, rolname from dbapi.auth where id = ?|] user
  return $ maybe LoginFailed (\r ->
      let (hashed, role) = r in
      if checkPass hashed pass
         then LoginSuccess role
         else LoginFailed
    ) u
