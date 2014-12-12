{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}
module Auth where

import Data.Aeson
import Control.Monad (mzero)
import Control.Applicative ( (<*>), (<$>) )
import Crypto.BCrypt
import Data.Text
import Data.Monoid
import qualified Hasql as H
import qualified Hasql.Postgres as H
import Data.String.Conversions (cs)
import PgQuery (pgFmtLit)

import System.IO.Unsafe

data AuthUser = AuthUser {
    userId :: String
  , userPass :: String
  , userRole :: String
  } deriving (Show)

instance FromJSON AuthUser where
  parseJSON (Object v) = AuthUser <$>
                         v .: "id" <*>
                         v .: "pass" <*>
                         v .: "role"
  parseJSON _ = mzero

instance ToJSON AuthUser where
  toJSON u = object [
      "id" .= userId u
    , "pass" .= userPass u
    , "role" .= userRole u ]

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
setRole role = H.unit ("set role " <> cs (pgFmtLit role), [], True)

resetRole :: H.Tx H.Postgres s ()
resetRole = H.unit [H.q|reset role|]

addUser :: Text -> Text -> Text -> H.Tx H.Postgres s ()
addUser identity pass role = do
  let Just hashed = unsafePerformIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (cs pass)
  H.unit $
    [H.q|insert into dbapi.auth (id, pass, rolname) values (?, ?, ?)|]
      identity (cs hashed :: Text) role

signInRole :: Text -> Text -> H.Tx H.Postgres s LoginAttempt
signInRole user pass = do
  u <- H.single $ [H.q|select pass, rolname from dbapi.auth where id = ?|] user
  return $ maybe LoginFailed (\r ->
      let (hashed, role) = r in
      if checkPass hashed pass
         then LoginSuccess role
         else LoginFailed
    ) u
