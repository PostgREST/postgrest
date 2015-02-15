module Auth where

import Data.Aeson
import Control.Monad (mzero)
import Control.Applicative ( (<*>), (<$>) )
import Crypto.BCrypt
import Data.Text
import Data.Monoid
import qualified Data.Vector as V
import qualified Hasql as H
import qualified Hasql.Backend as B
import qualified Hasql.Postgres as P
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

setRole :: Text -> H.Tx P.Postgres s ()
setRole role = H.unitEx $ B.Stmt ("set role " <> cs (pgFmtLit role)) V.empty True

resetRole :: H.Tx P.Postgres s ()
resetRole = H.unitEx [H.stmt|reset role|]

addUser :: Text -> Text -> Text -> H.Tx P.Postgres s ()
addUser identity pass role = do
  let Just hashed = unsafePerformIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (cs pass)
  H.unitEx $
    [H.stmt|insert into postgrest.auth (id, pass, rolname) values (?, ?, ?)|]
      identity (cs hashed :: Text) role

signInRole :: Text -> Text -> H.Tx P.Postgres s LoginAttempt
signInRole user pass = do
  u <- H.maybeEx $ [H.stmt|select pass, rolname from postgrest.auth where id = ?|] user
  return $ maybe LoginFailed (\r ->
      let (hashed, role) = r in
      if checkPass hashed pass
         then LoginSuccess role
         else LoginFailed
    ) u
