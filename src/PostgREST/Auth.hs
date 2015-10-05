module PostgREST.Auth where

import           Control.Applicative
import           Control.Monad           (mzero)
import           Crypto.BCrypt
import           Data.Aeson
import           Data.Map
import           Data.Monoid
import           Data.String.Conversions (cs)
import           Data.Text
import           Data.Maybe (isNothing)
import qualified Data.Vector             as V
import qualified Hasql                   as H
import qualified Hasql.Backend           as B
import qualified Hasql.Postgres          as P
import           PostgREST.PgQuery       (pgFmtLit)
import           Prelude
import qualified Web.JWT                 as JWT

import           System.IO.Unsafe

data AuthUser = AuthUser {
    userId   :: String
  , userPass :: String
  , userRole :: Maybe String
  } deriving (Show)

instance FromJSON AuthUser where
  parseJSON (Object v) = AuthUser <$>
                         v .: "id" <*>
                         v .: "pass" <*>
                         v .:? "role"
  parseJSON _ = mzero

instance ToJSON AuthUser where
  toJSON u = object [
      "id" .= userId u
    , "pass" .= userPass u
    , "role" .= userRole u ]

type DbRole = Text
type UserId = Text

data LoginAttempt =
    NoCredentials
  | MalformedAuth
  | LoginFailed
  | LoginSuccess DbRole UserId
  deriving (Eq, Show)

checkPass :: Text -> Text -> Bool
checkPass = (. cs) . validatePassword . cs

setRole :: Text -> H.Tx P.Postgres s ()
setRole role = H.unitEx $ B.Stmt ("set local role " <> cs (pgFmtLit role)) V.empty True

setUserId :: Text -> H.Tx P.Postgres s ()
setUserId uid =
  if uid /= ""
    then H.unitEx $ B.Stmt ("set local user_vars.user_id = " <> cs (pgFmtLit uid)) V.empty True
    else resetUserId

resetUserId :: H.Tx P.Postgres s ()
resetUserId = H.unitEx [H.stmt|reset user_vars.user_id|]

addUser :: Text -> Text -> Maybe Text -> H.Tx P.Postgres s ()
addUser identity pass role =
  H.unitEx $
    if isNothing role
      then [H.stmt|insert into postgrest.auth (id, pass) values (?, ?)|]
        identity hashedText
      else [H.stmt|insert into postgrest.auth (id, pass, rolname) values (?, ?, ?)|]
        identity hashedText role
  where Just hashed = unsafePerformIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (cs pass)
        hashedText = cs hashed :: Text

signInRole :: Text -> Text -> H.Tx P.Postgres s LoginAttempt
signInRole user pass = do
  u <- H.maybeEx $ [H.stmt|select id, pass, rolname from postgrest.auth where id = ?|] user
  return $ maybe LoginFailed (\r ->
      let (uid, hashed, role) = r in
      if checkPass hashed pass
         then LoginSuccess role uid
         else LoginFailed
    ) u

signInWithJWT :: Text -> Text -> LoginAttempt
signInWithJWT secret input = case maybeRole of
    Just (Just (String role)) -> case maybeUserId of
      Just (Just (String uid)) -> LoginSuccess (cs role) (cs uid)
      _   -> LoginFailed
    _   -> LoginFailed
  where
    maybeRole = (Data.Map.lookup "role" <$> claims) ::Maybe (Maybe Value)
    maybeUserId = (Data.Map.lookup "id" <$> claims) ::Maybe (Maybe Value)
    claims = JWT.unregisteredClaims <$> JWT.claims <$> decoded
    decoded = JWT.decodeAndVerifySignature (JWT.secret secret) input

tokenJWT :: Text -> Text -> Text -> Text
tokenJWT secret uid role = JWT.encodeSigned JWT.HS256 (JWT.secret secret) claimsSet
  where
    claimsSet = JWT.def {
      JWT.unregisteredClaims = Data.Map.fromList [("id", String uid), ("role", String role)]
    }
