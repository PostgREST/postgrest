{-# LANGUAGE FlexibleContexts #-}
module PostgREST.Auth where

import           Control.Applicative
import           Control.Monad           (mzero)

import           Data.Aeson
import           Data.Map (lookup, fromList, toList)
import           Data.Monoid
import           Data.String.Conversions (cs)
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Vector             as V
import qualified Hasql                   as H
import qualified Hasql.Backend           as B
import qualified Hasql.Postgres          as P
import           PostgREST.PgQuery       (pgFmtLit)
import           Prelude 
import qualified Web.JWT                 as JWT



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

setJWTEnv :: Text -> Text -> Maybe [Text]
setJWTEnv secret input = setDBEnv $ jwtClaims secret input

setDBEnv :: Maybe JWT.ClaimsMap -> Maybe [Text]
setDBEnv maybeClaims =
  (map setVar . toList) <$> maybeClaims
  where
    setVar ("role", String val) = setRole val
    setVar (key, String val) = "set local postgrest." <> key <> " = " <> cs (pgFmtLit val) <> ";"

setRole role = "set local role " <> cs (pgFmtLit role) <> ";"

jwtClaims :: Text -> Text -> Maybe JWT.ClaimsMap
jwtClaims secret input = claims
  where
    claims = JWT.unregisteredClaims <$> JWT.claims <$> decoded
    decoded = JWT.decodeAndVerifySignature (JWT.secret secret) input

tokenJWT :: Text -> Text -> Text -> Text
tokenJWT secret uid role = JWT.encodeSigned JWT.HS256 (JWT.secret secret) claimsSet
  where
    claimsSet = JWT.def {
      JWT.unregisteredClaims = Data.Map.fromList [("id", String uid), ("role", String role)]
    }
