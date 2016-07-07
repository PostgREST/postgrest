{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : PostgREST.Auth
Description : PostgREST authorization functions.

This module provides functions to deal with the JWT authorization (http://jwt.io).
It also can be used to define other authorization functions,
in the future Oauth, LDAP and similar integrations can be coded here.

Authentication should always be implemented in an external service.
In the test suite there is an example of simple login function that can be used for a
very simple authentication system inside the PostgreSQL database.
-}
module PostgREST.Auth (
    claimsToSQL
  , containsRole
  , jwtClaims
  , tokenJWT
  , mixedClaimsTokenToJWT
  ) where

import           Protolude
import           Control.Lens
import           Data.Aeson              (Value (..), parseJSON, toJSON)
import           Data.Aeson.Lens
import           Data.Aeson.Types        (parseMaybe, emptyObject)
import qualified Data.Vector             as V
import qualified Data.HashMap.Strict     as M
import           Data.Maybe              (fromJust)
import           Data.String.Conversions (cs)
import           Data.Time.Clock         (NominalDiffTime)
import           PostgREST.QueryBuilder  (pgFmtIdent, pgFmtLit, unquoted)
import qualified Web.JWT                 as JWT

{-|
  Receives a map of JWT claims and returns a list of PostgreSQL
  statements to set the claims as user defined GUCs.  Except if we
  have a claim called role, this one is mapped to a SET ROLE
  statement.
-}
claimsToSQL :: M.HashMap Text Value -> [ByteString]
claimsToSQL claims = roleStmts <> varStmts
 where
  roleStmts = maybeToList $
    (\r -> "set local role " <> r <> ";") . cs . valueToVariable <$> M.lookup "role" claims
  varStmts = map setVar $ M.toList (M.delete "role" claims)
  setVar (k, val) = "set local " <> cs (pgFmtIdent $ "postgrest.claims." <> k)
                    <> " = " <> cs (valueToVariable val) <> ";"
  valueToVariable = pgFmtLit . unquoted

{-|
  Receives the JWT secret (from config) and a JWT and
  returns a map of JWT claims
  In case there is any problem decoding the JWT it returns an error Text
-}
jwtClaims :: JWT.Secret -> Text -> NominalDiffTime -> Either Text (M.HashMap Text Value)
jwtClaims _ "" _ = Right M.empty
jwtClaims secret jwt time =
  case isExpired <$> mClaims of
    Just True -> Left "JWT expired"
    Nothing -> Left "Invalid JWT"
    Just False -> Right $ value2map $ fromJust mClaims
 where
  isExpired claims =
    let mExp = claims ^? key "exp" . _Integer
    in fromMaybe False $ (<= time) . fromInteger <$> mExp
  mClaims = toJSON . JWT.claims <$> JWT.decodeAndVerifySignature secret jwt
  value2map (Object o) = o
  value2map _          = M.empty

{-|
  Receives the JWT secret (from config) and a JWT and a JSON value
  and returns a signed JWT.
-}
tokenJWT :: JWT.Secret -> Value -> Text
tokenJWT secret (Array arr) =
  let obj = if V.null arr then emptyObject else V.head arr in
    tokenJWT secret obj
tokenJWT secret (Object o) =
  let
    jcs = parseMaybe parseJSON (Object o) :: Maybe JWT.JWTClaimsSet in
    JWT.encodeSigned JWT.HS256 secret $ fromMaybe JWT.def jcs
tokenJWT secret _ = tokenJWT secret emptyObject


{-|
  Whether a response from jwtClaims contains a role claim
-}
containsRole :: Either Text (M.HashMap Text Value) -> Bool
containsRole (Left _) = False
containsRole (Right claims) = M.member "role" claims



{-|
  for mixed claimes
-}
encryptJsonValueFunc :: JWT.Secret -> (Value -> Value)
encryptJsonValueFunc x = \y -> String (tokenJWT x y)


mixedClaimsTokenToJWT :: [Text] -> Value -> JWT.Secret -> Value
mixedClaimsTokenToJWT t (Array arr) s    =
  let obj = if V.null arr then emptyObject else V.head arr in
    mixedClaimsTokenToJWT t obj s
mixedClaimsTokenToJWT [] (Object o) _    = Object o
mixedClaimsTokenToJWT (x:xs) (Object o) s     = mixedClaimsTokenToJWT xs
                                                (Object $ M.adjust (encryptJsonValueFunc s) x o)
                                                 s
mixedClaimsTokenToJWT _ v _ = v
