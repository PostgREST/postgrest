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
  , JWTAttempt(..)
  ) where

import           Protolude
import           Control.Lens
import           Data.Aeson              (Value (..), parseJSON, toJSON)
import           Data.Aeson.Lens
import           Data.Aeson.Types        (parseMaybe, emptyObject, emptyArray)
import qualified Data.Vector             as V
import qualified Data.HashMap.Strict     as M
import           Data.Maybe              (fromJust)
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
    (\r -> "set local role " <> r <> ";") . toS . valueToVariable <$> M.lookup "role" claims
  varStmts = map setVar $ M.toList (M.delete "role" claims)
  setVar (k, val) = "set local " <> toS (pgFmtIdent $ "request.jwt.claim." <> k)
                    <> " = " <> toS (valueToVariable val) <> ";"
  valueToVariable = pgFmtLit . unquoted

{-|
  Possible situations encountered with client JWTs
-}
data JWTAttempt = JWTExpired
                | JWTInvalid
                | JWTMissingSecret
                | JWTClaims (M.HashMap Text Value)
                deriving Eq

{-|
  Receives the JWT secret (from config) and a JWT and returns a map
  of JWT claims.
-}
jwtClaims :: Maybe JWT.Secret -> Text -> NominalDiffTime -> JWTAttempt
jwtClaims _ "" _ = JWTClaims M.empty
jwtClaims secret jwt time =
  case secret of
    Nothing -> JWTMissingSecret
    Just s ->
      let mClaims = toJSON . JWT.claims <$> JWT.decodeAndVerifySignature s jwt in
      case isExpired <$> mClaims of
        Just True -> JWTExpired
        Nothing -> JWTInvalid
        Just False -> JWTClaims $ value2map $ fromJust mClaims
 where
  isExpired claims =
    let mExp = claims ^? key "exp" . _Integer
    in fromMaybe False $ (<= time) . fromInteger <$> mExp
  value2map (Object o) = o
  value2map _          = M.empty

{-|
  Receives the JWT secret (from config) and a JWT and a JSON value
  and returns a signed JWT.
-}
tokenJWT :: JWT.Secret -> Value -> Text
tokenJWT secret (Array arr) =
  let obj = if V.null arr then emptyObject else V.head arr
      jcs = parseMaybe parseJSON obj :: Maybe JWT.JWTClaimsSet in
  JWT.encodeSigned JWT.HS256 secret $ fromMaybe JWT.def jcs
tokenJWT secret _ = tokenJWT secret emptyArray

{-|
  Whether a response from jwtClaims contains a role claim
-}
containsRole :: JWTAttempt -> Bool
containsRole (JWTClaims claims) = M.member "role" claims
containsRole _ = False
