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
    containsRole
  , jwtClaims
  , JWTAttempt(..)
  ) where

import           Protolude
import           Data.Aeson              (Value (..), toJSON)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as M
import           Data.Time.Clock         (NominalDiffTime)

import           Crypto.JOSE.Compact
import           Crypto.JOSE.JWK
import           Crypto.JWT

{-|
  Possible situations encountered with client JWTs
-}
data JWTAttempt = JWTInvalid JWTError
                | JWTMissingSecret
                | JWTClaims (M.HashMap Text Value)
                deriving Eq

{-|
  Receives the JWT secret (from config) and a JWT and returns a map
  of JWT claims.
-}
jwtClaims :: Maybe JWK -> BL.ByteString -> NominalDiffTime -> JWTAttempt
jwtClaims _ "" _ = JWTClaims M.empty
jwtClaims secret payload _time =
  case secret of
    Nothing -> JWTMissingSecret
    Just _ ->
      case jwtClaimsSet <$> decodeCompact payload of
        Left e -> JWTInvalid e
        Right claims -> JWTClaims $ claims2map claims

{-|
  Whether a response from jwtClaims contains a role claim
-}
containsRole :: JWTAttempt -> Bool
containsRole (JWTClaims claims) = M.member "role" claims
containsRole _ = False

{-|
  Internal helper used to turn JWT ClaimSet into something
  easier to work with
-}
claims2map :: ClaimsSet -> M.HashMap Text Value
claims2map = val2map . toJSON
 where
  val2map (Object o) = o
  val2map _ = M.empty
