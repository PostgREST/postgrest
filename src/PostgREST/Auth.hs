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
  , parseJWK
  ) where

import           Protolude        hiding ((&))
import           Control.Lens
import           Data.Aeson              (Value (..), decode, toJSON)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as M

import           Crypto.JOSE.Compact
import           Crypto.JOSE.JWK
import           Crypto.JOSE.JWS
import           Crypto.JOSE.Types
import           Crypto.JWT

{-|
  Possible situations encountered with client JWTs
-}
data JWTAttempt = JWTInvalid JWTError
                | JWTMissingSecret
                | JWTClaims (M.HashMap Text Value)
                deriving (Eq, Show)

{-|
  Receives the JWT secret (from config) and a JWT and returns a map
  of JWT claims.
-}
jwtClaims :: Maybe JWK -> BL.ByteString -> IO JWTAttempt
jwtClaims _ "" = return $ JWTClaims M.empty
jwtClaims secret payload =
  case secret of
    Nothing -> return JWTMissingSecret
    Just jwk -> do
      let validation = defaultJWTValidationSettings
      eJwt <- runExceptT $ do
        jwt <- decodeCompact payload
        validateJWSJWT validation jwk jwt
        return jwt
      return $ case eJwt of
        Left e -> JWTInvalid e
        Right jwt -> JWTClaims . claims2map . jwtClaimsSet $ jwt

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

parseJWK :: ByteString -> JWK
parseJWK str =
  fromMaybe (hs256jwk str) (decode (toS str) :: Maybe JWK)

{-|
  Internal helper to generate HMAC-SHA256. When the jwt key in the
  config file is a simple string rather than a JWK object, we'll
  apply this function to it.
-}
hs256jwk :: ByteString -> JWK
hs256jwk key =
  fromKeyMaterial km
    & jwkUse .~ Just Sig
    & jwkAlg .~ (Just $ JWSAlg HS256)
 where
  km = OctKeyMaterial (OctKeyParameters Oct (Base64Octets key))
