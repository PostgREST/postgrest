{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
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
  , parseSecret
  ) where

import qualified Crypto.JOSE.Types   as JOSE.Types
import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as M
import           Data.Vector         as V

import Control.Lens    (set)
import Data.Time.Clock (UTCTime)

import Control.Lens.Operators
import Crypto.JWT

import PostgREST.Types
import Protolude

{-|
  Possible situations encountered with client JWTs
-}
data JWTAttempt = JWTInvalid JWTError
                | JWTMissingSecret
                | JWTClaims (M.HashMap Text JSON.Value)
                deriving (Eq, Show)

{-|
  Receives the JWT secret and audience (from config) and a JWT and returns a map
  of JWT claims.
-}
jwtClaims :: Maybe JWKSet -> Maybe StringOrURI -> LByteString -> UTCTime -> Maybe JSPath -> IO JWTAttempt
jwtClaims _ _ "" _ _ = return $ JWTClaims M.empty
jwtClaims secret audience payload time jspath =
  case secret of
    Nothing -> return JWTMissingSecret
    Just s -> do
      let validation = set allowedSkew 1 $ defaultJWTValidationSettings (maybe (const True) (==) audience)
      eJwt <- runExceptT $ do
        jwt <- decodeCompact payload
        verifyClaimsAt validation s time jwt
      return $ case eJwt of
        Left e    -> JWTInvalid e
        Right jwt -> JWTClaims $ claims2map jwt jspath

{-|
  Turn JWT ClaimSet into something easier to work with,
  also here the jspath is applied to put the "role" in the map
-}
claims2map :: ClaimsSet -> Maybe JSPath -> M.HashMap Text JSON.Value
claims2map claims jspath = (\case
    val@(JSON.Object o) ->
      let role = maybe M.empty (M.singleton "role") $
                 walkJSPath (Just val) =<< jspath in
      M.delete "role" o `M.union` role -- mutating the map
    _ -> M.empty
  ) $ JSON.toJSON claims

walkJSPath :: Maybe JSON.Value -> JSPath -> Maybe JSON.Value
walkJSPath x                      []                = x
walkJSPath (Just (JSON.Object o)) (JSPKey key:rest) = walkJSPath (M.lookup key o) rest
walkJSPath (Just (JSON.Array ar)) (JSPIdx idx:rest) = walkJSPath (ar V.!? idx) rest
walkJSPath _                      _                 = Nothing

{-|
  Whether a response from jwtClaims contains a role claim
-}
containsRole :: JWTAttempt -> Bool
containsRole (JWTClaims claims) = M.member "role" claims
containsRole _                  = False

{-|
  Parse `jwt-secret` configuration option and turn into a JWKSet.

  There are three ways to specify `jwt-secret`: text secret, JSON Web Key
  (JWK), or JSON Web Key Set (JWKS). The first two are converted into a JWKSet
  with one key and the last is converted as is.
-}
parseSecret :: ByteString -> JWKSet
parseSecret str =
  fromMaybe (maybe secret (\jwk' -> JWKSet [jwk']) maybeJWK)
    maybeJWKSet
 where
  maybeJWKSet = JSON.decode (toS str) :: Maybe JWKSet
  maybeJWK = JSON.decode (toS str) :: Maybe JWK
  secret = JWKSet [jwkFromSecret str]

{-|
  Internal helper to generate a symmetric HMAC-SHA256 JWK from a text secret.
-}
jwkFromSecret :: ByteString -> JWK
jwkFromSecret key =
  fromKeyMaterial km
    & jwkUse ?~ Sig
    & jwkAlg ?~ JWSAlg HS256
 where
  km = OctKeyMaterial (OctKeyParameters (JOSE.Types.Base64Octets key))
