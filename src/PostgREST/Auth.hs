{-|
Module      : PostgREST.Auth
Description : PostgREST authentication functions.

This module provides functions to deal with the JWT authentication (http://jwt.io).
It also can be used to define other authentication functions,
in the future Oauth, LDAP and similar integrations can be coded here.

Authentication should always be implemented in an external service.
In the test suite there is an example of simple login function that can be used for a
very simple authentication system inside the PostgreSQL database.
-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Auth
  ( getResult
  , getJwtDur
  , getRole
  , middleware
  ) where

import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Key                  as K
import qualified Data.Aeson.KeyMap               as KM
import qualified Data.Aeson.Types                as JSON
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import qualified Data.Scientific                 as Sci
import qualified Data.Text                       as T
import qualified Data.Vault.Lazy                 as Vault
import qualified Data.Vector                     as V
import qualified Jose.Jwk                        as JWT
import qualified Jose.Jwt                        as JWT
import qualified Network.HTTP.Types.Header       as HTTP
import qualified Network.Wai                     as Wai
import qualified Network.Wai.Middleware.HttpAuth as Wai

import Control.Monad.Except    (liftEither)
import Data.Either.Combinators (mapLeft)
import Data.List               (lookup)
import Data.Time.Clock         (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX   (utcTimeToPOSIXSeconds)
import System.IO.Unsafe        (unsafePerformIO)
import System.TimeIt           (timeItT)

import PostgREST.AppState      (AppState, getConfig, getJwtCacheState,
                                getTime)
import PostgREST.Auth.JwtCache (lookupJwtCache)
import PostgREST.Auth.Types    (AuthResult (..))
import PostgREST.Config        (AppConfig (..), FilterExp (..),
                                JSPath, JSPathExp (..))
import PostgREST.Error         (Error (..))

import Protolude

-- | Receives the JWT secret and audience (from config) and a JWT and returns a
-- JSON object of JWT claims.
parseToken :: AppConfig -> ByteString -> UTCTime -> ExceptT Error IO JSON.Value
parseToken _ "" _ = return JSON.emptyObject
parseToken AppConfig{..} token time = do
  secret <- liftEither . maybeToRight JwtTokenMissing $ configJWKS
  eitherContent <- liftIO $ JWT.decode (JWT.keys secret) Nothing token
  content <- liftEither . mapLeft jwtDecodeError $ eitherContent
  liftEither $ verifyClaims content
  where
      -- TODO: Improve errors, those were just taken as-is from hs-jose to avoid
      -- breaking changes.
      jwtDecodeError :: JWT.JwtError -> Error
      jwtDecodeError (JWT.KeyError _)     = JwtTokenInvalid "JWSError JWSInvalidSignature"
      jwtDecodeError JWT.BadCrypto        = JwtTokenInvalid "JWSError (CompactDecodeError Invalid number of parts: Expected 3 parts; got 2)"
      jwtDecodeError (JWT.BadAlgorithm _) = JwtTokenInvalid "JWSError JWSNoSignatures"
      jwtDecodeError e                    = JwtTokenInvalid $ show e

      verifyClaims :: JWT.JwtContent -> Either Error JSON.Value
      verifyClaims (JWT.Jws (_, claims)) = case JSON.decodeStrict claims of
        Nothing                    -> Left $ JwtTokenInvalid "Parsing claims failed"
        Just (JSON.Object mclaims)
          | failedExpClaim mclaims -> Left $ JwtTokenInvalid "JWT expired"
          | failedNbfClaim mclaims -> Left $ JwtTokenInvalid "JWTNotYetValid"
          | failedIatClaim mclaims -> Left $ JwtTokenInvalid "JWTIssuedAtFuture"
          | failedAudClaim mclaims -> Left $ JwtTokenInvalid "JWTNotInAudience"
        Just jclaims               -> Right jclaims
      -- TODO: We could enable JWE support here (encrypted tokens)
      verifyClaims _                = Left $ JwtTokenInvalid "Unsupported token type"

      allowedSkewSeconds = 30 :: Int64
      now = floor . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds time
      sciToInt = fromMaybe 0 . Sci.toBoundedInteger

      failedExpClaim :: KM.KeyMap JSON.Value -> Bool
      failedExpClaim mclaims = case KM.lookup "exp" mclaims of
        Just (JSON.Number secs) -> now > (sciToInt secs + allowedSkewSeconds)
        _                       -> False

      failedNbfClaim :: KM.KeyMap JSON.Value -> Bool
      failedNbfClaim mclaims = case KM.lookup "nbf" mclaims of
        Just (JSON.Number secs) -> now < (sciToInt secs - allowedSkewSeconds)
        _                       -> False

      failedIatClaim :: KM.KeyMap JSON.Value -> Bool
      failedIatClaim mclaims = case KM.lookup "iat" mclaims of
        Just (JSON.Number secs) -> now < (sciToInt secs - allowedSkewSeconds)
        _                       -> False

      failedAudClaim :: KM.KeyMap JSON.Value -> Bool
      failedAudClaim mclaims = case KM.lookup "aud" mclaims of
        Just (JSON.String str) -> maybe (const False) (/=) configJwtAudience str
        _                      -> False

parseClaims :: Monad m =>
  AppConfig -> JSON.Value -> ExceptT Error m AuthResult
parseClaims AppConfig{..} jclaims@(JSON.Object mclaims) = do
  -- role defaults to anon if not specified in jwt
  role <- liftEither . maybeToRight JwtTokenRequired $
    unquoted <$> walkJSPath (Just jclaims) configJwtRoleClaimKey <|> configDbAnonRole
  return AuthResult
           { authClaims = mclaims & KM.insert "role" (JSON.toJSON $ decodeUtf8 role)
           , authRole = role
           }
  where
    walkJSPath :: Maybe JSON.Value -> JSPath -> Maybe JSON.Value
    walkJSPath x                      []                = x
    walkJSPath (Just (JSON.Object o)) (JSPKey key:rest) = walkJSPath (KM.lookup (K.fromText key) o) rest
    walkJSPath (Just (JSON.Array ar)) (JSPIdx idx:rest) = walkJSPath (ar V.!? idx) rest
    walkJSPath (Just (JSON.Array ar)) [JSPFilter (EqualsCond txt)] = findFirstMatch (==) txt ar
    walkJSPath (Just (JSON.Array ar)) [JSPFilter (NotEqualsCond txt)] = findFirstMatch (/=) txt ar
    walkJSPath (Just (JSON.Array ar)) [JSPFilter (StartsWithCond txt)] = findFirstMatch T.isPrefixOf txt ar
    walkJSPath (Just (JSON.Array ar)) [JSPFilter (EndsWithCond txt)] = findFirstMatch T.isSuffixOf txt ar
    walkJSPath (Just (JSON.Array ar)) [JSPFilter (ContainsCond txt)] = findFirstMatch T.isInfixOf txt ar
    walkJSPath _                      _                 = Nothing

    findFirstMatch matchWith pattern = foldr checkMatch Nothing
      where
        checkMatch (JSON.String txt) acc
            | pattern `matchWith` txt = Just $ JSON.String txt
            | otherwise = acc
        checkMatch _ acc = acc

    unquoted :: JSON.Value -> BS.ByteString
    unquoted (JSON.String t) = encodeUtf8 t
    unquoted v               = LBS.toStrict $ JSON.encode v
-- impossible case - just added to please -Wincomplete-patterns
parseClaims _ _ = return AuthResult { authClaims = KM.empty, authRole = mempty }

-- | Validate authorization header.
--   Parse and store JWT claims for future use in the request.
middleware :: AppState -> Wai.Middleware
middleware appState app req respond = do
  conf <- getConfig appState
  time <- getTime appState

  let token  = fromMaybe "" $ Wai.extractBearerAuth =<< lookup HTTP.hAuthorization (Wai.requestHeaders req)
      parseJwt = runExceptT $ parseToken conf token time >>= parseClaims conf
      jwtCacheState = getJwtCacheState appState

-- If ServerTimingEnabled -> calculate JWT validation time
-- If JwtCacheMaxLifetime -> cache JWT validation result
  req' <- case (configServerTimingEnabled conf, configJwtCacheMaxLifetime conf) of
    (True, 0)            -> do
          (dur, authResult) <- timeItT parseJwt
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult & Vault.insert jwtDurKey dur }

    (True, maxLifetime)  -> do
          (dur, authResult) <- timeItT $ lookupJwtCache jwtCacheState token maxLifetime parseJwt time
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult & Vault.insert jwtDurKey dur }

    (False, 0)           -> do
          authResult <- parseJwt
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult }

    (False, maxLifetime) -> do
          authResult <- lookupJwtCache jwtCacheState token maxLifetime parseJwt time
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult }

  app req' respond

authResultKey :: Vault.Key (Either Error AuthResult)
authResultKey = unsafePerformIO Vault.newKey
{-# NOINLINE authResultKey #-}

getResult :: Wai.Request -> Maybe (Either Error AuthResult)
getResult = Vault.lookup authResultKey . Wai.vault

jwtDurKey :: Vault.Key Double
jwtDurKey = unsafePerformIO Vault.newKey
{-# NOINLINE jwtDurKey #-}

getJwtDur :: Wai.Request -> Maybe Double
getJwtDur =  Vault.lookup jwtDurKey . Wai.vault

getRole :: Wai.Request -> Maybe BS.ByteString
getRole req = authRole <$> (rightToMaybe =<< getResult req)
