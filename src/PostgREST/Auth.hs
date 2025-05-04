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
  ( getAuthResult )
  where

import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Key                  as K
import qualified Data.Aeson.KeyMap               as KM
import qualified Data.Aeson.Types                as JSON
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Internal        as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import qualified Data.CaseInsensitive            as CI
import qualified Data.Scientific                 as Sci
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Jose.Jwk                        as JWT
import qualified Jose.Jwt                        as JWT
import qualified Network.HTTP.Types.Header       as HTTP
import qualified Network.Wai.Middleware.HttpAuth as Wai

import Control.Monad.Except    (liftEither)
import Data.Either.Combinators (mapLeft)
import Data.List               (lookup)
import Data.Time.Clock         (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX   (utcTimeToPOSIXSeconds)

import PostgREST.ApiRequest    (ApiRequest (..))
import PostgREST.AppState      (AppState, getConfig, getJwtCacheState,
                                getTime)
import PostgREST.Auth.JwtCache (lookupJwtCache)
import PostgREST.Auth.Types    (AuthResult (..))
import PostgREST.Config        (AppConfig (..), FilterExp (..),
                                JSPath, JSPathExp (..))
import PostgREST.Error         (Error (..), JwtError (..))

import Protolude

-- | Receives the JWT secret and audience (from config) and a JWT and returns a
-- JSON object of JWT claims.
parseToken :: AppConfig -> Maybe ByteString -> UTCTime -> ExceptT Error IO JSON.Value
parseToken _ Nothing _ = return JSON.emptyObject
parseToken _ (Just "") _ = throwE . JwtErr $ JwtDecodeError "Empty JWT is sent in Authorization header"
parseToken AppConfig{..} (Just tkn) time = do
  secret <- liftEither . maybeToRight (JwtErr JwtSecretMissing) $ configJWKS
  tknWith3Parts <- liftEither $ hasThreeParts tkn
  eitherContent <- liftIO $ JWT.decode (JWT.keys secret) Nothing tknWith3Parts
  content <- liftEither . mapLeft (JwtErr . jwtDecodeError) $ eitherContent
  liftEither $ mapLeft JwtErr $ verifyClaims content
  where
      hasThreeParts :: ByteString -> Either Error ByteString
      hasThreeParts token = case length $ BS.split (BS.c2w '.') token of
        3 -> Right token
        n -> Left $ JwtErr $ JwtDecodeError ("Expected 3 parts in JWT; got " <> show n)
      jwtDecodeError :: JWT.JwtError -> JwtError
      -- The only errors we can get from JWT.decode function are:
      --   BadAlgorithm
      --   KeyError
      --   BadCrypto
      jwtDecodeError (JWT.KeyError _)     = JwtDecodeError "No suitable key or wrong key type"
      jwtDecodeError (JWT.BadAlgorithm _) = JwtDecodeError "Wrong or unsupported encoding algorithm"
      jwtDecodeError JWT.BadCrypto        = JwtDecodeError "JWT cryptographic operation failed"
      -- Control never reaches here, the decode function only returns the above three
      jwtDecodeError _                    = JwtDecodeError "JWT couldn't be decoded"

      verifyClaims :: JWT.JwtContent -> Either JwtError JSON.Value
      verifyClaims (JWT.Jws (_, claims)) = case JSON.decodeStrict claims of
        Nothing                    -> Left $ JwtClaimsError "Parsing claims failed"
        Just (JSON.Object mclaims)
          | failedExpClaim mclaims -> Left $ JwtClaimsError "JWT expired"
          | failedNbfClaim mclaims -> Left $ JwtClaimsError "JWT not yet valid"
          | failedIatClaim mclaims -> Left $ JwtClaimsError "JWT issued at future"
          | failedAudClaim mclaims -> Left $ JwtClaimsError "JWT not in audience"
        Just jclaims               -> Right jclaims
      -- TODO: We could enable JWE support here (encrypted tokens)
      verifyClaims _                = Left $ JwtDecodeError "Unsupported token type"

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
  role <- liftEither . maybeToRight (JwtErr JwtTokenRequired) $
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
    walkJSPath (Just (JSON.Array ar)) [JSPFilter filterCond] = case filterCond of
        EqualsCond txt     -> findFirstMatch (==) txt ar
        NotEqualsCond txt  -> findFirstMatch (/=) txt ar
        StartsWithCond txt -> findFirstMatch T.isPrefixOf txt ar
        EndsWithCond txt   -> findFirstMatch T.isSuffixOf txt ar
        ContainsCond txt   -> findFirstMatch T.isInfixOf txt ar
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

-- | Perform authentication and authorization
--   Parse JWT and return AuthResult
getAuthResult :: AppState -> ApiRequest -> IO (Either Error AuthResult)
getAuthResult appState ApiRequest{..} = do
  conf <- getConfig appState
  time <- getTime appState

  let ciHdrs = map (first CI.mk) iHeaders
      token  = Wai.extractBearerAuth =<< lookup HTTP.hAuthorization ciHdrs
      parseJwt = runExceptT $ parseToken conf token time >>= parseClaims conf
      jwtCacheState = getJwtCacheState appState

  case configJwtCacheMaxLifetime conf of
    0           -> parseJwt -- If 0 then cache is diabled; no lookup
    maxLifetime -> case token of
      -- Lookup only if token found in header
      Just tkn -> lookupJwtCache jwtCacheState tkn maxLifetime parseJwt time
      Nothing  -> parseJwt
