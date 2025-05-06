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
import qualified Data.ByteString.Internal        as BS
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
        Just jclaims@(JSON.Object mclaims) ->
          verifyClaim mclaims "exp" isValidExpClaim "JWT expired" >>
          verifyClaim mclaims "nbf" isValidNbfClaim "JWT not yet valid" >>
          verifyClaim mclaims "iat" isValidIatClaim "JWT issued at future" >>
          verifyClaim mclaims "aud" isValidAudClaim "JWT not in audience" >>
          return jclaims
        _ -> Left $ JwtClaimsError "Parsing claims failed"
      -- TODO: We could enable JWE support here (encrypted tokens)
      verifyClaims _ = Left $ JwtDecodeError "Unsupported token type"

      verifyClaim mclaims claim func err = do
        isValid <- maybe (Right True) func (KM.lookup claim mclaims)
        unless isValid $ Left $ JwtClaimsError err

      allowedSkewSeconds = 30 :: Int64
      now = floor . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds time
      sciToInt = fromMaybe 0 . Sci.toBoundedInteger

      isValidExpClaim :: JSON.Value -> Either JwtError Bool
      isValidExpClaim (JSON.Number secs) = Right $ now <= (sciToInt secs + allowedSkewSeconds)
      isValidExpClaim _ = Left $ JwtClaimsError "The JWT 'exp' claim must be a number"

      isValidNbfClaim :: JSON.Value -> Either JwtError Bool
      isValidNbfClaim (JSON.Number secs) = Right $ now >= (sciToInt secs - allowedSkewSeconds)
      isValidNbfClaim _ = Left $ JwtClaimsError "The JWT 'nbf' claim must be a number"

      isValidIatClaim :: JSON.Value -> Either JwtError Bool
      isValidIatClaim (JSON.Number secs) = Right $ now >= (sciToInt secs - allowedSkewSeconds)
      isValidIatClaim _ = Left $ JwtClaimsError "The JWT 'iat' claim must be a number"

      isValidAudClaim :: JSON.Value -> Either JwtError Bool
      isValidAudClaim (JSON.String str) = Right $ maybe (const True) (==) configJwtAudience str
      isValidAudClaim _ = Left $ JwtClaimsError "The JWT 'aud' claim must be a string or an array of strings"

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

  let token  = Wai.extractBearerAuth =<< lookup HTTP.hAuthorization (Wai.requestHeaders req)
      parseJwt = runExceptT $ parseToken conf token time >>= parseClaims conf
      jwtCacheState = getJwtCacheState appState

-- If ServerTimingEnabled -> calculate JWT validation time
-- If JwtCacheMaxLifetime -> cache JWT validation result
  req' <- case (configServerTimingEnabled conf, configJwtCacheMaxLifetime conf) of
    (True, 0)            -> do
          (dur, authResult) <- timeItT parseJwt
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult & Vault.insert jwtDurKey dur }

    (True, maxLifetime)  -> do
          (dur, authResult) <- timeItT $ case token of
            Just tkn -> lookupJwtCache jwtCacheState tkn maxLifetime parseJwt time
            Nothing  -> parseJwt
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult & Vault.insert jwtDurKey dur }

    (False, 0)           -> do
          authResult <- parseJwt
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult }

    (False, maxLifetime) -> do
          authResult <- case token of
            Just tkn -> lookupJwtCache jwtCacheState tkn maxLifetime parseJwt time
            Nothing -> parseJwt
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
