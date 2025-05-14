{-|
Module      : PostgREST.Auth.Jwt
Description : PostgREST JWT support functions.

This module provides functions to deal with the JWT authentication (http://jwt.io).
It also can be used to define other authentication functions,
in the future Oauth, LDAP and similar integrations can be coded here.

Authentication should always be implemented in an external service.
In the test suite there is an example of simple login function that can be used for a
very simple authentication system inside the PostgreSQL database.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module PostgREST.Auth.Jwt
  ( parseAndDecodeClaims
  , parseClaims) where

import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Key                  as K
import qualified Data.Aeson.KeyMap               as KM
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Internal        as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import qualified Data.Scientific                 as Sci
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Jose.Jwk                        as JWT
import qualified Jose.Jwt                        as JWT

import Control.Monad.Except    (liftEither)
import Data.Either.Combinators (mapLeft)
import Data.Text               ()
import Data.Time.Clock         (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX   (utcTimeToPOSIXSeconds)

--import PostgREST.Auth.JwtCache (lookupJwtCache)
import PostgREST.Auth.Types    (AuthResult (..))
import PostgREST.Config        (AppConfig (..), FilterExp (..),
                                JSPath, JSPathExp (..))
import PostgREST.Error         (Error (..), JwtError (..))

import Protolude hiding (first)
import Control.Arrow (Kleisli(Kleisli, runKleisli))
import Jose.Jwk (JwkSet)
import Data.Aeson.Types (parseMaybe)
import Data.Aeson ((.:?))
import Data.Aeson.Key (toString)

parseAndDecodeClaims :: JwkSet -> ByteString -> ExceptT Error IO JSON.Object
parseAndDecodeClaims jwkSet token = parseToken jwkSet token >>= decodeClaims

decodeClaims :: MonadError Error m => JWT.JwtContent -> m JSON.Object
decodeClaims (JWT.Jws (_, claims)) = maybe (throwError (JwtErr $ JwtClaimsError "Parsing claims failed")) pure (JSON.decodeStrict claims)
decodeClaims _ = throwError $ JwtErr $ JwtDecodeError "Unsupported token type"

validateClaims :: MonadError Error m => UTCTime -> Maybe Text -> JSON.Object -> m ()
validateClaims time getConfigAud claims = liftEither $ maybeToLeft () (checkForErrors time getConfigAud claims)

data ValidAud = VANull | VAString Text | VAArray [Text] deriving Generic
instance JSON.FromJSON ValidAud where
  parseJSON JSON.Null = pure VANull
  parseJSON o = JSON.genericParseJSON JSON.defaultOptions { JSON.sumEncoding = JSON.UntaggedValue } o

checkForErrors :: (Monad m, Alternative m) => UTCTime -> Maybe Text -> JSON.Object -> m Error
checkForErrors time cfgAud = (runKleisli . asum) . fmap (Kleisli . checkClaim) $
  [
    claim "exp" parseNumberError $ inThePast "JWT expired"
  , claim "nbf" parseNumberError $ inTheFuture "JWT not yet valid"
  , claim "iat" parseNumberError $ inTheFuture "JWT issued at future"
  , claim "aud" (const "The JWT 'aud' claim must be a string or an array of strings") checkAud
  ]
  where
      checkClaim = ((JwtErr . JwtClaimsError <$>) .)

      allowedSkewSeconds = 30 :: Int64
      sciToInt = fromMaybe 0 . Sci.toBoundedInteger
      toSec t = floor . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds t
      now = toSec time

      inTheFuture = checkTime ((now + allowedSkewSeconds) <)
      inThePast = checkTime ((now - allowedSkewSeconds) >)

      checkTime cond = checkValue (cond. sciToInt)

      checkAud (VAString aud) = maybe empty pure cfgAud >>= checkValue (aud /=) jwtNotInAudience
      checkAud (VAArray auds) | (not . null) auds = maybe empty pure cfgAud >>= checkValue (not . (`elem` auds)) jwtNotInAudience
      checkAud _ = empty

      jwtNotInAudience = "JWT not in audience"

      checkValue invalid msg val =
        if invalid val then
          pure msg
        else
          empty

      claim key parseError checkParsed = maybe (pure $ parseError (T.pack (toString key))) (maybe empty checkParsed) . parseMaybe (.:? key)

      parseNumberError key = mconcat ["The JWT '", key, "' claim must be a number"]

-- | Receives the JWT secret and audience (from config) and a JWT and returns a
-- JSON object of JWT claims.
parseToken :: JwkSet -> ByteString -> ExceptT Error IO JWT.JwtContent
parseToken _ "" = throwError . JwtErr $ JwtDecodeError "Empty JWT is sent in Authorization header"
parseToken secret tkn = do
  -- secret <- liftEither . maybeToRight (JwtErr JwtSecretMissing) $ configJWKS
  tknWith3Parts <- hasThreeParts tkn
  eitherContent <- lift $ JWT.decode (JWT.keys secret) Nothing tknWith3Parts
  liftEither . mapLeft (JwtErr . jwtDecodeError) $ eitherContent
  --liftEither $ mapLeft JwtErr $ verifyClaims content
  where
      --hasThreeParts :: ByteString -> Either Error ByteString
      hasThreeParts token = case length $ BS.split (BS.c2w '.') token of
        3 -> pure token
        n -> throwError $ JwtErr $ JwtDecodeError ("Expected 3 parts in JWT; got " <> show n)

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

parseClaims :: AppConfig -> UTCTime -> JSON.Object -> ExceptT Error IO AuthResult
parseClaims AppConfig{configJwtAudience, configJwtRoleClaimKey, configDbAnonRole} time mclaims = do
  validateClaims time configJwtAudience mclaims
  -- role defaults to anon if not specified in jwt
  role <- liftEither . maybeToRight (JwtErr JwtTokenRequired) $
    unquoted <$> walkJSPath (Just $ JSON.Object mclaims) configJwtRoleClaimKey <|> configDbAnonRole
  pure AuthResult
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
