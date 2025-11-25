{-|
Module      : PostgREST.Auth.Jwt
Description : PostgREST JWT support functions.

This module provides functions to deal with JWT parsing and validation (http://jwt.io).
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PostgREST.Auth.Jwt
  ( Validation (..)
  , Validated (getValidated)
  , parseAndDecodeClaims
  , validateAud
  , validateTimeClaims
  , (>>>)) where

import qualified Data.Aeson               as JSON
import qualified Data.Aeson.KeyMap        as KM
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Scientific          as Sci
import qualified Jose.Jwk                 as JWT
import qualified Jose.Jwt                 as JWT

import Control.Monad.Except    (liftEither)
import Data.Either.Combinators (mapLeft)
import Data.Text               ()
import Data.Time.Clock         (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX   (utcTimeToPOSIXSeconds)

import PostgREST.Error (Error (..),
                        JwtClaimsError (AudClaimNotStringOrArray, ExpClaimNotNumber, IatClaimNotNumber, JWTExpired, JWTIssuedAtFuture, JWTNotInAudience, JWTNotYetValid, NbfClaimNotNumber, ParsingClaimsFailed),
                        JwtDecodeError (..), JwtError (..))

import Data.Aeson       ((.:?))
import Data.Aeson.Types (parseMaybe)
import Data.Coerce      (coerce)
import Jose.Jwk         (JwkSet)
import Protolude        hiding (first)

-- A value tagged by a type-level list of validations pefrormed on it
newtype Validated (k :: [v]) a = Validated { getValidated :: a }

-- Helper to implement type safe validation chaining
type family (++) (lst::[k]) lst' where
  '[] ++ lst = lst
  (l : ls) ++ lst = l : (ls ++ lst)

-- Validation chaining operator
(>>>) :: (Monad m, Coercible (m (Validated kc c)) (m (Validated (kb ++ kc) c)))
  => (a -> m (Validated kb b))
  -> (b -> m (Validated kc c))
  -> a
  -> m (Validated (kb ++ kc) c)
f >>> g = coerce . (f >=> g . coerce)

parseAndDecodeClaims :: (MonadError Error m, MonadIO m) => JwkSet -> ByteString -> m JSON.Object
parseAndDecodeClaims jwkSet = parseToken jwkSet >=> decodeClaims

data Validation = Aud | Time

validateAud :: MonadError Error m => (Text -> Bool) -> JSON.Object -> m (Validated '[Aud] JSON.Object)
validateAud = validate . checkAud

validateTimeClaims :: MonadError Error m => UTCTime -> JSON.Object -> m (Validated '[Time] JSON.Object)
validateTimeClaims = validate . checkExpNbfIat

decodeClaims :: MonadError Error m => JWT.JwtContent -> m JSON.Object
decodeClaims (JWT.Jws (_, claims)) = maybe (throwError (JwtErr $ JwtClaimsErr ParsingClaimsFailed)) pure (JSON.decodeStrict claims)
decodeClaims _ = throwError $ JwtErr $ JwtDecodeErr UnsupportedTokenType

validate :: MonadError Error m => (t -> Alt Maybe JwtClaimsError) -> t -> m (Validated k t)
validate f claims = fmap Validated $ liftEither $ maybeToLeft claims $ fmap JwtErr . getAlt $ JwtClaimsErr <$> f claims

data ValidAud = VAString Text | VAArray [Text] deriving Generic
instance JSON.FromJSON ValidAud where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions { JSON.sumEncoding = JSON.UntaggedValue }

claim :: (JSON.FromJSON a, Applicative f, Monoid (f p)) => KM.Key -> p -> (a -> f p) -> JSON.Object -> f p
claim key parseError checkParsed = maybe (pure parseError) (maybe mempty checkParsed) . parseMaybe (.:? key)

checkValue :: (Applicative f, Monoid (f p)) => (t -> Bool) -> p -> t -> f p
checkValue invalid msg val =
  if invalid val then
    pure msg
  else
    mempty

checkAud :: (Applicative f, Monoid (f JwtClaimsError)) => (Text -> Bool) -> JSON.Object -> f JwtClaimsError
checkAud audMatches = claim "aud" AudClaimNotStringOrArray $ checkValue (not . validAud) JWTNotInAudience
  where
    validAud = \case
      (VAString aud) -> audMatches aud
      (VAArray auds) -> null auds || any audMatches auds

checkExpNbfIat :: (Applicative m, Monoid (m JwtClaimsError)) => UTCTime -> JSON.Object -> m JwtClaimsError
checkExpNbfIat time = mconcat
  [
    claim "exp" ExpClaimNotNumber $ inThePast JWTExpired
  , claim "nbf" NbfClaimNotNumber $ inTheFuture JWTNotYetValid
  , claim "iat" IatClaimNotNumber $ inTheFuture JWTIssuedAtFuture
  ]
  where
      allowedSkewSeconds = 30 :: Int64
      sciToInt = fromMaybe 0 . Sci.toBoundedInteger
      toSec = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
      now = toSec time

      inTheFuture = checkTime ((now + allowedSkewSeconds) <)
      inThePast = checkTime ((now - allowedSkewSeconds) >)

      checkTime cond = checkValue (cond. sciToInt)

parseToken :: (MonadError Error m, MonadIO m) => JwkSet -> ByteString -> m JWT.JwtContent
parseToken _ "" = throwError $ JwtErr $ JwtDecodeErr EmptyAuthHeader
parseToken secret tkn = do
  -- secret <- liftEither . maybeToRight (JwtErr JwtSecretMissing) $ configJWKS
  tknWith3Parts <- hasThreeParts tkn
  eitherContent <- liftIO $ JWT.decode (JWT.keys secret) Nothing tknWith3Parts
  liftEither . mapLeft (JwtErr . jwtDecodeError) $ eitherContent
  --liftEither $ mapLeft JwtErr $ verifyClaims content
  where
      --hasThreeParts :: ByteString -> Either Error ByteString
      hasThreeParts token = case length $ BS.split (BS.c2w '.') token of
        3 -> pure token
        n -> throwError $ JwtErr $ JwtDecodeErr $ UnexpectedParts n

      jwtDecodeError :: JWT.JwtError -> JwtError
      -- The only errors we can get from JWT.decode function are:
      --   BadAlgorithm
      --   KeyError
      --   BadCrypto
      jwtDecodeError (JWT.KeyError m)     = JwtDecodeErr $ KeyError m
      jwtDecodeError (JWT.BadAlgorithm m) = JwtDecodeErr $ BadAlgorithm m
      jwtDecodeError JWT.BadCrypto        = JwtDecodeErr BadCrypto
      -- Control never reaches here, the decode function only returns the above three
      jwtDecodeError _                    = JwtDecodeErr UnreachableDecodeError
