{-|
Module      : PostgREST.Auth.Jwt
Description : PostgREST JWT support functions.

This module provides functions to deal with JWT parsing and validation (http://jwt.io).
-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}

module PostgREST.Auth.Jwt
  ( parseAndDecodeClaims
  , parseClaims) where

import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.KeyMap          as KM
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Scientific            as Sci
import qualified Jose.Jwk                   as JWT
import qualified Jose.Jwt                   as JWT

import Control.Monad.Except    (liftEither)
import Data.Either.Combinators (mapLeft)
import Data.Text               ()
import Data.Time.Clock         (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX   (utcTimeToPOSIXSeconds)

import PostgREST.Auth.Types    (AuthResult (..))
import PostgREST.Config        (AppConfig (..), audMatchesCfg)
import PostgREST.Config.JSPath (walkJSPath)
import PostgREST.Error         (Error (..),
                                JwtClaimsError (AudClaimNotStringOrArray, ExpClaimNotNumber, IatClaimNotNumber, JWTExpired, JWTIssuedAtFuture, JWTNotInAudience, JWTNotYetValid, NbfClaimNotNumber, ParsingClaimsFailed),
                                JwtDecodeError (..), JwtError (..))

import Data.Aeson       ((.:?))
import Data.Aeson.Types (parseMaybe)
import Jose.Jwk         (JwkSet)
import Protolude        hiding (first)

parseAndDecodeClaims :: (MonadError Error m, MonadIO m) => JwkSet -> ByteString -> m JSON.Object
parseAndDecodeClaims jwkSet token = parseToken jwkSet token >>= decodeClaims

decodeClaims :: MonadError Error m => JWT.JwtContent -> m JSON.Object
decodeClaims (JWT.Jws (_, claims)) = maybe (throwError (JwtErr $ JwtClaimsErr ParsingClaimsFailed)) pure (JSON.decodeStrict claims)
decodeClaims _ = throwError $ JwtErr $ JwtDecodeErr UnsupportedTokenType

validateClaims :: MonadError Error m => UTCTime -> (Text -> Bool) -> JSON.Object -> m ()
validateClaims time audMatches claims = liftEither $ maybeToLeft () (fmap JwtErr . getAlt $ JwtClaimsErr <$> checkForErrors time audMatches claims)

data ValidAud = VAString Text | VAArray [Text] deriving Generic
instance JSON.FromJSON ValidAud where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions { JSON.sumEncoding = JSON.UntaggedValue }

checkForErrors :: (Applicative m, Monoid (m JwtClaimsError)) => UTCTime -> (Text -> Bool) -> JSON.Object -> m JwtClaimsError
checkForErrors time audMatches = mconcat
  [
    claim "exp" ExpClaimNotNumber $ inThePast JWTExpired
  , claim "nbf" NbfClaimNotNumber $ inTheFuture JWTNotYetValid
  , claim "iat" IatClaimNotNumber $ inTheFuture JWTIssuedAtFuture
  , claim "aud" AudClaimNotStringOrArray $ checkValue (not . validAud) JWTNotInAudience
  ]
  where
      allowedSkewSeconds = 30 :: Int64
      sciToInt = fromMaybe 0 . Sci.toBoundedInteger
      toSec = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
      now = toSec time

      inTheFuture = checkTime ((now + allowedSkewSeconds) <)
      inThePast = checkTime ((now - allowedSkewSeconds) >)

      checkTime cond = checkValue (cond. sciToInt)

      validAud = \case
        (VAString aud) -> audMatches aud
        (VAArray auds) -> null auds || any audMatches auds

      checkValue invalid msg val =
        if invalid val then
          pure msg
        else
          mempty

      claim key parseError checkParsed = maybe (pure parseError) (maybe mempty checkParsed) . parseMaybe (.:? key)

-- | Receives the JWT secret and audience (from config) and a JWT and returns a
-- JSON object of JWT claims.
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

parseClaims :: (MonadError Error m, MonadIO m) => AppConfig -> UTCTime -> JSON.Object -> m AuthResult
parseClaims cfg@AppConfig{configJwtRoleClaimKey, configDbAnonRole} time mclaims = do
  validateClaims time (audMatchesCfg cfg) mclaims
  -- role defaults to anon if not specified in jwt
  role <- liftEither . maybeToRight (JwtErr JwtTokenRequired) $
    unquoted <$> walkJSPath (Just $ JSON.Object mclaims) configJwtRoleClaimKey <|> configDbAnonRole
  pure AuthResult
           { authClaims = mclaims & KM.insert "role" (JSON.toJSON $ decodeUtf8 role)
           , authRole = role
           }
  where
    unquoted :: JSON.Value -> BS.ByteString
    unquoted (JSON.String t) = encodeUtf8 t
    unquoted v               = LBS.toStrict $ JSON.encode v
