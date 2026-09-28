{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- |
-- Module      : PostgREST.Auth.Jwt
-- Description : PostgREST JWT support functions.
--
-- This module provides functions to deal with JWT parsing and validation (http://jwt.io).
module PostgREST.Auth.Jwt
  ( parseAndDecodeClaims
  , parseClaims
  )
where

import Control.Monad.Except (liftEither)
import Data.Aeson ((.:?))
import Data.Aeson.Types (parseMaybe)
import Data.Bitraversable (bitraverse)
import Data.Either.Combinators (mapLeft)
import Data.Text ()
import Data.Time.Clock (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Jose.Jwk (Jwk, JwkSet)
import Protolude hiding (first)

import Data.Aeson qualified as JSON
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Scientific qualified as Sci
import Jose.Jwk qualified as JWT
import Jose.Jwt qualified as JWT

import PostgREST.Auth.Types (AuthResult (..))
import PostgREST.Config (AppConfig (..), audMatchesCfg)
import PostgREST.Config.JSPath (evaluateJSPath)
import PostgREST.Error
  ( Error (..)
  , JwtClaimsError (..)
  , JwtDecodeError (..)
  , JwtError (..)
  )

-- | Also returns the key that verified the JWT
parseAndDecodeClaims :: (MonadError Error m, MonadIO m) => JwkSet -> ByteString -> m (JSON.Object, Maybe Jwk)
parseAndDecodeClaims jwkSet = parseToken jwkSet >=> bitraverse decodeClaims pure

decodeClaims :: (MonadError Error m) => JWT.JwtContent -> m JSON.Object
decodeClaims (JWT.Jws (_, claims)) = maybe (throwError (JwtErr $ JwtClaimsErr ParsingClaimsFailed)) pure (JSON.decodeStrict claims)
decodeClaims _ = throwError $ JwtErr $ JwtDecodeErr UnsupportedTokenType

validateClaims :: (MonadError Error m) => UTCTime -> (Text -> Bool) -> JSON.Object -> m ()
validateClaims time audMatches claims = liftEither $ maybeToLeft () (fmap JwtErr . getAlt $ JwtClaimsErr <$> checkForErrors time audMatches claims)

data ValidAud = VAString Text | VAArray [Text] deriving (Generic)

instance JSON.FromJSON ValidAud where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions{JSON.sumEncoding = JSON.UntaggedValue}

checkForErrors :: (Applicative m, Monoid (m JwtClaimsError)) => UTCTime -> (Text -> Bool) -> JSON.Object -> m JwtClaimsError
checkForErrors time audMatches =
  mconcat
    [ claim "exp" ExpClaimNotNumber $ inThePast JWTExpired
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

    checkTime cond = checkValue (cond . sciToInt)

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
parseToken :: (MonadError Error m, MonadIO m) => JwkSet -> ByteString -> m (JWT.JwtContent, Maybe Jwk)
parseToken _ "" = throwError $ JwtErr $ JwtDecodeErr EmptyAuthHeader
parseToken secret tkn = do
  tknWith3Parts <- hasThreeParts tkn
  eitherContent <- liftIO $ decodeWithKey (JWT.keys secret) tknWith3Parts
  liftEither . mapLeft (JwtErr . jwtDecodeError) $ eitherContent
  where
    hasThreeParts token = case length $ BS.split (BS.c2w '.') token of
      3 -> pure token
      n -> throwError $ JwtErr $ JwtDecodeErr $ UnexpectedParts n

    jwtDecodeError :: JWT.JwtError -> JwtError
    -- The only errors we can get from JWT.decode function are:
    --   BadAlgorithm
    --   KeyError
    --   BadCrypto
    jwtDecodeError (JWT.KeyError m) = JwtDecodeErr $ KeyError m
    jwtDecodeError (JWT.BadAlgorithm m) = JwtDecodeErr $ BadAlgorithm m
    jwtDecodeError JWT.BadCrypto = JwtDecodeErr BadCrypto
    -- Control never reaches here, the decode function only returns the above three
    jwtDecodeError _ = JwtDecodeErr UnreachableDecodeError

-- | Like JWT.decode, but also returns the key that verified a JWS. As
-- JWT.decode does, the header is read once, the keys that can't verify the
-- token (by kid, alg and key type) are skipped, and the others are tried in
-- order until one verifies it, so the result and the errors are the same.
decodeWithKey :: [Jwk] -> ByteString -> IO (Either JWT.JwtError (JWT.JwtContent, Maybe Jwk))
decodeWithKey keys token = case jwsHeader of
  Just header -> firstVerified $ filter (JWT.canDecodeJws header) keys
  -- not a JWS, or a header read differently by JWT.decode, which finds out
  Nothing ->
    JWT.decode keys Nothing token >>= \case
      Right JWT.Jws{} -> firstVerified keys
      result -> pure $ (,Nothing) <$> result
  where
    -- no candidates means no verification: JWT.decode only reports why
    firstVerified [] = fmap (,Nothing) <$> JWT.decode keys Nothing token
    firstVerified candidates = go candidates

    go (key : rest) =
      JWT.decode [key] Nothing token >>= \case
        Right content -> pure $ Right (content, Just key)
        -- the key didn't verify the token
        Left (JWT.KeyError _) -> go rest
        -- the token is malformed, whatever the key
        Left err -> pure $ Left err
    go [] = pure . Left $ JWT.KeyError "None of the keys was able to decode the JWT"

    jwsHeader = case BS.split (BS.c2w '.') token of
      [header, _, _]
        | Right bytes <- B64URL.decodeUnpadded header
        , Right (JWT.JwsH jws) <- JWT.parseHeader bytes ->
            Just jws
      _ -> Nothing

parseClaims :: (MonadError Error m, MonadIO m) => AppConfig -> UTCTime -> JSON.Object -> m AuthResult
parseClaims cfg@AppConfig{configJwtRoleClaimKey, configDbAnonRole} time mclaims = do
  validateClaims time (audMatchesCfg cfg) mclaims
  -- role defaults to anon if not specified in jwt
  role <-
    liftEither . maybeToRight (JwtErr JwtTokenRequired) $
      unquoted <$> evaluateJSPath (Just $ JSON.Object mclaims) configJwtRoleClaimKey <|> configDbAnonRole
  pure
    AuthResult
      { authClaims = mclaims
      , authRole = role
      }
  where
    unquoted :: JSON.Value -> BS.ByteString
    unquoted (JSON.String t) = encodeUtf8 t
    unquoted v = LBS.toStrict $ JSON.encode v
