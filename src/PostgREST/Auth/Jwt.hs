{-|
Module      : PostgREST.Auth.Jwt
Description : PostgREST JWT support functions.

This module provides functions to deal with JWT parsing and validation (http://jwt.io).
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Auth.Jwt
  ( parseToken
  , parseClaims
  )
  where

import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.Key             as K
import qualified Data.Aeson.KeyMap          as KM
import qualified Data.Aeson.Types           as JSON
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Scientific            as Sci
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Jose.Jwk                   as JWT
import qualified Jose.Jwt                   as JWT

import Control.Monad.Except    (liftEither)
import Data.Either.Combinators (mapLeft)
import Data.Time.Clock         (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX   (utcTimeToPOSIXSeconds)

import PostgREST.Auth.Types (AuthResult (..))
import PostgREST.Config     (AppConfig (..), FilterExp (..), JSPath,
                             JSPathExp (..))
import PostgREST.Error      (Error (..), JwtClaimsError (..),
                             JwtDecodeError (..), JwtError (..))

import Protolude hiding (first)

data JwtTimeClaim = Exp | Iat | Nbf

-- | Receives the JWT secret and audience (from config) and a JWT and returns a
-- JSON object of JWT claims.
parseToken :: AppConfig -> Maybe ByteString -> UTCTime -> ExceptT Error IO JSON.Value
parseToken _ Nothing _ = return JSON.emptyObject
parseToken _ (Just "") _ = throwE . JwtErr $ JwtDecodeErr EmptyAuthHeader
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
        n -> Left $ JwtErr $ JwtDecodeErr $ UnexpectedParts n
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

      verifyClaims :: JWT.JwtContent -> Either JwtError JSON.Value
      verifyClaims (JWT.Jws (_, claims)) = case JSON.decodeStrict claims of
        Just jclaims@(JSON.Object mclaims) ->
          verifyClaim mclaims "exp" (isValidTimeClaim Exp) JWTExpired >>
          verifyClaim mclaims "nbf" (isValidTimeClaim Nbf) JWTNotYetValid >>
          verifyClaim mclaims "iat" (isValidTimeClaim Iat) JWTIssuedAtFuture >>
          verifyClaim mclaims "aud" isValidAudClaim JWTNotInAudience >>
          return jclaims
        _ -> Left $ JwtClaimsErr ParsingClaimsFailed
      -- TODO: We could enable JWE support here (encrypted tokens)
      verifyClaims _ = Left $ JwtDecodeErr UnsupportedTokenType

      verifyClaim mclaims claim isValidFunc err = do
        isValid <- maybe (Right True) isValidFunc (KM.lookup claim mclaims)
        unless isValid $ Left $ JwtClaimsErr err

      allowedSkewSeconds = 30 :: Int64
      now = floor . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds time
      sciToInt = fromMaybe 0 . Sci.toBoundedInteger
      allStrings = all (\case (JSON.String _) -> True; _ -> False)

      isValidTimeClaim ::  JwtTimeClaim -> JSON.Value -> Either JwtError Bool
      isValidTimeClaim jwtClaim (JSON.Number secs) = case jwtClaim of
        Exp -> Right $ (now - allowedSkewSeconds) < sciToInt secs
        Nbf -> Right $ (now + allowedSkewSeconds) > sciToInt secs
        Iat -> Right $ (now + allowedSkewSeconds) > sciToInt secs
      isValidTimeClaim jwtClaim _ = case jwtClaim of
        Exp -> Left $ JwtClaimsErr ExpClaimNotNumber
        Nbf -> Left $ JwtClaimsErr NbfClaimNotNumber
        Iat -> Left $ JwtClaimsErr IatClaimNotNumber

      isValidAudClaim :: JSON.Value -> Either JwtError Bool
      isValidAudClaim JSON.Null = Right True -- {"aud": null} is valid for all audiences
      isValidAudClaim (JSON.String str) = Right $ maybe (const True) (==) configJwtAudience str
      isValidAudClaim (JSON.Array arr)
        | null arr = Right True              -- {"aud": []} is valid for all audiences
        | allStrings arr = Right $ maybe True (\a -> JSON.String a `elem` arr) configJwtAudience
      isValidAudClaim _ = Left $ JwtClaimsErr AudClaimNotStringOrArray

parseClaims :: AppConfig -> JSON.Value -> ExceptT Error IO AuthResult
parseClaims AppConfig{..} jclaims@(JSON.Object mclaims) = do
  -- role defaults to anon if not specified in jwt
  role <- liftEither . maybeToRight (JwtErr JwtTokenRequired) $
    unquoted <$> walkJSPath (Just jclaims) configJwtRoleClaimKey <|> configDbAnonRole
  return AuthResult {
      authClaims = mclaims & KM.insert "role" (JSON.toJSON $ decodeUtf8 role)
    , authRole = role
    }
  where
    walkJSPath :: Maybe JSON.Value -> JSPath -> Maybe JSON.Value
    walkJSPath x                      []                = x
    walkJSPath (Just (JSON.Object o)) (JSPKey key:rest) = walkJSPath (KM.lookup (K.fromText key) o) rest
    walkJSPath (Just (JSON.Array ar)) (JSPIdx idx:rest) = walkJSPath (ar V.!? idx) rest
    walkJSPath (Just (JSON.Array ar)) [JSPFilter filt] =
      case filt of
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
