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
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Auth
  ( AuthResult (..)
  , getResult
  , getRole
  , middleware
  ) where

import qualified Crypto.JWT                      as JWT
import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Types                as JSON
import qualified Data.ByteString.Lazy.Char8      as LBS
import qualified Data.HashMap.Strict             as HM
import qualified Data.Text.Encoding              as T
import qualified Data.Vault.Lazy                 as Vault
import qualified Data.Vector                     as V
import qualified Network.HTTP.Types.Header       as HTTP
import qualified Network.Wai                     as Wai
import qualified Network.Wai.Middleware.HttpAuth as Wai

import Control.Lens            (set)
import Control.Monad.Except    (liftEither)
import Data.Either.Combinators (mapLeft)
import Data.List               (lookup)
import Data.Time.Clock         (UTCTime)
import System.IO.Unsafe        (unsafePerformIO)

import PostgREST.AppState (AppState, getConfig, getTime)
import PostgREST.Config   (AppConfig (..), JSPath, JSPathExp (..))
import PostgREST.Error    (Error (..))

import Protolude


data AuthResult = AuthResult
  { authClaims :: HM.HashMap Text JSON.Value
  , authRole   :: Text
  }

-- | Receives the JWT secret and audience (from config) and a JWT and returns a
-- JSON object of JWT claims.
parseToken :: Monad m =>
  AppConfig -> LByteString -> UTCTime -> ExceptT Error m JSON.Value
parseToken _ "" _ = return JSON.emptyObject
parseToken AppConfig{..} token time = do
  secret <- liftEither . maybeToRight JwtTokenMissing $ configJWKS
  eitherClaims <-
    lift . runExceptT $
      JWT.verifyClaimsAt validation secret time =<< JWT.decodeCompact token
  liftEither . mapLeft jwtClaimsError $ JSON.toJSON <$> eitherClaims
  where
    validation =
      JWT.defaultJWTValidationSettings audienceCheck & set JWT.allowedSkew 1

    audienceCheck :: JWT.StringOrURI -> Bool
    audienceCheck = maybe (const True) (==) configJwtAudience

    jwtClaimsError :: JWT.JWTError -> Error
    jwtClaimsError JWT.JWTExpired = JwtTokenInvalid "JWT expired"
    jwtClaimsError e              = JwtTokenInvalid $ show e

parseClaims :: Monad m =>
  AppConfig -> JSON.Value -> ExceptT Error m AuthResult
parseClaims AppConfig{..} jclaims@(JSON.Object mclaims) = do
  -- role defaults to anon if not specified in jwt
  role <- liftEither . maybeToRight JwtTokenRequired $
    unquoted <$> walkJSPath (Just jclaims) configJwtRoleClaimKey <|> configDbAnonRole
  return AuthResult
           { authClaims = mclaims & HM.insert "role" (JSON.toJSON role)
           , authRole = role
           }
  where
    walkJSPath :: Maybe JSON.Value -> JSPath -> Maybe JSON.Value
    walkJSPath x                      []                = x
    walkJSPath (Just (JSON.Object o)) (JSPKey key:rest) = walkJSPath (HM.lookup key o) rest
    walkJSPath (Just (JSON.Array ar)) (JSPIdx idx:rest) = walkJSPath (ar V.!? idx) rest
    walkJSPath _                      _                 = Nothing

    unquoted :: JSON.Value -> Text
    unquoted (JSON.String t) = t
    unquoted v = T.decodeUtf8 . LBS.toStrict $ JSON.encode v
-- impossible case - just added to please -Wincomplete-patterns
parseClaims _ _ = return AuthResult { authClaims = HM.empty, authRole = mempty }

-- | Validate authorization header.
--   Parse and store JWT claims for future use in the request.
middleware :: AppState -> Wai.Middleware
middleware appState app req respond = do
  conf <- getConfig appState
  time <- getTime appState

  let token = fromMaybe "" $ Wai.extractBearerAuth =<< lookup HTTP.hAuthorization (Wai.requestHeaders req)
  authResult <- runExceptT $
    parseToken conf (LBS.fromStrict token) time >>=
    parseClaims conf

  let req' = req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult }
  app req' respond

authResultKey :: Vault.Key (Either Error AuthResult)
authResultKey = unsafePerformIO Vault.newKey
{-# NOINLINE authResultKey #-}

getResult :: Wai.Request -> Maybe (Either Error AuthResult)
getResult = Vault.lookup authResultKey . Wai.vault

getRole :: Wai.Request -> Maybe Text
getRole req = authRole <$> (rightToMaybe =<< getResult req)
