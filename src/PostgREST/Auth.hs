{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
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
module PostgREST.Auth
  ( getResult
  , getJwtDur
  , getRole
  , middleware
  ) where

import qualified Data.ByteString                 as BS
import qualified Data.Vault.Lazy                 as Vault
import qualified Network.HTTP.Types.Header       as HTTP
import qualified Network.Wai                     as Wai
import qualified Network.Wai.Middleware.HttpAuth as Wai

import Data.List        (lookup)
import System.IO.Unsafe (unsafePerformIO)
import System.TimeIt    (timeItT)

import PostgREST.AppState      (AppState, getConfig, getJwtCacheState,
                                getTime)
import PostgREST.Auth.JwtCache (lookupJwtCache)
import PostgREST.Auth.Types    (AuthResult (..))
import PostgREST.Config        (AppConfig (..), FilterExp (..),
                                JSPath, JSPathExp (..))
import PostgREST.Error         (Error (..), JwtError (..))

import           Control.Monad.Except (liftEither)
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Key       as K
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Protolude

-- | Validate authorization header
--   Parse and store JWT claims for future use in the request.
middleware :: AppState -> Wai.Middleware
middleware appState app req respond = do
  conf@AppConfig{..} <- getConfig appState
  time <- getTime appState

  let token  = Wai.extractBearerAuth =<< lookup HTTP.hAuthorization (Wai.requestHeaders req)
      parseToken = maybe (pure KM.empty) (lookupJwtCache jwtCacheState time)
      parseJwt = runExceptT $ parseToken >=> parseClaims conf $ token
      jwtCacheState = getJwtCacheState appState

  -- If ServerTimingEnabled -> calculate JWT validation time
  req' <- if configServerTimingEnabled then do
      (dur, authResult) <- timeItT parseJwt
      pure $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult & Vault.insert jwtDurKey dur }
    else do
      authResult <- parseJwt
      pure $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult }

  app req' respond

parseClaims :: (MonadError Error m, MonadIO m) => AppConfig -> JSON.Object -> m AuthResult
parseClaims AppConfig{configJwtRoleClaimKey, configDbAnonRole} mclaims = do
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
    unquoted v               = BS.toStrict $ JSON.encode v

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
