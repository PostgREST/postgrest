{-# LANGUAGE RecordWildCards #-}
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
import PostgREST.Auth.Jwt      (parseClaims, parseToken)
import PostgREST.Auth.JwtCache (lookupJwtCache)
import PostgREST.Auth.Types    (AuthResult (..))
import PostgREST.Config        (AppConfig (..))
import PostgREST.Error         (Error (..))

import Protolude

-- | Validate authorization header.
--   Parse and store JWT claims for future use in the request.
middleware :: AppState -> Wai.Middleware
middleware appState app req respond = do
  cfg@AppConfig{..} <- getConfig appState

  let timeAction = getTime appState
      token  = Wai.extractBearerAuth =<< lookup HTTP.hAuthorization (Wai.requestHeaders req)
      parseJwt = runExceptT $ parseToken cfg token timeAction >>= parseClaims cfg
      jwtCacheState = getJwtCacheState appState

-- If ServerTimingEnabled -> calculate JWT validation time
-- If JwtCacheMaxLifetime -> cache JWT validation result
  req' <- case (configServerTimingEnabled, configJwtCacheMaxLifetime) of
    (True, 0)            -> do
          (dur, authResult) <- timeItT parseJwt
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult & Vault.insert jwtDurKey dur }

    (True, maxLifetime)  -> do
          (dur, authResult) <- timeItT $ case token of
            Just tkn -> lookupJwtCache jwtCacheState tkn maxLifetime parseJwt timeAction
            Nothing  -> parseJwt
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult & Vault.insert jwtDurKey dur }

    (False, 0)           -> do
          authResult <- parseJwt
          return $ req { Wai.vault = Wai.vault req & Vault.insert authResultKey authResult }

    (False, maxLifetime) -> do
          authResult <- case token of
            Just tkn -> lookupJwtCache jwtCacheState tkn maxLifetime parseJwt timeAction
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
