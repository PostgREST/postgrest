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
  ( getAuthResult )
  where

import PostgREST.AppState      (AppState, getConfig, getJwtCacheState,
                                getTime)
import PostgREST.Auth.Jwt      (parseClaims)
import PostgREST.Auth.JwtCache (lookupJwtCache)
import PostgREST.Auth.Types    (AuthResult)
import PostgREST.Error         (Error)

import Protolude

-- | Perform authentication and authorization
--   Parse JWT and return AuthResult
getAuthResult :: AppState -> Maybe ByteString -> IO (Either Error AuthResult)
getAuthResult appState token = do
  conf <- getConfig appState
  time <- getTime appState

  let jwtCacheState = getJwtCacheState appState
      parseJwt = runExceptT $ do
        claims <- lookupJwtCache jwtCacheState token
        parseClaims conf time claims

  parseJwt
