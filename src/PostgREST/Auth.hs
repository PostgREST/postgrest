{-# LANGUAGE FlexibleContexts #-}
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
module PostgREST.Auth (
    setRole
  , claimsToSQL
  , jwtClaims
  , tokenJWT
  ) where

import           Data.Aeson              (Value (..), Object)
import           Data.Aeson.Types        (emptyObject, emptyArray)
import           Data.Vector             as V (null, head)
import           Data.Map                as M (fromList, toList)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           PostgREST.PgQuery       (pgFmtLit, pgFmtIdent, unquoted)
import qualified Web.JWT                 as JWT
import qualified Data.HashMap.Lazy       as H

{-|
  Receives a map of JWT claims and returns a list
  of PostgreSQL statements to set the claims as user defined GUCs.
  Except if we have a claim called role,
  this one is mapped to a SET ROLE statement.
  In case there is any problem decoding the JWT it returns Nothing.
-}
claimsToSQL :: JWT.ClaimsMap -> [Text]
claimsToSQL = map setVar . toList
  where
    setVar ("role", String val) = setRole val
    setVar (k, val) = "set local postgrest.claims." <> pgFmtIdent k <>
                      " = " <> valueToVariable val <> ";"
    valueToVariable = pgFmtLit . unquoted

{-|
  Receives the JWT secret (from config) and a JWT and
  returns a map of JWT claims
  In case there is any problem decoding the JWT it returns Nothing.
-}
jwtClaims :: Text -> Text -> Maybe JWT.ClaimsMap
jwtClaims secret input = JWT.unregisteredClaims <$> JWT.claims <$> decoded
  where
    decoded = JWT.decodeAndVerifySignature (JWT.secret secret) input

-- | Receives the name of a role and returns a SET ROLE statement
setRole :: Text -> Text
setRole role = "set local role " <> cs (pgFmtLit role) <> ";"


{-|
  Receives the JWT secret (from config) and a JWT and a JSON value
  and returns a signed JWT.
-}
tokenJWT :: Text -> Value -> Text
tokenJWT secret (Array a) = JWT.encodeSigned JWT.HS256 (JWT.secret secret)
                               JWT.def { JWT.unregisteredClaims = fromHashMap o }
                          where
                            Object o = if V.null a then emptyObject else V.head a
                            fromHashMap :: Object -> JWT.ClaimsMap
                            fromHashMap = M.fromList . H.toList
tokenJWT secret _          = tokenJWT secret emptyArray
