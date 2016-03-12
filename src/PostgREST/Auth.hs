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

import           Control.Lens
import           Data.Aeson              (Value (..), parseJSON, toJSON)
import           Data.Aeson.Lens
import           Data.Aeson.Types        (parseMaybe, emptyObject, emptyArray)
import qualified Data.ByteString         as BS
import qualified Data.Vector             as V
import qualified Data.HashMap.Strict     as M
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Time.Clock         (NominalDiffTime)
import           PostgREST.QueryBuilder  (pgFmtLit, pgFmtIdent, unquoted)
import qualified Web.JWT                 as JWT

{-|
  Receives a map of JWT claims and returns a list
  of PostgreSQL statements to set the claims as user defined GUCs.
  Except if we have a claim called role,
  this one is mapped to a SET ROLE statement.
  In case there is any problem decoding the JWT it returns Nothing.
-}
claimsToSQL :: M.HashMap Text Value -> [BS.ByteString]
claimsToSQL = map setVar . M.toList
  where
    setVar ("role", String val) = setRole val
    setVar (k, val) = "set local " <> cs (pgFmtIdent $ "postgrest.claims." <> k)
                      <> " = " <> cs (valueToVariable val) <> ";"
    valueToVariable = pgFmtLit . unquoted

{-|
  Receives the JWT secret (from config) and a JWT and
  returns a map of JWT claims
  In case there is any problem decoding the JWT it returns Nothing.
-}


jwtClaims :: JWT.Secret -> Text -> NominalDiffTime -> Either Text (M.HashMap Text Value)
jwtClaims secret input time =
  case mClaims of
    Nothing -> Right M.empty
    Just claims -> do
      let mExp = claims ^? key "exp" . _Integer
          expired = fromMaybe False $ (<= time) . fromInteger <$> mExp
      if expired
        then Left "JWT expired"
        else Right (value2map claims)
 where
  mClaims = toJSON . JWT.claims <$> JWT.decodeAndVerifySignature secret input
  value2map (Object o) = o
  value2map _          = M.empty

{-| Receives the name of a role and returns a SET ROLE statement -}
setRole :: Text -> BS.ByteString
setRole r = "set local role " <> cs (pgFmtLit r) <> ";"


{-|
  Receives the JWT secret (from config) and a JWT and a JSON value
  and returns a signed JWT.
-}
tokenJWT :: JWT.Secret -> Value -> Text
tokenJWT secret (Array arr) =
  let obj = if V.null arr then emptyObject else V.head arr
      jcs = parseMaybe parseJSON obj :: Maybe JWT.JWTClaimsSet in
  JWT.encodeSigned JWT.HS256 secret $ fromMaybe JWT.def jcs
tokenJWT secret _ = tokenJWT secret emptyArray
