{-# LANGUAGE FlexibleContexts #-}
module PostgREST.Auth (
  setRole
  , setJWTEnv
  , tokenJWT
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types (emptyObject, emptyArray)
import           Data.Vector as V (null, head)
import           Data.Map as M (fromList, toList)
import           Data.Monoid
import           Data.String.Conversions (cs)
import           Data.Text (Text)
import           PostgREST.PgQuery       (pgFmtLit, pgFmtIdent, unquoted)
import           Prelude
import qualified Web.JWT                 as JWT
import qualified Data.HashMap.Lazy       as H

setJWTEnv :: Text -> Text -> Maybe [Text]
setJWTEnv secret input = setDBEnv $ jwtClaims secret input

setDBEnv :: Maybe JWT.ClaimsMap -> Maybe [Text]
setDBEnv maybeClaims =
  (map setVar . toList) <$> maybeClaims
  where
    setVar ("role", String val) = setRole val
    setVar (k, val) = "set local postgrest.claims." <> pgFmtIdent k <>
                  " = " <> valueToVariable val <> ";"
    valueToVariable = pgFmtLit . unquoted

setRole :: Text -> Text
setRole role = "set local role " <> cs (pgFmtLit role) <> ";"

jwtClaims :: Text -> Text -> Maybe JWT.ClaimsMap
jwtClaims secret input = claims
  where
    claims = JWT.unregisteredClaims <$> JWT.claims <$> decoded
    decoded = JWT.decodeAndVerifySignature (JWT.secret secret) input

tokenJWT :: Text -> Value -> Text
tokenJWT secret (Array a) = JWT.encodeSigned JWT.HS256 (JWT.secret secret)
                               JWT.def { JWT.unregisteredClaims = fromHashMap o }
                          where
                            Object o = if V.null a then emptyObject else V.head a
tokenJWT secret _          = tokenJWT secret emptyArray

fromHashMap :: Object -> JWT.ClaimsMap
fromHashMap = M.fromList . H.toList
