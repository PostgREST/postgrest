{-# LANGUAGE FlexibleContexts #-}
module PostgREST.Auth (
  setRole
  , setJWTEnv
  , tokenJWT
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Map (fromList, toList)
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.String.Conversions (cs)
import           Data.Text (Text)
import           PostgREST.PgQuery       (pgFmtLit)
import           Prelude
import qualified Web.JWT                 as JWT
import qualified Data.HashMap.Lazy       as HashMap
import Data.Aeson.Lens
import Control.Lens.Operators

setJWTEnv :: Text -> Text -> Maybe [Text]
setJWTEnv secret input = setDBEnv $ jwtClaims secret input

setDBEnv :: Maybe JWT.ClaimsMap -> Maybe [Text]
setDBEnv maybeClaims =
  (map setVar . toList) <$> maybeClaims
  where
    setVar ("role", String val) = setRole val
    setVar (k, String val) = "set local postgrest.claims." <> k <> " = " <> pgFmtLit val <> ";"
    setVar (k, Bool val) = "set local postgrest.claims." <> k <> " = " <> showText val <> ";"
    setVar (k, Number val) = "set local postgrest.claims." <> k <> " = " <> showText val <> ";"
    setVar _ = ""
    showText :: Show a => a -> Text
    showText = cs . show

setRole :: Text -> Text
setRole role = "set local role " <> cs (pgFmtLit role) <> ";"

jwtClaims :: Text -> Text -> Maybe JWT.ClaimsMap
jwtClaims secret input = claims
  where
    claims = JWT.unregisteredClaims <$> JWT.claims <$> decoded
    decoded = JWT.decodeAndVerifySignature (JWT.secret secret) input

tokenJWT :: Text -> Value -> Text
tokenJWT secret claims = JWT.encodeSigned JWT.HS256 (JWT.secret secret) claimsSet
  where
    claimsSet = JWT.def {
      JWT.unregisteredClaims = Data.Map.fromList claimsList
    }
    claimsList =  fromMaybe [] $ HashMap.toList <$> (claims ^? nth 0 . _Object)
