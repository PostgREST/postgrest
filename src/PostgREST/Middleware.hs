{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import           Data.Aeson                    (Value (..))
import qualified Data.HashMap.Strict           as M
import qualified Hasql.Transaction             as H

import           Network.HTTP.Types.Status     (badRequest400, unauthorized401)
import           Network.Wai                   (Application, Response)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.ApiRequest          (ApiRequest(..))
import           PostgREST.Auth                (claimsToSQL, JWTAttempt(..))
import           PostgREST.Config              (AppConfig (..), corsPolicy)
import           PostgREST.Error               (errResponse)

import           Protolude                     hiding (concat, null)

runWithClaims :: AppConfig -> JWTAttempt ->
                 (ApiRequest -> H.Transaction Response) ->
                 ApiRequest -> H.Transaction Response
runWithClaims conf eClaims app req =
  case eClaims of
    JWTExpired -> return $ errResponse unauthorized401 "JWT expired"
    JWTInvalid -> return $ errResponse badRequest400 "JWT invalid"
    JWTClaims claims -> do
      -- role claim defaults to anon if not specified in jwt
      H.sql . mconcat . claimsToSQL $ M.union claims (M.singleton "role" anon)
      app req
  where
    anon = String . toS $ configAnonRole conf

defaultMiddle :: Application -> Application
defaultMiddle =
    gzip def
  . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
