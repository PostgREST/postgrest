{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import           Data.Aeson                    (Value (..))
import qualified Data.HashMap.Strict           as M
import qualified Hasql.Transaction             as H

import           Network.HTTP.Types.Status     (unauthorized401, status500)
import           Network.Wai                   (Application, Response,
                                                responseLBS)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.ApiRequest          (ApiRequest(..), ContentType(..),
                                                toHeader)
import           PostgREST.Auth                (claimsToSQL, JWTAttempt(..))
import           PostgREST.Config              (AppConfig (..), corsPolicy)
import           PostgREST.Error               (errResponse)

import           Protolude                     hiding (concat, null)

runWithClaims :: AppConfig -> JWTAttempt ->
                 (ApiRequest -> H.Transaction Response) ->
                 ApiRequest -> H.Transaction Response
runWithClaims conf eClaims app req =
  case eClaims of
    JWTExpired -> return $ unauthed "JWT expired"
    JWTInvalid -> return $ unauthed "JWT invalid"
    JWTMissingSecret -> return $ errResponse status500 "Server lacks JWT secret"
    JWTClaims claims -> do
      -- role claim defaults to anon if not specified in jwt
      let setClaims = claimsToSQL (M.union claims (M.singleton "role" anon))
      H.sql $ mconcat setClaims
      mapM_ H.sql customReqCheck
      app req
  where
    anon = String . toS $ configAnonRole conf
    customReqCheck = (\f -> "select " <> toS f <> "();") <$> configReqCheck conf
    unauthed message = responseLBS unauthorized401
      [ toHeader CTApplicationJSON
      , ( "WWW-Authenticate"
        , "Bearer error=\"invalid_token\", " <>
          "error_description=\"" <> message <> "\""
        )
      ]
      (toS $ "{\"message\":\""<>message<>"\"}")

defaultMiddle :: Application -> Application
defaultMiddle =
    gzip def
  . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
