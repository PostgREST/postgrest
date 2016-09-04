{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import           Data.Aeson                    (Value (..))
import qualified Data.HashMap.Strict           as M
import qualified Hasql.Transaction             as H

import           Network.HTTP.Types.Status     (unauthorized401)
import           Network.Wai                   (Application, Response,
                                                responseLBS)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.ApiRequest          (ApiRequest(..), ContentType(..),
                                                ctToHeader)
import           PostgREST.Auth                (claimsToSQL, JWTAttempt(..))
import           PostgREST.Config              (AppConfig (..), corsPolicy)

import           Protolude                     hiding (concat, null)

runWithClaims :: AppConfig -> JWTAttempt ->
                 (ApiRequest -> H.Transaction Response) ->
                 ApiRequest -> H.Transaction Response
runWithClaims conf eClaims app req =
  case eClaims of
    JWTExpired -> return $ unauthed "JWT expired"
    JWTInvalid -> return $ unauthed "JWT invalid"
    JWTClaims claims -> do
      -- role claim defaults to anon if not specified in jwt
      H.sql . mconcat . claimsToSQL $ M.union claims (M.singleton "role" anon)
      app req
  where
    anon = String . toS $ configAnonRole conf
    unauthed message = responseLBS unauthorized401
      [ ctToHeader CTApplicationJSON
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
