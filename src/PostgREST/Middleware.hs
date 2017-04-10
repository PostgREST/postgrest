{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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

import           PostgREST.ApiRequest          (ApiRequest(..))
import           PostgREST.Auth                (JWTAttempt(..))
import           PostgREST.Config              (AppConfig (..), corsPolicy)
import           PostgREST.Error               (simpleError)
import           PostgREST.Types               (ContentType (..), toHeader)
import           PostgREST.QueryBuilder        (pgFmtLit, unquoted, pgFmtEnvVar)

import           Protolude                     hiding (concat, null)

runWithClaims :: AppConfig -> JWTAttempt ->
                 (ApiRequest -> H.Transaction Response) ->
                 ApiRequest -> H.Transaction Response
runWithClaims conf eClaims app req =
  case eClaims of
    JWTExpired -> return $ unauthed "JWT expired"
    JWTInvalid -> return $ unauthed "JWT invalid"
    JWTMissingSecret -> return $ simpleError status500 "Server lacks JWT secret"
    JWTClaims claims -> do
      H.sql $ toS.mconcat $ setRoleSql ++ claimsSql ++ headersSql ++ cookiesSql
      mapM_ H.sql customReqCheck
      app req
      where
        headersSql = map (pgFmtEnvVar "request.header.") $ iHeaders req
        cookiesSql = map (pgFmtEnvVar "request.cookie.") $ iCookies req
        claimsSql = map (pgFmtEnvVar "request.jwt.claim.") [(c,unquoted v) | (c,v) <- M.toList claimsWithRole]
        setRoleSql = maybeToList $
          (\r -> "set local role " <> r <> ";") . toS . pgFmtLit . unquoted <$> M.lookup "role" claimsWithRole
        -- role claim defaults to anon if not specified in jwt
        claimsWithRole = M.union claims (M.singleton "role" anon)
        anon = String . toS $ configAnonRole conf
        customReqCheck = (\f -> "select " <> toS f <> "();") <$> configReqCheck conf
  where
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
