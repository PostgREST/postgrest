{-|
Module      : PostgREST.Middleware
Description : Sets the PostgreSQL GUCs, role, search_path and pre-request function. Validates JWT.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module PostgREST.Middleware where

import           Crypto.JWT
import qualified Data.Aeson                    as JSON
import qualified Data.HashMap.Strict           as M
import qualified Hasql.Transaction             as H

import           Network.HTTP.Types.Status     (unauthorized401, status500)
import           Network.Wai                   (Application, Response)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.ApiRequest          (ApiRequest(..))
import           PostgREST.Auth                (JWTAttempt(..))
import           PostgREST.Config              (AppConfig (..), corsPolicy)
import           PostgREST.Error               (simpleError)
import           PostgREST.QueryBuilder        (unquoted, pgFmtSetLocal, pgFmtSetLocalSearchPath)

import           Protolude

runWithClaims :: AppConfig -> JWTAttempt ->
                 (ApiRequest -> H.Transaction Response) ->
                 ApiRequest -> H.Transaction Response
runWithClaims conf eClaims app req =
  case eClaims of
    JWTInvalid JWTExpired -> return $ unauthed "JWT expired"
    JWTInvalid e -> return $ unauthed $ show e
    JWTMissingSecret -> return $ simpleError status500 [] "Server lacks JWT secret"
    JWTClaims claims -> do
      H.sql $ toS . mconcat $ setSearchPathSql : setRoleSql ++ claimsSql ++ headersSql ++ cookiesSql ++ appSettingsSql
      mapM_ H.sql customReqCheck
      app req
      where
        headersSql = pgFmtSetLocal "request.header." <$> iHeaders req
        cookiesSql = pgFmtSetLocal "request.cookie." <$> iCookies req
        claimsSql = pgFmtSetLocal "request.jwt.claim." <$> [(c,unquoted v) | (c,v) <- M.toList claimsWithRole]
        appSettingsSql = pgFmtSetLocal mempty <$> configSettings conf
        setRoleSql = maybeToList $ (\x ->
          pgFmtSetLocal mempty ("role", unquoted x)) <$> M.lookup "role" claimsWithRole
        setSearchPathSql = pgFmtSetLocalSearchPath $ configSchema conf : configExtraSearchPath conf
        -- role claim defaults to anon if not specified in jwt
        claimsWithRole = M.union claims (M.singleton "role" anon)
        anon = JSON.String . toS $ configAnonRole conf
        customReqCheck = (\f -> "select " <> toS f <> "();") <$> configReqCheck conf
  where
    unauthed message = simpleError
      unauthorized401
      [( "WWW-Authenticate"
        , "Bearer error=\"invalid_token\", " <>
          "error_description=" <> show message
      )]
      message

defaultMiddle :: Application -> Application
defaultMiddle =
    gzip def
  . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
