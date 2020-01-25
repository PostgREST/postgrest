{-|
Module      : PostgREST.Middleware
Description : Sets the PostgreSQL GUCs, role, search_path and pre-request function. Validates JWT.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as M
import           Data.Scientific     (FPFormat (..), formatScientific,
                                      isInteger)

import Network.Wai                   (Application, Response)
import Network.Wai.Middleware.Cors   (cors)
import Network.Wai.Middleware.Gzip   (def, gzip)
import Network.Wai.Middleware.Static (only, staticPolicy)

import Crypto.JWT

import PostgREST.ApiRequest   (ApiRequest (..))
import PostgREST.Auth         (JWTAttempt (..))
import PostgREST.Config       (AppConfig (..), corsPolicy)
import PostgREST.Error        (SimpleError (JwtTokenInvalid, JwtTokenMissing),
                               errorResponseFor)
import PostgREST.QueryBuilder (setLocalQuery, setLocalSearchPathQuery)
import Protolude

-- | m could be Hasql.Transaction or Hasql.Session 
runWithClaims :: Monad m =>
                 (ByteString -> m ()) ->
                 AppConfig -> JWTAttempt ->
                 (ApiRequest -> m Response) ->
                 ApiRequest -> m Response
runWithClaims bM conf eClaims app req =
  case eClaims of
    JWTMissingSecret      -> return . errorResponseFor $ JwtTokenMissing
    JWTInvalid JWTExpired -> return . errorResponseFor . JwtTokenInvalid $ "JWT expired"
    JWTInvalid e          -> return . errorResponseFor . JwtTokenInvalid . show $ e
    JWTClaims claims      -> do
      bM $ toS . mconcat $ setSearchPathSql : setRoleSql ++ claimsSql ++ headersSql ++ cookiesSql ++ appSettingsSql
      mapM_ bM customReqCheck
      app req
      where
        headersSql = setLocalQuery "request.header." <$> iHeaders req
        cookiesSql = setLocalQuery "request.cookie." <$> iCookies req
        claimsSql = setLocalQuery "request.jwt.claim." <$> [(c,unquoted v) | (c,v) <- M.toList claimsWithRole]
        appSettingsSql = setLocalQuery mempty <$> configSettings conf
        setRoleSql = maybeToList $ (\x ->
          setLocalQuery mempty ("role", unquoted x)) <$> M.lookup "role" claimsWithRole
        setSearchPathSql = setLocalSearchPathQuery $ configSchema conf : configExtraSearchPath conf
        -- role claim defaults to anon if not specified in jwt
        claimsWithRole = M.union claims (M.singleton "role" anon)
        anon = JSON.String . toS $ configAnonRole conf
        customReqCheck = (\f -> "select " <> toS f <> "();") <$> configReqCheck conf

defaultMiddle :: Application -> Application
defaultMiddle =
    gzip def
  . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = show b
unquoted v = toS $ JSON.encode v
