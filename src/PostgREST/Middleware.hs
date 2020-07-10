{-|
Module      : PostgREST.Middleware
Description : Sets CORS policy. Also the PostgreSQL GUCs, role, search_path and pre-request function.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive  as CI
import           Data.Function         (id)
import qualified Data.HashMap.Strict   as M
import           Data.List             (lookup)
import           Data.Scientific       (FPFormat (..),
                                        formatScientific, isInteger)
import           Data.Text             (strip)
import qualified Hasql.Transaction     as H

import Network.Wai                          (Application, Request,
                                             Response, requestHeaders)
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..),
                                             cors)
import Network.Wai.Middleware.Gzip          (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static        (only, staticPolicy)

import PostgREST.ApiRequest   (ApiRequest (..))
import PostgREST.Config       (AppConfig (..))
import PostgREST.QueryBuilder (setLocalQuery, setLocalSearchPathQuery)
import PostgREST.Types        (LogSetup (..))
import Protolude              hiding (head, toS)
import Protolude.Conv         (toS)

-- | Runs local(transaction scoped) GUCs for every request, plus the pre-request function
runPgLocals :: AppConfig   -> M.HashMap Text JSON.Value ->
               (ApiRequest -> H.Transaction Response) ->
               ApiRequest  -> H.Transaction Response
runPgLocals conf claims app req = do
  H.sql $ toS . mconcat $ setSearchPathSql : setRoleSql ++ claimsSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ appSettingsSql
  traverse_ H.sql preReq
  app req
  where
    methodSql = setLocalQuery mempty ("request.method", toS $ iMethod req)
    pathSql = setLocalQuery mempty ("request.path", toS $ iPath req)
    headersSql = setLocalQuery "request.header." <$> iHeaders req
    cookiesSql = setLocalQuery "request.cookie." <$> iCookies req
    claimsSql = setLocalQuery "request.jwt.claim." <$> [(c,unquoted v) | (c,v) <- M.toList claimsWithRole]
    appSettingsSql = setLocalQuery mempty <$> configSettings conf
    setRoleSql = maybeToList $ (\x ->
      setLocalQuery mempty ("role", unquoted x)) <$> M.lookup "role" claimsWithRole
    setSearchPathSql = setLocalSearchPathQuery (iSchema req : configExtraSearchPath conf)
    -- role claim defaults to anon if not specified in jwt
    claimsWithRole = M.union claims (M.singleton "role" anon)
    anon = JSON.String . toS $ configAnonRole conf
    preReq = (\f -> "select " <> toS f <> "();") <$> configPreReq conf

pgrstMiddleware :: LogSetup -> Application -> Application
pgrstMiddleware logs =
    (if logs == LogQuiet then id else logStdout)
  . gzip def
  . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PATCH", "PUT", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

-- | CORS policy to be used in by Wai Cors middleware
corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      corsOrigins = Just ([origin], True)
    , corsRequestHeaders = "Authentication":accHeaders
    , corsExposedHeaders = Just [
        "Content-Encoding", "Content-Location", "Content-Range", "Content-Type"
      , "Date", "Location", "Server", "Transfer-Encoding", "Range-Unit"
      ]
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . toS . strip . toS) $ BS.split ',' hdrs
      Nothing -> []

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = show b
unquoted v = toS $ JSON.encode v
