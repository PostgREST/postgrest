{-|
Module      : PostgREST.Middleware
Description : Sets CORS policy. Also the PostgreSQL GUCs, role, search_path and pre-request function.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.CaseInsensitive      as CI
import           Data.Function             (id)
import qualified Data.HashMap.Strict       as M
import           Data.List                 (lookup)
import           Data.Scientific           (FPFormat (..),
                                            formatScientific,
                                            isInteger)
import qualified Data.Text                 as T
import qualified Hasql.Transaction         as H
import           Network.HTTP.Types.Status (statusCode)
import           Network.Wai.Logger        (showSockAddr)
import           System.Log.FastLogger     (toLogStr)

import Network.Wai
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..),
                                             cors)
import Network.Wai.Middleware.Gzip          (def, gzip)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static        (only, staticPolicy)

import PostgREST.ApiRequest   (ApiRequest (..))
import PostgREST.Config       (AppConfig (..))
import PostgREST.QueryBuilder (setLocalQuery, setLocalSearchPathQuery)
import PostgREST.Types        (LogLevel (..))
import Protolude              hiding (head, toS)
import Protolude.Conv         (toS)
import System.IO.Unsafe       (unsafePerformIO)

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

-- | Log in apache format. Only requests with a failure status.
-- | There's no easy way to filter logs in the apache format on https://hackage.haskell.org/package/wai-extra-3.0.29.2/docs/Network-Wai-Middleware-RequestLogger.html#t:OutputFormat.
-- | So here we copy https://github.com/kazu-yamamoto/logger/blob/a4f51b909a099c51af7a3f75cf16e19a06f9e257/wai-logger/Network/Wai/Logger/Apache.hs#L45
-- | TODO: Add the ability to filter apache logs on wai-extra and remove this function.
pgrstFormat :: OutputFormatter
pgrstFormat date req status responseSize =
  if statusCode status < 400
    then toLogStr BS.empty
  else toLogStr (getSourceFromSocket req)
    <> " - - ["
    <> toLogStr date
    <> "] \""
    <> toLogStr (requestMethod req)
    <> " "
    <> toLogStr (rawPathInfo req <> rawQueryString req)
    <> " "
    <> toLogStr (show (httpVersion req)::Text)
    <> "\" "
    <> toLogStr (show (statusCode status)::Text)
    <> " "
    <> toLogStr (maybe "-" show responseSize::Text)
    <> " \""
    <> toLogStr (fromMaybe mempty $ requestHeaderReferer req)
    <> "\" \""
    <> toLogStr (fromMaybe mempty $ requestHeaderUserAgent req)
    <> "\"\n"
  where
    getSourceFromSocket = BS.pack . showSockAddr . remoteHost

pgrstMiddleware :: LogLevel -> Application -> Application
pgrstMiddleware logLevel =
    logger
  . gzip def
  . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
  where
    logger = case logLevel of
      LogCrit  -> id
      LogError -> unsafePerformIO $ mkRequestLogger def { outputFormat = CustomOutputFormat pgrstFormat }
      LogInfo  -> logStdout

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
      Just hdrs -> map (CI.mk . toS . T.strip . toS) $ BS.split ',' hdrs
      Nothing -> []

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = show b
unquoted v = toS $ JSON.encode v
