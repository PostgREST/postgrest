{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import           Data.Aeson                    (Value (..))
import qualified Data.HashMap.Strict           as M
import           Data.String.Conversions       (cs)
import qualified Hasql.Transaction             as H

import           Network.HTTP.Types.Header     (hAccept)
import           Network.HTTP.Types.Status     (status400, status415)
import           Network.Wai                   (Application, Request (..),
                                                Response, requestHeaders)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.ApiRequest          (ApiRequest(..), ContentType(..), pickContentType)
import           PostgREST.Auth                (claimsToSQL)
import           PostgREST.Config              (AppConfig (..), corsPolicy)
import           PostgREST.Error               (errResponse)
import           Data.Text
import           Data.List (lookup)

import           Protolude                     hiding (concat, null)

runWithClaims :: AppConfig -> Either Text (M.HashMap Text Value) ->
                 (ApiRequest -> H.Transaction Response) ->
                 ApiRequest -> H.Transaction Response
runWithClaims conf eClaims app req =
  case eClaims of
    Left e -> clientErr e
    Right claims -> do
          -- role claim defaults to anon if not specified in jwt
          H.sql . mconcat . claimsToSQL $ M.union claims (M.singleton "role" anon)
          app req
  where
    anon = String . cs $ configAnonRole conf
    clientErr = return . errResponse status400

unsupportedAccept :: Application -> Application
unsupportedAccept app req respond =
  case (isTargetRoot, accept) of
    (_, Left _) -> unsupportedAcceptRespond
    (False, Right OpenAPI) -> unsupportedAcceptRespond
    (_, Right _) -> app req respond
  where accept = pickContentType $ lookup hAccept $ requestHeaders req
        path = pathInfo req
        isTargetRoot = fromMaybe True $ (== "") <$> listToMaybe path
        unsupportedAcceptRespond = respond $ errResponse status415 "Unsupported Accept header, try: application/json"

defaultMiddle :: Application -> Application
defaultMiddle =
    gzip def
  . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
  . unsupportedAccept
