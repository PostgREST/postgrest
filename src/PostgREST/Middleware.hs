{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import           Data.Aeson                    (Value (..))
import qualified Data.HashMap.Strict           as M
import           Data.String.Conversions       (cs)
import           Data.Text
import           Data.Time.Clock               (NominalDiffTime)
import qualified Hasql.Transaction             as H

import           Network.HTTP.Types.Header     (hAccept)
import           Network.HTTP.Types.Status     (status400, status415)
import           Network.Wai                   (Application, Request (..),
                                                Response, requestHeaders)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.ApiRequest          (ApiRequest(..), pickContentType)
import           PostgREST.Auth                (jwtClaims, claimsToSQL)
import           PostgREST.Config              (AppConfig (..), corsPolicy)
import           PostgREST.Error               (errResponse)

import           Prelude                       hiding (concat, null)

runWithClaims :: AppConfig -> NominalDiffTime ->
                 (ApiRequest -> H.Transaction Response) ->
                 ApiRequest -> H.Transaction Response
runWithClaims conf time app req = do
    let eClaims = jwtClaims jwtSecret (iJWT req) time
    case eClaims of
      Left e -> clientErr e
      Right claims ->
        if M.null claims && not (null $ iJWT req)
          then clientErr "Invalid JWT"
          else do
            -- role claim defaults to anon if not specified in jwt
            H.sql . mconcat . claimsToSQL $ M.union claims (M.singleton "role" anon)
            app req
  where
    jwtSecret = configJwtSecret conf
    anon = String . cs $ configAnonRole conf
    clientErr = return . errResponse status400

unsupportedAccept :: Application -> Application
unsupportedAccept app req respond =
  case accept of
    Left _ -> respond $ errResponse status415 "Unsupported Accept header, try: application/json"
    Right _ -> app req respond
  where accept = pickContentType $ lookup hAccept $ requestHeaders req

defaultMiddle :: Application -> Application
defaultMiddle =
    gzip def
  . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
  . unsupportedAccept
