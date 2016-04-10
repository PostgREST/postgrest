{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import           Data.Aeson                    (Value (..))
import qualified Data.HashMap.Strict           as M
import           Data.Maybe                    (fromMaybe)
import           Data.String.Conversions       (cs)
import           Data.Text
import           Data.Time.Clock               (NominalDiffTime)
import qualified Hasql.Transaction             as H

import           Network.HTTP.Types.Header     (hAccept, hAuthorization)
import           Network.HTTP.Types.Status     (status400, status415)
import           Network.Wai                   (Application, Request (..),
                                                Response, requestHeaders)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.ApiRequest          (pickContentType)
import           PostgREST.Auth                (setRole, jwtClaims, claimsToSQL)
import           PostgREST.Config              (AppConfig (..), corsPolicy)
import           PostgREST.Error               (errResponse)

import           Prelude                       hiding (concat, null)

runWithClaims :: AppConfig -> NominalDiffTime ->
                 (Request -> H.Transaction Response) ->
                 Request -> H.Transaction Response
runWithClaims conf time app req = do
    let tokenStr = case split (== ' ') (cs auth) of
          ("Bearer" : t : _) -> t
          _                  -> ""
        eClaims = jwtClaims jwtSecret tokenStr time
    case eClaims of
      Left e -> clientErr e
      Right claims ->
        if M.null claims && not (null tokenStr)
          then clientErr "Invalid JWT"
          else do
            H.sql . mconcat $
              setRole (
                fromMaybe anon (M.lookup "role" claims)
              ) : claimsToSQL (M.delete "role" claims)
            app req
  where
    hdrs = requestHeaders req
    jwtSecret = configJwtSecret conf
    auth = fromMaybe "" $ lookup hAuthorization hdrs
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
