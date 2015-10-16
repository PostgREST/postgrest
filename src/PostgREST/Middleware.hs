{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Monoid
import           Data.Text
import           Data.String.Conversions       (cs)
import qualified Hasql                         as H
import qualified Hasql.Postgres                as P

import           Network.HTTP.Types.Header     (hAccept, hAuthorization,
                                                hLocation)
import           Network.HTTP.Types.Status     (status301, status400, status415)
import           Network.URI                   (URI (..), parseURI)
import           Network.Wai                   (Application, Request (..),
                                                Response, isSecure, rawPathInfo,
                                                rawQueryString, requestHeaders,
                                                responseLBS)
import           Network.Wai.Middleware.Cors   (cors)
import           Network.Wai.Middleware.Gzip   (def, gzip)
import           Network.Wai.Middleware.Static (only, staticPolicy)

import           PostgREST.App                 (contentTypeForAccept)
import           PostgREST.Auth                (setRole, setJWTEnv)
import           PostgREST.Config              (AppConfig (..), corsPolicy)

import           Prelude hiding(concat)

import qualified Data.Vector             as V
import qualified Hasql.Backend           as B

runWithClaims :: forall s. AppConfig ->
                 (Request -> H.Tx P.Postgres s Response) ->
                 Request -> H.Tx P.Postgres s Response
runWithClaims conf app req = do
    H.unitEx $ B.Stmt env V.empty True
    app req
 where
   hdrs = requestHeaders req
   jwtSecret = (cs $ configJwtSecret conf) :: Text
   auth = fromMaybe "" $ lookup hAuthorization hdrs
   anon = cs $ configAnonRole conf
   jwtEnv =
     case split (==' ') (cs auth) of
       ("Bearer" : jwt : _) -> fromMaybe [] (setJWTEnv jwtSecret jwt)
       _ -> []
   env = concat $ setRole anon : jwtEnv

redirectInsecure :: Application -> Application
redirectInsecure app req respond = do
  let hdrs = requestHeaders req
      host = lookup "host" hdrs
      uriM = parseURI . cs =<< mconcat [
        Just "https://",
        host,
        Just $ rawPathInfo req,
        Just $ rawQueryString req]
      isHerokuSecure = lookup "x-forwarded-proto" hdrs == Just "https"

  if not (isSecure req || isHerokuSecure)
    then case uriM of
      Just uri ->
        respond $ responseLBS status301 [
            (hLocation, cs . show $ uri { uriScheme = "https:" })
          ] ""
      Nothing ->
        respond $ responseLBS status400 [] "SSL is required"
    else app req respond

unsupportedAccept :: Application -> Application
unsupportedAccept app req respond = do
  let
    accept = lookup hAccept $ requestHeaders req
  if isNothing $ contentTypeForAccept accept
  then respond $ responseLBS status415 [] "Unsupported Accept header, try: application/json"
  else app req respond

defaultMiddle :: Bool -> Application -> Application
defaultMiddle secure = (if secure then redirectInsecure else id)
  . gzip def . cors corsPolicy
  . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
  . unsupportedAccept
