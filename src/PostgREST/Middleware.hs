{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.Middleware where

import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid
import Data.Text
-- import Data.Pool(withResource, Pool)

import qualified Hasql as H
import qualified Hasql.Postgres as P
import Data.String.Conversions(cs)

import Network.HTTP.Types.Header (hLocation, hAuthorization, hAccept)
import Network.HTTP.Types (RequestHeaders)
import Network.HTTP.Types.Status (status400, status401, status301, status415)
import Network.Wai (Application, requestHeaders, responseLBS, rawPathInfo,
                   rawQueryString, isSecure, Request(..), Response)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Middleware.Static (staticPolicy, only)
import Network.URI (URI(..), parseURI)

import PostgREST.Config (AppConfig(..), corsPolicy)
import PostgREST.Auth (LoginAttempt(..), signInRole, signInWithJWT, setRole, resetRole, setUserId, resetUserId)
import PostgREST.App (contentTypeForAccept)
import Codec.Binary.Base64.String (decode)

import Prelude

authenticated :: forall s. AppConfig ->
                 (Request -> H.Tx P.Postgres s Response) ->
                 Request -> H.Tx P.Postgres s Response
authenticated conf app req = do
  attempt <- httpRequesterRole (requestHeaders req)
  case attempt of
    MalformedAuth ->
      return $ responseLBS status400 [] "Malformed basic auth header"
    LoginFailed ->
      return $ responseLBS status401 [] "Invalid username or password"
    LoginSuccess role uid -> if role /= currentRole then runInRole role uid else app req
    NoCredentials         -> if anon /= currentRole then runInRole anon "" else app req

 where
   jwtSecret = cs $ configJwtSecret conf
   currentRole = cs $ configDbUser conf
   anon = cs $ configAnonRole conf
   httpRequesterRole :: RequestHeaders -> H.Tx P.Postgres s LoginAttempt
   httpRequesterRole hdrs = do
    let auth = fromMaybe "" $ lookup hAuthorization hdrs
    case split (==' ') (cs auth) of
      ("Basic" : b64 : _) ->
        case split (==':') (cs . decode . cs $ b64) of
          (u:p:_) -> signInRole u p
          _ -> return MalformedAuth
      ("Bearer" : jwt : _) ->
        return $ signInWithJWT jwtSecret jwt
      _ -> return NoCredentials

   runInRole :: Text -> Text -> H.Tx P.Postgres s Response
   runInRole r uid = do
     setUserId uid
     setRole r
     res <- app req
     resetRole
     resetUserId
     return res


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
