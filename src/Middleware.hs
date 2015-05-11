{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Middleware where

import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Text
-- import Data.Pool(withResource, Pool)

import qualified Hasql as H
import qualified Hasql.Postgres as P
import Data.String.Conversions(cs)

import Network.HTTP.Types.Header (hLocation, hAuthorization)
import Network.HTTP.Types (RequestHeaders)
import Network.HTTP.Types.Status (status400, status401, status301)
import Network.Wai (Application, requestHeaders, responseLBS, rawPathInfo,
                   rawQueryString, isSecure, Request(..), Response)
import Network.URI (URI(..), parseURI)

import Auth (LoginAttempt(..), signInRole, setRole, resetRole)
import Codec.Binary.Base64.String (decode)

authenticated :: forall s. Text -> Text ->
                 (Request -> H.Tx P.Postgres s Response) ->
                 Request -> H.Tx P.Postgres s Response
authenticated currentRole anon app req = do
    if anon /= currentRole then runInRole anon else app req

 where
   httpRequesterRole :: RequestHeaders -> H.Tx P.Postgres s LoginAttempt
   httpRequesterRole hdrs = do
    let auth = fromMaybe "" $ lookup hAuthorization hdrs
    case split (==' ') (cs auth) of
      ("Basic" : b64 : _) ->
        case split (==':') (cs . decode . cs $ b64) of
          (u:p:_) -> signInRole u p
          _ -> return MalformedAuth
      _ -> return NoCredentials

   runInRole :: Text -> H.Tx P.Postgres s Response
   runInRole r = do
     setRole r
     res <- app req
     resetRole
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
