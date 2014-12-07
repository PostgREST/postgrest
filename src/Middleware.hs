{-# OPTIONS_GHC -fno-warn-orphans #-}

module Middleware where

--import Data.Aeson ((.=), toJSON, ToJSON, object, encode)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Text
-- import Data.Pool(withResource, Pool)

import qualified Hasql as H
import qualified Hasql.Postgres as H
import Data.String.Conversions(cs)
import Control.Exception (catchJust)

import Network.HTTP.Types.Header (hLocation, hContentType, hAuthorization)
import Network.HTTP.Types (RequestHeaders)
import Network.HTTP.Types.Status (status400, status401, status301)
import Network.Wai (Application, requestHeaders, responseLBS, rawPathInfo,
                   rawQueryString, isSecure, Request(..), Response)
import Network.URI (URI(..), parseURI)

import Auth (LoginAttempt(..), signInRole, setRole, resetRole)
import Codec.Binary.Base64.String (decode)

import Debug.Trace

-- data Environment = Test | Production deriving (Eq)

-- safeAction :: Request -> Bool
-- safeAction = (`notElem` ["PATCH", "PUT"]) . requestMethod

-- withSavepoint :: Environment -> (Connection -> Application) ->
--                  Connection -> Application
-- withSavepoint env app conn req respond =
--   if env == Production && safeAction req
--     then go
--     else Database.PostgreSQL.Simple.withSavepoint conn go
--   where go = app conn req respond

authenticated :: Text -> (Request -> H.Session H.Postgres IO Response) ->
       Request -> H.Session H.Postgres IO Response
authenticated anon app req = do
  attempt <- httpRequesterRole (requestHeaders req)
  case attempt of
    MalformedAuth ->
      return $ responseLBS status400 [] "Malformed basic auth header"
    LoginFailed ->
      return $ responseLBS status401 [] "Invalid username or password"
    LoginSuccess role -> runInRole role
    NoCredentials -> runInRole anon

 where
   httpRequesterRole :: RequestHeaders -> H.Session H.Postgres IO LoginAttempt
   httpRequesterRole hdrs = do
    let auth = fromMaybe "" $ lookup hAuthorization hdrs
    case split (==' ') (cs auth) of
      ("Basic" : b64 : _) ->
        case split (==':') (cs . decode . cs $ b64) of
          (u:p:_) -> H.tx Nothing $ signInRole u p
          _ -> return MalformedAuth
      _ -> return NoCredentials

   runInRole :: Text -> H.Session H.Postgres IO Response
   runInRole r = do
     H.tx Nothing $ setRole r
     resp <- app req
     H.tx Nothing resetRole
     return resp

-- instance ToJSON SqlError where
--   toJSON t = object [
--       "error" .= object [
--           "message" .= (cs $ sqlErrorMsg t :: String)
--         , "detail"  .= (cs $ sqlErrorDetail t :: String)
--         , "state"   .= (cs $ sqlState t :: String)
--         , "hint"    .= (cs $ sqlErrorHint t :: String)
--       ]
--     ]

clientErrors :: Application -> Application
clientErrors app req respond =
  catchJust isPgException (app req respond) $ \err ->
    respond $
      responseLBS status400 [(hContentType, "application/json")] (cs $ show err)
      -- if sqlState err == "42P01"
      --  then responseLBS status404 [] ""
      --  else responseLBS status400 [(hContentType, "application/json")] (encode err)

  where
    isPgException :: H.Error -> Maybe H.Error
    isPgException x = Just (traceShow x x)


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
