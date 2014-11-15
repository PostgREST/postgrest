{-# OPTIONS_GHC -fno-warn-orphans #-}

module Middleware where

--import Data.Aeson ((.=), toJSON, ToJSON, object, encode)
-- import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
-- import Data.Pool(withResource, Pool)

import qualified Hasql as H
import Data.String.Conversions(cs)
--import qualified Data.ByteString.Char8 as BS
import Control.Exception (catchJust)

import Network.HTTP.Types.Header (hLocation, hContentType)
import Network.HTTP.Types.Status (status400, status301)
import Network.Wai (Application, requestHeaders, responseLBS, rawPathInfo,
                   rawQueryString, isSecure)
import Network.URI (URI(..), parseURI)

-- import Auth (LoginAttempt(..), signInRole, setRole, resetRole)
-- import Codec.Binary.Base64.String (decode)

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

-- authenticated :: BS.ByteString -> (Connection -> Application) ->
--                  Connection -> Application
-- authenticated anon app conn req respond = do
--   attempt <- httpRequesterRole (requestHeaders req)
--   case attempt of
--     MalformedAuth ->
--       respond $ responseLBS status400 [] "Malformed basic auth header"
--     LoginFailed ->
--       respond $ responseLBS status401 [] "Invalid username or password"
--     LoginSuccess role ->
--       bracket_ (setRole conn role) (resetRole conn) $ app conn req respond
--     NoCredentials ->
--       bracket_ (setRole conn anon) (resetRole conn) $ app conn req respond

--  where
--    httpRequesterRole :: RequestHeaders -> IO LoginAttempt
--    httpRequesterRole hdrs = do
--     let auth = fromMaybe "" $ lookup hAuthorization hdrs
--     case BS.split ' ' (cs auth) of
--       ("Basic" : b64 : _) ->
--         case BS.split ':' $ cs (decode $ cs b64) of
--           (u:p:_) -> signInRole conn u p
--           _ -> return MalformedAuth
--       _ -> return NoCredentials

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
