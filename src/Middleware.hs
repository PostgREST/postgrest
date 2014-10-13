{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Middleware where

import Data.Aeson

import Network.HTTP.Types.Header (hContentType, hLocation)
import Network.HTTP.Types.Status (status400, status301)
import Database.HDBC.Types (SqlError(..))
import Control.Exception (catchJust)
import Data.String.Conversions (cs)
import Network.Wai
import Network.URI (URI(..), parseURI)
import Data.Monoid (mconcat)

instance ToJSON SqlError where
  toJSON t = object [
      "error" .= object [
          "code"    .= seNativeError t
        , "message" .= seErrorMsg t
        , "state"   .= seState t
      ]
    ]

reportPgErrors :: Middleware
reportPgErrors app req respond =
  catchJust isPgException (app req respond) (
      respond . responseLBS status400 [(hContentType, "application/json")]
              . encode
    )

  where
    isPgException :: SqlError -> Maybe SqlError
    isPgException = Just


redirectInsecure :: Middleware
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
