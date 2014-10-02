{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Middleware where

import Data.Aeson

import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status400)
import Database.HDBC.Types (SqlError(..))
import Control.Exception (catchJust)
import Network.Wai


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
