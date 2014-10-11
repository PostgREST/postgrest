{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Middleware where

import Data.Aeson

import Database.HDBC (runRaw)
import Database.HDBC.PostgreSQL (Connection)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status400)
import Database.HDBC.Types (SqlError(..))
import Network.Wai (Application, Request, Response, ResponseReceived, responseLBS)
import Control.Exception (finally, catchJust)

type ResHandler = Response -> IO ResponseReceived

inTransaction :: Connection -> (Connection -> Application) -> Request -> ResHandler -> IO ResponseReceived
inTransaction conn app req respond =
  finally (putStrLn "begin txn" >> runRaw conn "begin" >> app conn req respond) (putStrLn "commit txn" >> runRaw conn "commit")

instance ToJSON SqlError where
  toJSON t = object [
      "error" .= object [
          "code"    .= seNativeError t
        , "message" .= seErrorMsg t
        , "state"   .= seState t
      ]
    ]

reportPgErrors :: Application -> Request -> ResHandler -> IO ResponseReceived
reportPgErrors app req respond =
  catchJust isPgException (app req respond) (
      respond . responseLBS status400 [(hContentType, "application/json")]
              . encode
    )

  where
    isPgException :: SqlError -> Maybe SqlError
    isPgException = Just
