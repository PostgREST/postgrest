{-|
Module      : PostgREST.Error
Description : PostgREST error HTTP responses
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PostgREST.Error (
  apiRequestError
, pgError
, simpleError
, singularityError
, binaryFieldError
, connectionLostError
, encodeError
, gucHeadersError
) where

import           Protolude
import           Data.Aeson                ((.=))
import qualified Data.Aeson                as JSON
import           Data.Text                 (unwords)
import qualified Hasql.Pool                as P
import qualified Hasql.Session             as H
import           Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Status as HT
import           Network.Wai               (Response, responseLBS)
import           PostgREST.Types
import           Text.Read                 (readMaybe)

apiRequestError :: ApiRequestError -> Response
apiRequestError err =
  errorResponse status
    [toHeader CTApplicationJSON] err
  where
    status =
      case err of
        ActionInappropriate -> HT.status405
        UnsupportedVerb -> HT.status405
        InvalidBody _ -> HT.status400
        ParseRequestError _ _ -> HT.status400
        NoRelationBetween _ _ -> HT.status400
        InvalidRange -> HT.status416
        UnknownRelation -> HT.status404
        InvalidFilters -> HT.status405

simpleError :: HT.Status -> [Header] -> Text -> Response
simpleError status hdrs message =
  errorResponse status (toHeader CTApplicationJSON : hdrs) $
    JSON.object ["message" .= message]

errorResponse :: JSON.ToJSON a => HT.Status -> [Header] -> a -> Response
errorResponse status hdrs e =
  responseLBS status hdrs $ encodeError e

pgError :: Bool -> P.UsageError -> Response
pgError authed e =
  let status = httpStatus authed e
      jsonType = toHeader CTApplicationJSON
      wwwAuth = ("WWW-Authenticate", "Bearer")
      hdrs = if status == HT.status401
                then [jsonType, wwwAuth]
                else [jsonType] in
  responseLBS status hdrs (encodeError e)

singularityError :: (Integral a, Show a) => a -> Response
singularityError numRows =
  responseLBS HT.status406
    [toHeader CTSingularJSON]
    $ toS . formatGeneralError
      "JSON object requested, multiple (or no) rows returned"
      $ unwords
        [ "Results contain", show numRows, "rows,"
        , toS (toMime CTSingularJSON), "requires 1 row"
        ]
  where
    formatGeneralError :: Text -> Text -> Text
    formatGeneralError message details = toS . JSON.encode $
      JSON.object ["message" .= message, "details" .= details]


binaryFieldError :: Response
binaryFieldError =
  simpleError HT.status406 [] (toS (toMime CTOctetStream) <>
  " requested but a single column was not selected")

gucHeadersError :: Response
gucHeadersError =
 simpleError HT.status500 []
 "response.headers guc must be a JSON array composed of objects with a single key and a string value"

connectionLostError :: Response
connectionLostError =
  simpleError HT.status503 [] "Database connection lost, retrying the connection."

encodeError :: JSON.ToJSON a => a -> LByteString
encodeError = JSON.encode

instance JSON.ToJSON ApiRequestError where
  toJSON (ParseRequestError message details) = JSON.object [
    "message" .= message, "details" .= details]
  toJSON ActionInappropriate = JSON.object [
    "message" .= ("Bad Request" :: Text)]
  toJSON (InvalidBody errorMessage) = JSON.object [
    "message" .= (toS errorMessage :: Text)]
  toJSON InvalidRange = JSON.object [
    "message" .= ("HTTP Range error" :: Text)]
  toJSON UnknownRelation = JSON.object [
    "message" .= ("Unknown relation" :: Text)]
  toJSON (NoRelationBetween parent child) = JSON.object [
    "message" .= ("Could not find foreign keys between these entities, No relation found between " <> parent <> " and " <> child :: Text)]
  toJSON UnsupportedVerb = JSON.object [
    "message" .= ("Unsupported HTTP verb" :: Text)]
  toJSON InvalidFilters = JSON.object [
    "message" .= ("Filters must include all and only primary key columns with 'eq' operators" :: Text)]

instance JSON.ToJSON P.UsageError where
  toJSON (P.ConnectionError e) = JSON.object [
    "code" .= ("" :: Text),
    "message" .= ("Database connection error" :: Text),
    "details" .= (toS $ fromMaybe "" e :: Text)]
  toJSON (P.SessionError e) = JSON.toJSON e -- H.Error

instance JSON.ToJSON H.QueryError where
  toJSON (H.QueryError _ _ e) = JSON.toJSON e

instance JSON.ToJSON H.CommandError where
  toJSON (H.ResultError (H.ServerError c m d h)) = case toS c of
    'P':'T':_ ->
      JSON.object [
        "details" .= (fmap toS d::Maybe Text),
        "hint" .= (fmap toS h::Maybe Text)]
    _ ->
      JSON.object [
        "code" .= (toS c::Text),
        "message" .= (toS m::Text),
        "details" .= (fmap toS d::Maybe Text),
        "hint" .= (fmap toS h::Maybe Text)]
  toJSON (H.ResultError (H.UnexpectedResult m)) = JSON.object [
    "message" .= (m::Text)]
  toJSON (H.ResultError (H.RowError i H.EndOfInput)) = JSON.object [
    "message" .= ("Row error: end of input"::Text),
    "details" .=
      ("Attempt to parse more columns than there are in the result"::Text),
    "details" .= (("Row number " <> show i)::Text)]
  toJSON (H.ResultError (H.RowError i H.UnexpectedNull)) = JSON.object [
    "message" .= ("Row error: unexpected null"::Text),
    "details" .= ("Attempt to parse a NULL as some value."::Text),
    "details" .= (("Row number " <> show i)::Text)]
  toJSON (H.ResultError (H.RowError i (H.ValueError d))) = JSON.object [
    "message" .= ("Row error: Wrong value parser used"::Text),
    "details" .= d,
    "details" .= (("Row number " <> show i)::Text)]
  toJSON (H.ResultError (H.UnexpectedAmountOfRows i)) = JSON.object [
    "message" .= ("Unexpected amount of rows"::Text),
    "details" .= i]
  toJSON (H.ClientError d) = JSON.object [
    "message" .= ("Database client error"::Text),
    "details" .= (fmap toS d::Maybe Text)]

httpStatus :: Bool -> P.UsageError -> HT.Status
httpStatus _ (P.ConnectionError _) = HT.status503
httpStatus authed (P.SessionError (H.QueryError _ _ (H.ResultError (H.ServerError c m _ _)))) =
  case toS c of
    '0':'8':_ -> HT.status503 -- pg connection err
    '0':'9':_ -> HT.status500 -- triggered action exception
    '0':'L':_ -> HT.status403 -- invalid grantor
    '0':'P':_ -> HT.status403 -- invalid role specification
    "23503"   -> HT.status409 -- foreign_key_violation
    "23505"   -> HT.status409 -- unique_violation
    '2':'5':_ -> HT.status500 -- invalid tx state
    '2':'8':_ -> HT.status403 -- invalid auth specification
    '2':'D':_ -> HT.status500 -- invalid tx termination
    '3':'8':_ -> HT.status500 -- external routine exception
    '3':'9':_ -> HT.status500 -- external routine invocation
    '3':'B':_ -> HT.status500 -- savepoint exception
    '4':'0':_ -> HT.status500 -- tx rollback
    '5':'3':_ -> HT.status503 -- insufficient resources
    '5':'4':_ -> HT.status413 -- too complex
    '5':'5':_ -> HT.status500 -- obj not on prereq state
    '5':'7':_ -> HT.status500 -- operator intervention
    '5':'8':_ -> HT.status500 -- system error
    'F':'0':_ -> HT.status500 -- conf file error
    'H':'V':_ -> HT.status500 -- foreign data wrapper error
    "P0001"   -> HT.status400 -- default code for "raise"
    'P':'0':_ -> HT.status500 -- PL/pgSQL Error
    'X':'X':_ -> HT.status500 -- internal Error
    "42883"   -> HT.status404 -- undefined function
    "42P01"   -> HT.status404 -- undefined table
    "42501"   -> if authed then HT.status403 else HT.status401 -- insufficient privilege
    'P':'T':n -> fromMaybe HT.status500 (HT.mkStatus <$> readMaybe n <*> pure m)
    _         -> HT.status400
httpStatus _ (P.SessionError (H.QueryError _ _ (H.ResultError _))) = HT.status500
httpStatus _ (P.SessionError (H.QueryError _ _ (H.ClientError _))) = HT.status503
