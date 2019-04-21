{-|
Module      : PostgREST.Error
Description : PostgREST error HTTP responses
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PostgREST.Error (
  errorResponseFor
, ApiRequestError(..)
, PgError(..)
, SimpleError(..)
, errorPayload
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


class (JSON.ToJSON a) => PostgRestError a where
  status   :: a -> HT.Status
  headers  :: a -> [Header]

  errorPayload :: a -> LByteString
  errorPayload = JSON.encode

  errorResponseFor :: a -> Response
  errorResponseFor err = responseLBS (status err) (headers err) $ errorPayload err



data ApiRequestError
  = ActionInappropriate
  | InvalidRange
  | InvalidBody ByteString
  | ParseRequestError Text Text
  | NoRelationBetween Text Text
  | InvalidFilters
  | UnknownRelation                -- Unreachable?
  | UnsupportedVerb                -- Unreachable?
  deriving (Show, Eq)

instance PostgRestError ApiRequestError where
  status InvalidRange            = HT.status416
  status InvalidFilters          = HT.status405
  status (InvalidBody _)         = HT.status400
  status UnsupportedVerb         = HT.status405
  status UnknownRelation         = HT.status404
  status ActionInappropriate     = HT.status405
  status (ParseRequestError _ _) = HT.status400
  status (NoRelationBetween _ _) = HT.status400

  headers _ = [toHeader CTApplicationJSON]

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


data PgError = PgError Authenticated P.UsageError
type Authenticated = Bool

instance PostgRestError PgError where
  status (PgError authed usageError) = pgErrorStatus authed usageError

  headers err =
    if status err == HT.status401
       then [toHeader CTApplicationJSON, ("WWW-Authenticate", "Bearer") :: Header]
       else [toHeader CTApplicationJSON]

instance JSON.ToJSON PgError where
  toJSON (PgError _ usageError) = JSON.toJSON usageError

instance JSON.ToJSON P.UsageError where
  toJSON (P.ConnectionError e) = JSON.object [
    "code"    .= ("" :: Text),
    "message" .= ("Database connection error" :: Text),
    "details" .= (toS $ fromMaybe "" e :: Text)]
  toJSON (P.SessionError e) = JSON.toJSON e -- H.Error

instance JSON.ToJSON H.QueryError where
  toJSON (H.QueryError _ _ e) = JSON.toJSON e

instance JSON.ToJSON H.CommandError where
  toJSON (H.ResultError (H.ServerError c m d h)) = case toS c of
    'P':'T':_ -> JSON.object [
        "details" .= (fmap toS d :: Maybe Text),
        "hint"    .= (fmap toS h :: Maybe Text)]

    _         -> JSON.object [
        "code"    .= (toS c      :: Text),
        "message" .= (toS m      :: Text),
        "details" .= (fmap toS d :: Maybe Text),
        "hint"    .= (fmap toS h :: Maybe Text)]

  toJSON (H.ResultError (H.UnexpectedResult m)) = JSON.object [
    "message" .= (m :: Text)]
  toJSON (H.ResultError (H.RowError i H.EndOfInput)) = JSON.object [
    "message" .= ("Row error: end of input" :: Text),
    "details" .= ("Attempt to parse more columns than there are in the result" :: Text),
    "hint"    .= (("Row number " <> show i) :: Text)]
  toJSON (H.ResultError (H.RowError i H.UnexpectedNull)) = JSON.object [
    "message" .= ("Row error: unexpected null" :: Text),
    "details" .= ("Attempt to parse a NULL as some value." :: Text),
    "hint"    .= (("Row number " <> show i) :: Text)]
  toJSON (H.ResultError (H.RowError i (H.ValueError d))) = JSON.object [
    "message" .= ("Row error: Wrong value parser used" :: Text),
    "details" .= d,
    "hint"    .= (("Row number " <> show i) :: Text)]
  toJSON (H.ResultError (H.UnexpectedAmountOfRows i)) = JSON.object [
    "message" .= ("Unexpected amount of rows" :: Text),
    "details" .= i]
  toJSON (H.ClientError d) = JSON.object [
    "message" .= ("Database client error" :: Text),
    "details" .= (fmap toS d :: Maybe Text)]

pgErrorStatus :: Bool -> P.UsageError -> HT.Status
pgErrorStatus _      (P.ConnectionError _)                                      = HT.status503
pgErrorStatus _      (P.SessionError (H.QueryError _ _ (H.ClientError _)))      = HT.status503
pgErrorStatus authed (P.SessionError (H.QueryError _ _ (H.ResultError rError))) =
  case rError of
    (H.ServerError c m _ _) ->
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

    _                       -> HT.status500


data SimpleError
  = GucHeadersError
  | BinaryFieldError
  | ConnectionLostError
  | PutSingletonError
  | PutMatchingPkError
  | PutRangeNotAllowedError
  | PutPayloadIncompleteError
  | JwtTokenMissing
  | JwtTokenInvalid Text
  | SingularityError Integer
  | ContentTypeError [ByteString]
  deriving (Show, Eq)

instance PostgRestError SimpleError where
  status GucHeadersError           = HT.status500
  status BinaryFieldError          = HT.status406
  status ConnectionLostError       = HT.status503
  status PutSingletonError         = HT.status400
  status PutMatchingPkError        = HT.status400
  status PutRangeNotAllowedError   = HT.status400
  status PutPayloadIncompleteError = HT.status400
  status JwtTokenMissing           = HT.status500
  status (JwtTokenInvalid _)       = HT.unauthorized401
  status (SingularityError _)      = HT.status406
  status (ContentTypeError _)      = HT.status415

  headers (SingularityError _)     = [toHeader CTSingularJSON]
  headers (JwtTokenInvalid m)      = [toHeader CTApplicationJSON, invalidTokenHeader m]
  headers _                        = [toHeader CTApplicationJSON]

instance JSON.ToJSON SimpleError where
  toJSON GucHeadersError           = JSON.object [
    "message" .= ("response.headers guc must be a JSON array composed of objects with a single key and a string value" :: Text)]
  toJSON BinaryFieldError          = JSON.object [
    "message" .= ((toS (toMime CTOctetStream) <> " requested but a single column was not selected") :: Text)]
  toJSON ConnectionLostError       = JSON.object [
    "message" .= ("Database connection lost, retrying the connection." :: Text)]

  toJSON PutSingletonError         = JSON.object [
    "message" .= ("PUT payload must contain a single row" :: Text)]
  toJSON PutRangeNotAllowedError   = JSON.object [
    "message" .= ("Range header and limit/offset querystring parameters are not allowed for PUT" :: Text)]
  toJSON PutPayloadIncompleteError = JSON.object [
    "message" .= ("You must specify all columns in the payload when using PUT" :: Text)]
  toJSON PutMatchingPkError        = JSON.object [
    "message" .= ("Payload values do not match URL in primary key column(s)" :: Text)]

  toJSON (ContentTypeError cts)    = JSON.object [
    "message" .= ("None of these Content-Types are available: " <> (toS . intercalate ", " . map toS) cts :: Text)]
  toJSON (SingularityError n)      = JSON.object [
    "message" .= ("JSON object requested, multiple (or no) rows returned" :: Text),
    "details" .= unwords ["Results contain", show n, "rows,", toS (toMime CTSingularJSON), "requires 1 row"]]

  toJSON JwtTokenMissing           = JSON.object [
    "message" .= ("Server lacks JWT secret" :: Text)]
  toJSON (JwtTokenInvalid message) = JSON.object [
    "message" .= (message :: Text)]

invalidTokenHeader :: Text -> Header
invalidTokenHeader m =
  ("WWW-Authenticate", "Bearer error=\"invalid_token\", " <> "error_description=" <> show m)
