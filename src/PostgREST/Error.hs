{-|
Module      : PostgREST.Error
Description : PostgREST error HTTP responses
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Error
  ( errorResponseFor
  , ApiRequestError(..)
  , PgError(..)
  , Error(..)
  , errorPayload
  , checkIsFatal
  , singularityError
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Encoding.Error  as T
import qualified Hasql.Pool                as SQL
import qualified Hasql.Session             as SQL
import qualified Network.HTTP.Types.Status as HTTP

import Data.Aeson  ((.=))
import Network.Wai (Response, responseLBS)

import Network.HTTP.Types.Header (Header)

import           PostgREST.ContentType (ContentType (..))
import qualified PostgREST.ContentType as ContentType

import PostgREST.DbStructure.Proc         (ProcDescription (..),
                                           ProcParam (..))
import PostgREST.DbStructure.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..))
import PostgREST.DbStructure.Table        (Column (..), Table (..))

import Protolude


class (JSON.ToJSON a) => PgrstError a where
  status   :: a -> HTTP.Status
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
  | NoRelBetween Text Text
  | AmbiguousRelBetween Text Text [Relationship]
  | AmbiguousRpc [ProcDescription]
  | NoRpc Text Text [Text] Bool ContentType Bool
  | InvalidFilters
  | UnacceptableSchema [Text]
  | ContentTypeError [ByteString]
  | UnsupportedVerb                -- Unreachable?

instance PgrstError ApiRequestError where
  status InvalidRange            = HTTP.status416
  status InvalidFilters          = HTTP.status405
  status (InvalidBody _)         = HTTP.status400
  status UnsupportedVerb         = HTTP.status405
  status ActionInappropriate     = HTTP.status405
  status (ParseRequestError _ _) = HTTP.status400
  status (NoRelBetween _ _)      = HTTP.status400
  status AmbiguousRelBetween{}   = HTTP.status300
  status (AmbiguousRpc _)        = HTTP.status300
  status NoRpc{}                 = HTTP.status404
  status (UnacceptableSchema _)  = HTTP.status406
  status (ContentTypeError _)    = HTTP.status415

  headers _ = [ContentType.toHeader CTApplicationJSON]

instance JSON.ToJSON ApiRequestError where
  toJSON (ParseRequestError message details) = JSON.object [
    "message" .= message, "details" .= details]
  toJSON ActionInappropriate = JSON.object [
    "message" .= ("Bad Request" :: Text)]
  toJSON (InvalidBody errorMessage) = JSON.object [
    "message" .= T.decodeUtf8 errorMessage]
  toJSON InvalidRange = JSON.object [
    "message" .= ("HTTP Range error" :: Text)]
  toJSON (NoRelBetween parent child) = JSON.object [
    "hint"    .= ("If a new foreign key between these entities was created in the database, try reloading the schema cache." :: Text),
    "message" .= ("Could not find a relationship between " <> parent <> " and " <> child <> " in the schema cache" :: Text)]
  toJSON (AmbiguousRelBetween parent child rels) = JSON.object [
    "hint"    .= ("Try changing '" <> child <> "' to one of the following: " <> relHint rels <> ". Find the desired relationship in the 'details' key." :: Text),
    "message" .= ("Could not embed because more than one relationship was found for '" <> parent <> "' and '" <> child <> "'" :: Text),
    "details" .= (compressedRel <$> rels) ]
  toJSON (AmbiguousRpc procs)  = JSON.object [
    "hint"    .= ("Try renaming the parameters or the function itself in the database so function overloading can be resolved" :: Text),
    "message" .= ("Could not choose the best candidate function between: " <> T.intercalate ", " [pdSchema p <> "." <> pdName p <> "(" <> T.intercalate ", " [ppName a <> " => " <> ppType a | a <- pdParams p] <> ")" | p <- procs])]
  toJSON (NoRpc schema procName argumentKeys hasPreferSingleObject contentType isInvPost)  =
    let prms = "(" <> T.intercalate ", " argumentKeys <> ")" in JSON.object [
    "hint"    .= ("If a new function was created in the database with this name and parameters, try reloading the schema cache." :: Text),
    "message" .= ("Could not find the " <> schema <> "." <> procName <>
      (case (hasPreferSingleObject, isInvPost, contentType) of
        (True, _, _)                 -> " function with a single json or jsonb parameter"
        (_, True, CTTextPlain)       -> " function with a single unnamed text parameter"
        (_, True, CTOctetStream)     -> " function with a single unnamed bytea parameter"
        (_, True, CTApplicationJSON) -> prms <> " function or the " <> schema <> "." <> procName <>" function with a single unnamed json or jsonb parameter"
        _                            -> prms <> " function") <>
      " in the schema cache")]
  toJSON UnsupportedVerb = JSON.object [
    "message" .= ("Unsupported HTTP verb" :: Text)]
  toJSON InvalidFilters = JSON.object [
    "message" .= ("Filters must include all and only primary key columns with 'eq' operators" :: Text)]
  toJSON (UnacceptableSchema schemas) = JSON.object [
    "message" .= ("The schema must be one of the following: " <> T.intercalate ", " schemas)]
  toJSON (ContentTypeError cts)    = JSON.object [
    "message" .= ("None of these Content-Types are available: " <> T.intercalate ", " (map T.decodeUtf8 cts))]

compressedRel :: Relationship -> JSON.Value
compressedRel Relationship{..} =
  let
    fmtEls els = "(" <> T.intercalate ", " els <> ")"
  in
  JSON.object $
    ("embedding" .= (tableName relTable <> " with " <> tableName relForeignTable :: Text))
    : case relCardinality of
        M2M Junction{..} -> [
            "cardinality" .= ("many-to-many" :: Text)
          , "relationship" .= (tableName junTable <> " using " <> junConstraint1 <> fmtEls (colName <$> junColumns1) <> " and " <> junConstraint2 <> fmtEls (colName <$> junColumns2))
          ]
        M2O cons -> [
            "cardinality" .= ("many-to-one" :: Text)
          , "relationship" .= (cons <> " using " <> tableName relTable <> fmtEls (colName <$> relColumns) <> " and " <> tableName relForeignTable <> fmtEls (colName <$> relForeignColumns))
          ]
        O2M cons -> [
            "cardinality" .= ("one-to-many" :: Text)
          , "relationship" .= (cons <> " using " <> tableName relTable <> fmtEls (colName <$> relColumns) <> " and " <> tableName relForeignTable <> fmtEls (colName <$> relForeignColumns))
          ]

relHint :: [Relationship] -> Text
relHint rels = T.intercalate ", " (hintList <$> rels)
  where
    hintList Relationship{..} =
      let buildHint rel = "'" <> tableName relForeignTable <> "!" <> rel <> "'" in
      case relCardinality of
        M2M Junction{..} -> buildHint (tableName junTable)
        M2O cons         -> buildHint cons
        O2M cons         -> buildHint cons

data PgError = PgError Authenticated SQL.UsageError
type Authenticated = Bool

instance PgrstError PgError where
  status (PgError authed usageError) = pgErrorStatus authed usageError

  headers err =
    if status err == HTTP.status401
       then [ContentType.toHeader CTApplicationJSON, ("WWW-Authenticate", "Bearer") :: Header]
       else [ContentType.toHeader CTApplicationJSON]

instance JSON.ToJSON PgError where
  toJSON (PgError _ usageError) = JSON.toJSON usageError

instance JSON.ToJSON SQL.UsageError where
  toJSON (SQL.ConnectionError e) = JSON.object [
    "code"    .= ("" :: Text),
    "message" .= ("Database connection error. Retrying the connection." :: Text),
    "details" .= (T.decodeUtf8With T.lenientDecode $ fromMaybe "" e :: Text)]
  toJSON (SQL.SessionError e) = JSON.toJSON e -- SQL.Error

instance JSON.ToJSON SQL.QueryError where
  toJSON (SQL.QueryError _ _ e) = JSON.toJSON e

instance JSON.ToJSON SQL.CommandError where
  toJSON (SQL.ResultError (SQL.ServerError c m d h)) = case BS.unpack c of
    'P':'T':_ -> JSON.object [
        "details" .= fmap T.decodeUtf8 d,
        "hint"    .= fmap T.decodeUtf8 h]

    _         -> JSON.object [
        "code"    .= (T.decodeUtf8 c      :: Text),
        "message" .= (T.decodeUtf8 m      :: Text),
        "details" .= (fmap T.decodeUtf8 d :: Maybe Text),
        "hint"    .= (fmap T.decodeUtf8 h :: Maybe Text)]

  toJSON (SQL.ResultError (SQL.UnexpectedResult m)) = JSON.object [
    "message" .= (m :: Text)]
  toJSON (SQL.ResultError (SQL.RowError i SQL.EndOfInput)) = JSON.object [
    "message" .= ("Row error: end of input" :: Text),
    "details" .= ("Attempt to parse more columns than there are in the result" :: Text),
    "hint"    .= (("Row number " <> show i) :: Text)]
  toJSON (SQL.ResultError (SQL.RowError i SQL.UnexpectedNull)) = JSON.object [
    "message" .= ("Row error: unexpected null" :: Text),
    "details" .= ("Attempt to parse a NULL as some value." :: Text),
    "hint"    .= (("Row number " <> show i) :: Text)]
  toJSON (SQL.ResultError (SQL.RowError i (SQL.ValueError d))) = JSON.object [
    "message" .= ("Row error: Wrong value parser used" :: Text),
    "details" .= d,
    "hint"    .= (("Row number " <> show i) :: Text)]
  toJSON (SQL.ResultError (SQL.UnexpectedAmountOfRows i)) = JSON.object [
    "message" .= ("Unexpected amount of rows" :: Text),
    "details" .= i]
  toJSON (SQL.ClientError d) = JSON.object [
    "message" .= ("Database client error. Retrying the connection." :: Text),
    "details" .= (fmap T.decodeUtf8 d :: Maybe Text)]

pgErrorStatus :: Bool -> SQL.UsageError -> HTTP.Status
pgErrorStatus _      (SQL.ConnectionError _)                                      = HTTP.status503
pgErrorStatus _      (SQL.SessionError (SQL.QueryError _ _ (SQL.ClientError _)))      = HTTP.status503
pgErrorStatus authed (SQL.SessionError (SQL.QueryError _ _ (SQL.ResultError rError))) =
  case rError of
    (SQL.ServerError c m _ _) ->
      case BS.unpack c of
        '0':'8':_ -> HTTP.status503 -- pg connection err
        '0':'9':_ -> HTTP.status500 -- triggered action exception
        '0':'L':_ -> HTTP.status403 -- invalid grantor
        '0':'P':_ -> HTTP.status403 -- invalid role specification
        "23503"   -> HTTP.status409 -- foreign_key_violation
        "23505"   -> HTTP.status409 -- unique_violation
        "25006"   -> HTTP.status405 -- read_only_sql_transaction
        '2':'5':_ -> HTTP.status500 -- invalid tx state
        '2':'8':_ -> HTTP.status403 -- invalid auth specification
        '2':'D':_ -> HTTP.status500 -- invalid tx termination
        '3':'8':_ -> HTTP.status500 -- external routine exception
        '3':'9':_ -> HTTP.status500 -- external routine invocation
        '3':'B':_ -> HTTP.status500 -- savepoint exception
        '4':'0':_ -> HTTP.status500 -- tx rollback
        '5':'3':_ -> HTTP.status503 -- insufficient resources
        '5':'4':_ -> HTTP.status413 -- too complex
        '5':'5':_ -> HTTP.status500 -- obj not on prereq state
        '5':'7':_ -> HTTP.status500 -- operator intervention
        '5':'8':_ -> HTTP.status500 -- system error
        'F':'0':_ -> HTTP.status500 -- conf file error
        'H':'V':_ -> HTTP.status500 -- foreign data wrapper error
        "P0001"   -> HTTP.status400 -- default code for "raise"
        'P':'0':_ -> HTTP.status500 -- PL/pgSQL Error
        'X':'X':_ -> HTTP.status500 -- internal Error
        "42883"   -> HTTP.status404 -- undefined function
        "42P01"   -> HTTP.status404 -- undefined table
        "42501"   -> if authed then HTTP.status403 else HTTP.status401 -- insufficient privilege
        'P':'T':n -> fromMaybe HTTP.status500 (HTTP.mkStatus <$> readMaybe n <*> pure m)
        _         -> HTTP.status400

    _                       -> HTTP.status500

checkIsFatal :: PgError -> Maybe Text
checkIsFatal (PgError _ (SQL.ConnectionError e))
  | isAuthFailureMessage = Just $ toS failureMessage
  | otherwise = Nothing
  where isAuthFailureMessage = "FATAL:  password authentication failed" `isPrefixOf` failureMessage
        failureMessage = BS.unpack $ fromMaybe mempty e
checkIsFatal (PgError _ (SQL.SessionError (SQL.QueryError _ _ (SQL.ResultError serverError))))
  = case serverError of
      -- Check for a syntax error (42601 is the pg code). This would mean the error is on our part somehow, so we treat it as fatal.
      SQL.ServerError "42601" _ _ _
        -> Just "Hint: This is probably a bug in PostgREST, please report it at https://github.com/PostgREST/postgrest/issues"
      -- Check for a "prepared statement <name> already exists" error (Code 42P05: duplicate_prepared_statement).
      -- This would mean that a connection pooler in transaction mode is being used
      -- while prepared statements are enabled in the PostgREST configuration,
      -- both of which are incompatible with each other.
      SQL.ServerError "42P05" _ _ _
        -> Just "Hint: If you are using connection poolers in transaction mode, try setting db-prepared-statements to false."
      -- Check for a "transaction blocks not allowed in statement pooling mode" error (Code 08P01: protocol_violation).
      -- This would mean that a connection pooler in statement mode is being used which is not supported in PostgREST.
      SQL.ServerError "08P01" "transaction blocks not allowed in statement pooling mode" _ _
        -> Just "Hint: Connection poolers in statement mode are not supported."
      _ -> Nothing
checkIsFatal _ = Nothing


data Error
  = GucHeadersError
  | GucStatusError
  | BinaryFieldError ContentType
  | ConnectionLostError
  | PutMatchingPkError
  | PutRangeNotAllowedError
  | JwtTokenMissing
  | JwtTokenInvalid Text
  | SingularityError Integer
  | NotFound
  | ApiRequestError ApiRequestError
  | PgErr PgError

instance PgrstError Error where
  status GucHeadersError         = HTTP.status500
  status GucStatusError          = HTTP.status500
  status (BinaryFieldError _)    = HTTP.status406
  status ConnectionLostError     = HTTP.status503
  status PutMatchingPkError      = HTTP.status400
  status PutRangeNotAllowedError = HTTP.status400
  status JwtTokenMissing         = HTTP.status500
  status (JwtTokenInvalid _)     = HTTP.unauthorized401
  status (SingularityError _)    = HTTP.status406
  status NotFound                = HTTP.status404
  status (PgErr err)             = status err
  status (ApiRequestError err)   = status err

  headers (SingularityError _)     = [ContentType.toHeader CTSingularJSON]
  headers (JwtTokenInvalid m)      = [ContentType.toHeader CTApplicationJSON, invalidTokenHeader m]
  headers (PgErr err)              = headers err
  headers (ApiRequestError err)    = headers err
  headers _                        = [ContentType.toHeader CTApplicationJSON]

instance JSON.ToJSON Error where
  toJSON GucHeadersError           = JSON.object [
    "message" .= ("response.headers guc must be a JSON array composed of objects with a single key and a string value" :: Text)]
  toJSON GucStatusError           = JSON.object [
    "message" .= ("response.status guc must be a valid status code" :: Text)]
  toJSON (BinaryFieldError ct)          = JSON.object [
    "message" .= ((T.decodeUtf8 (ContentType.toMime ct) <> " requested but more than one column was selected") :: Text)]
  toJSON ConnectionLostError       = JSON.object [
    "message" .= ("Database connection lost. Retrying the connection." :: Text)]

  toJSON PutRangeNotAllowedError   = JSON.object [
    "message" .= ("Range header and limit/offset querystring parameters are not allowed for PUT" :: Text)]
  toJSON PutMatchingPkError        = JSON.object [
    "message" .= ("Payload values do not match URL in primary key column(s)" :: Text)]

  toJSON (SingularityError n)      = JSON.object [
    "message" .= ("JSON object requested, multiple (or no) rows returned" :: Text),
    "details" .= T.unwords ["Results contain", show n, "rows,", T.decodeUtf8 (ContentType.toMime CTSingularJSON), "requires 1 row"]]

  toJSON JwtTokenMissing           = JSON.object [
    "message" .= ("Server lacks JWT secret" :: Text)]
  toJSON (JwtTokenInvalid message) = JSON.object [
    "message" .= (message :: Text)]
  toJSON NotFound = JSON.object []
  toJSON (PgErr err) = JSON.toJSON err
  toJSON (ApiRequestError err) = JSON.toJSON err

invalidTokenHeader :: Text -> Header
invalidTokenHeader m =
  ("WWW-Authenticate", "Bearer error=\"invalid_token\", " <> "error_description=" <> encodeUtf8 (show m))

singularityError :: (Integral a) => a -> Error
singularityError = SingularityError . toInteger
