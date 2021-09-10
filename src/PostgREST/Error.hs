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
import qualified Data.Text                 as T
import qualified Hasql.Pool                as P
import qualified Hasql.Session             as H
import qualified Network.HTTP.Types.Status as HT

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

import Protolude      hiding (toS)
import Protolude.Conv (toS, toSL)


class (JSON.ToJSON a) => PgrstError a where
  status   :: a -> HT.Status
  headers  :: a -> [Header]

  errorPayload :: a -> LByteString
  errorPayload = JSON.encode

  errorResponseFor :: a -> Response
  errorResponseFor err = responseLBS (status err) (headers err) $ errorPayload err




data ApiRequestError
  = ParseRequestError Text Text
  | ActionInappropriate
  | InvalidBody ByteString
  | InvalidRange
  | UnsupportedVerb                -- Unreachable?
  | InvalidFilters
  | UnacceptableSchema [Text]
  | ContentTypeError [ByteString]
  | NoRelBetween Text Text
  | AmbiguousRelBetween Text Text [Relationship]
  | NoRpc Text Text [Text] Bool ContentType Bool  
  | AmbiguousRpc [ProcDescription]  

instance PgrstError ApiRequestError where
  status (ParseRequestError _ _) = HT.status400
  status ActionInappropriate     = HT.status405
  status (InvalidBody _)         = HT.status400
  status InvalidRange            = HT.status416
  status UnsupportedVerb         = HT.status405
  status InvalidFilters          = HT.status405
  status (UnacceptableSchema _)  = HT.status406
  status (ContentTypeError _)    = HT.status415
  status (NoRelBetween _ _)      = HT.status400
  status AmbiguousRelBetween{}   = HT.status300
  status NoRpc{}                 = HT.status404
  status (AmbiguousRpc _)        = HT.status300

  headers _ = [ContentType.toHeader CTApplicationJSON]

instance JSON.ToJSON ApiRequestError where
  toJSON (ParseRequestError message details) = JSON.object [
    "code"    .= ApiRequestErrorCode00,
    "message" .= message,
    "details" .= details,
    "hint"    .= JSON.Null]
  toJSON ActionInappropriate = JSON.object [
    "code"    .= ApiRequestErrorCode01,
    "message" .= ("Bad Request" :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON (InvalidBody errorMessage) = JSON.object [
    "code"    .= ApiRequestErrorCode02,
    "message" .= (toS errorMessage :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON InvalidRange = JSON.object [
    "code"    .= ApiRequestErrorCode03,
    "message" .= ("HTTP Range error" :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON UnsupportedVerb = JSON.object [
    "code"    .= ApiRequestErrorCode04,
    "message" .= ("Unsupported HTTP verb" :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON InvalidFilters = JSON.object [
    "code"    .= ApiRequestErrorCode05,
    "message" .= ("Filters must include all and only primary key columns with 'eq' operators" :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON (UnacceptableSchema schemas) = JSON.object [
    "code"    .= ApiRequestErrorCode06,
    "message" .= ("The schema must be one of the following: " <> T.intercalate ", " schemas),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON (ContentTypeError cts)    = JSON.object [
    "code"    .= ApiRequestErrorCode07,
    "message" .= ("None of these Content-Types are available: " <> (toS . intercalate ", " . map toS) cts :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]

  toJSON (NoRelBetween parent child) = JSON.object [
    "code"    .= SchemaCacheErrorCode01,
    "message" .= ("Could not find a relationship between " <> parent <> " and " <> child <> " in the schema cache" :: Text),
    "details" .= JSON.Null,
    "hint"    .= ("If a new foreign key between these entities was created in the database, try reloading the schema cache." :: Text)]
  toJSON (AmbiguousRelBetween parent child rels) = JSON.object [
    "code"    .= SchemaCacheErrorCode02,
    "message" .= ("More than one relationship was found for " <> parent <> " and " <> child :: Text),
    "details" .= (compressedRel <$> rels),
    "hint"    .= ("By following the 'details' key, disambiguate the request by changing the url to /origin?select=relationship(*) or /origin?select=target!relationship(*)" :: Text)]  
  toJSON (NoRpc schema procName argumentKeys hasPreferSingleObject contentType isInvPost)  =
    let prms = "(" <> T.intercalate ", " argumentKeys <> ")" in JSON.object [
    "code"    .= SchemaCacheErrorCode04,
    "message" .= ("Could not find the " <> schema <> "." <> procName <>
          (case (hasPreferSingleObject, isInvPost, contentType) of
            (True, _, _)                 -> " function with a single json or jsonb parameter"
            (_, True, CTTextPlain)       -> " function with a single unnamed text parameter"
            (_, True, CTOctetStream)     -> " function with a single unnamed bytea parameter"
            (_, True, CTApplicationJSON) -> prms <> " function or the " <> schema <> "." <> procName <>" function with a single unnamed json or jsonb parameter"
            _                            -> prms <> " function") <>
          " in the schema cache"),
    "details" .= JSON.Null,
    "hint"    .= ("If a new function was created in the database with this name and parameters, try reloading the schema cache." :: Text)]
  toJSON (AmbiguousRpc procs)  = JSON.object [
    "code"    .= SchemaCacheErrorCode03,
    "message" .= ("Could not choose the best candidate function between: " <> T.intercalate ", " [pdSchema p <> "." <> pdName p <> "(" <> T.intercalate ", " [ppName a <> " => " <> ppType a | a <- pdParams p] <> ")" | p <- procs]),
    "details" .= JSON.Null,
    "hint"    .= ("Try renaming the parameters or the function itself in the database so function overloading can be resolved" :: Text)]

compressedRel :: Relationship -> JSON.Value
compressedRel Relationship{..} =
  let
    fmtTbl Table{..} = tableSchema <> "." <> tableName
    fmtEls els = "[" <> T.intercalate ", " els <> "]"
  in
  JSON.object $ [
    "origin"      .= fmtTbl relTable
  , "target"      .= fmtTbl relForeignTable
  ] ++
  case relCardinality of
    M2M Junction{..} -> [
        "cardinality" .= ("m2m" :: Text)
      , "relationship" .= (fmtTbl junTable <> fmtEls [junConstraint1] <> fmtEls [junConstraint2])
      ]
    M2O cons -> [
        "cardinality" .= ("m2o" :: Text)
      , "relationship" .= (cons <> fmtEls (colName <$> relColumns) <> fmtEls (colName <$> relForeignColumns))
      ]
    O2M cons -> [
        "cardinality" .= ("o2m" :: Text)
      , "relationship" .= (cons <> fmtEls (colName <$> relColumns) <> fmtEls (colName <$> relForeignColumns))
      ]

data PgError = PgError Authenticated P.UsageError
type Authenticated = Bool

instance PgrstError PgError where
  status (PgError authed usageError) = pgErrorStatus authed usageError

  headers err =
    if status err == HT.status401
       then [ContentType.toHeader CTApplicationJSON, ("WWW-Authenticate", "Bearer") :: Header]
       else [ContentType.toHeader CTApplicationJSON]

instance JSON.ToJSON PgError where
  toJSON (PgError _ usageError) = JSON.toJSON usageError

instance JSON.ToJSON P.UsageError where
  toJSON (P.ConnectionError e) = JSON.object [
    "code"    .= ConnectionErrorCode00,
    "message" .= ("Database connection error. Retrying the connection." :: Text),
    "details" .= (toSL $ fromMaybe "" e :: Text),
    "hint"    .= JSON.Null]
  toJSON (P.SessionError e) = JSON.toJSON e -- H.Error

instance JSON.ToJSON H.QueryError where
  toJSON (H.QueryError _ _ e) = JSON.toJSON e

instance JSON.ToJSON H.CommandError where
  toJSON (H.ResultError (H.ServerError c m d h)) = JSON.object [
        "code"    .= (toS c      :: Text),
        "message" .= (toS m      :: Text),
        "details" .= (fmap toS d :: Maybe Text),
        "hint"    .= (fmap toS h :: Maybe Text)]

  toJSON (H.ResultError (H.UnexpectedResult m)) = JSON.object [
    "code"    .= HasqlErrorCode01,
    "message" .= (m :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON (H.ResultError (H.RowError i H.EndOfInput)) = JSON.object [
    "code"    .= HasqlErrorCode02,
    "message" .= ("Row error: end of input" :: Text),
    "details" .= ("Attempt to parse more columns than there are in the result" :: Text),
    "hint"    .= (("Row number " <> show i) :: Text)]
  toJSON (H.ResultError (H.RowError i H.UnexpectedNull)) = JSON.object [
    "code"    .= HasqlErrorCode03,
    "message" .= ("Row error: unexpected null" :: Text),
    "details" .= ("Attempt to parse a NULL as some value." :: Text),
    "hint"    .= (("Row number " <> show i) :: Text)]
  toJSON (H.ResultError (H.RowError i (H.ValueError d))) = JSON.object [
    "code"    .= HasqlErrorCode04,
    "message" .= ("Row error: Wrong value parser used" :: Text),
    "details" .= d,
    "hint"    .= (("Row number " <> show i) :: Text)]
  toJSON (H.ResultError (H.UnexpectedAmountOfRows i)) = JSON.object [
    "code"    .= HasqlErrorCode05,
    "message" .= ("Unexpected amount of rows" :: Text),
    "details" .= i,
    "hint"    .= JSON.Null]
  toJSON (H.ClientError d) = JSON.object [
    "code"    .= ConnectionErrorCode01,
    "message" .= ("Database client error. Retrying the connection." :: Text),
    "details" .= (fmap toS d :: Maybe Text),
    "hint"    .= JSON.Null]

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
        "25006"   -> HT.status405 -- read_only_sql_transaction
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

checkIsFatal :: PgError -> Maybe Text
checkIsFatal (PgError _ (P.ConnectionError e))
  | isAuthFailureMessage = Just $ toS failureMessage
  | otherwise = Nothing
  where isAuthFailureMessage = "FATAL:  password authentication failed" `isPrefixOf` toS failureMessage
        failureMessage = fromMaybe mempty e
checkIsFatal (PgError _ (P.SessionError (H.QueryError _ _ (H.ResultError serverError))))
  = case serverError of
      -- Check for a syntax error (42601 is the pg code). This would mean the error is on our part somehow, so we treat it as fatal.
      H.ServerError "42601" _ _ _
        -> Just "Hint: This is probably a bug in PostgREST, please report it at https://github.com/PostgREST/postgrest/issues"
      -- Check for a "prepared statement <name> already exists" error (Code 42P05: duplicate_prepared_statement).
      -- This would mean that a connection pooler in transaction mode is being used
      -- while prepared statements are enabled in the PostgREST configuration,
      -- both of which are incompatible with each other.
      H.ServerError "42P05" _ _ _
        -> Just "Hint: If you are using connection poolers in transaction mode, try setting db-prepared-statements to false."
      -- Check for a "transaction blocks not allowed in statement pooling mode" error (Code 08P01: protocol_violation).
      -- This would mean that a connection pooler in statement mode is being used which is not supported in PostgREST.
      H.ServerError "08P01" "transaction blocks not allowed in statement pooling mode" _ _
        -> Just "Hint: Connection poolers in statement mode are not supported."
      _ -> Nothing
checkIsFatal _ = Nothing


data Error
  = ConnectionLostError
  | JwtTokenMissing
  | JwtTokenInvalid Text
  | GucHeadersError
  | GucStatusError
  | BinaryFieldError ContentType
  | PutMatchingPkError
  | PutRangeNotAllowedError
  | SingularityError Integer
  | NotFound
  | PgErr PgError
  | ApiRequestError ApiRequestError

instance PgrstError Error where
  status ConnectionLostError     = HT.status503
  status JwtTokenMissing         = HT.status500
  status (JwtTokenInvalid _)     = HT.unauthorized401
  status GucHeadersError         = HT.status500
  status GucStatusError          = HT.status500
  status (BinaryFieldError _)    = HT.status406
  status PutMatchingPkError      = HT.status400
  status PutRangeNotAllowedError = HT.status400
  status (SingularityError _)    = HT.status406
  status NotFound                = HT.status404
  status (PgErr err)             = status err
  status (ApiRequestError err)   = status err

  headers (SingularityError _)     = [ContentType.toHeader CTSingularJSON]
  headers (JwtTokenInvalid m)      = [ContentType.toHeader CTApplicationJSON, invalidTokenHeader m]
  headers (PgErr err)              = headers err
  headers (ApiRequestError err)    = headers err
  headers _                        = [ContentType.toHeader CTApplicationJSON]

instance JSON.ToJSON Error where
  toJSON ConnectionLostError       = JSON.object [
      "code"    .= ConnectionErrorCode02,
      "message" .= ("Database connection lost. Retrying the connection." :: Text),
      "details" .= JSON.Null,
      "hint"    .= JSON.Null]

  toJSON JwtTokenMissing           = JSON.object [
      "code"    .= JWTErrorCode00,
      "message" .= ("Server lacks JWT secret" :: Text),
      "details" .= JSON.Null,
      "hint"    .= JSON.Null]
  toJSON (JwtTokenInvalid message) = JSON.object [
      "code"    .= JWTErrorCode01,
      "message" .= (message :: Text),
      "details" .= JSON.Null,
      "hint"    .= JSON.Null]

  toJSON GucHeadersError           = JSON.object [
    "code"    .= GeneralErrorCode01,
    "message" .= ("response.headers guc must be a JSON array composed of objects with a single key and a string value" :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON GucStatusError           = JSON.object [
    "code"    .= GeneralErrorCode02,
    "message" .= ("response.status guc must be a valid status code" :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON (BinaryFieldError ct)          = JSON.object [
    "code"    .= GeneralErrorCode03,
    "message" .= ((toS (ContentType.toMime ct) <> " requested but more than one column was selected") :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]

  toJSON PutRangeNotAllowedError   = JSON.object [
    "code"    .= GeneralErrorCode04,
    "message" .= ("Range header and limit/offset querystring parameters are not allowed for PUT" :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]
  toJSON PutMatchingPkError        = JSON.object [
    "code"    .= GeneralErrorCode05,
    "message" .= ("Payload values do not match URL in primary key column(s)" :: Text),
    "details" .= JSON.Null,
    "hint"    .= JSON.Null]

  toJSON (SingularityError n)      = JSON.object [
    "code"    .= GeneralErrorCode06,
    "message" .= ("JSON object requested, multiple (or no) rows returned" :: Text),
    "details" .= T.unwords ["Results contain", show n, "rows,", toS (ContentType.toMime CTSingularJSON), "requires 1 row"],
    "hint"    .= JSON.Null]

  toJSON NotFound = JSON.object []
  toJSON (PgErr err) = JSON.toJSON err
  toJSON (ApiRequestError err) = JSON.toJSON err

invalidTokenHeader :: Text -> Header
invalidTokenHeader m =
  ("WWW-Authenticate", "Bearer error=\"invalid_token\", " <> "error_description=" <> encodeUtf8 (show m))

singularityError :: (Integral a) => a -> Error
singularityError = SingularityError . toInteger

-- Error codes are grouped by common modules or characteristics
data ErrorCode
  -- PostgreSQL connection errors
  = ConnectionErrorCode00
  | ConnectionErrorCode01
  | ConnectionErrorCode02
  -- API Request errors
  | ApiRequestErrorCode00
  | ApiRequestErrorCode01
  | ApiRequestErrorCode02
  | ApiRequestErrorCode03
  | ApiRequestErrorCode04
  | ApiRequestErrorCode05
  | ApiRequestErrorCode06
  | ApiRequestErrorCode07
  -- Schema Cache errors
  | SchemaCacheErrorCode01
  | SchemaCacheErrorCode02
  | SchemaCacheErrorCode03
  | SchemaCacheErrorCode04
  -- JWT authentication errors
  | JWTErrorCode00
  | JWTErrorCode01
  -- Hasql library errors
  | HasqlErrorCode00
  | HasqlErrorCode01
  | HasqlErrorCode02
  | HasqlErrorCode03
  | HasqlErrorCode04
  | HasqlErrorCode05
  -- Uncategorized errors that are not related to a single module
  | GeneralErrorCode00
  | GeneralErrorCode01
  | GeneralErrorCode02
  | GeneralErrorCode03
  | GeneralErrorCode04
  | GeneralErrorCode05
  | GeneralErrorCode06

instance JSON.ToJSON ErrorCode where
  toJSON e = JSON.toJSON (buildErrorCode e)

-- New group of errors will be added at the end of all the groups and will have the next prefix in the sequence
-- New errors are added at the end of the group they belong to and will have the next code in the sequence
buildErrorCode :: ErrorCode -> Text
buildErrorCode code = "PGRST" <> case code of
  ConnectionErrorCode00  -> "000"
  ConnectionErrorCode01  -> "001"
  ConnectionErrorCode02  -> "002"

  ApiRequestErrorCode00  -> "100"
  ApiRequestErrorCode01  -> "101"
  ApiRequestErrorCode02  -> "102"
  ApiRequestErrorCode03  -> "103"
  ApiRequestErrorCode04  -> "104"
  ApiRequestErrorCode05  -> "105"
  ApiRequestErrorCode06  -> "106"
  ApiRequestErrorCode07  -> "107"
  
  SchemaCacheErrorCode01 -> "200"
  SchemaCacheErrorCode02 -> "201"
  SchemaCacheErrorCode03 -> "202"
  SchemaCacheErrorCode04 -> "203"

  JWTErrorCode00         -> "300"
  JWTErrorCode01         -> "301"

  HasqlErrorCode00       -> "400"
  HasqlErrorCode01       -> "401"
  HasqlErrorCode02       -> "402"
  HasqlErrorCode03       -> "403"
  HasqlErrorCode04       -> "404"
  HasqlErrorCode05       -> "405"

  GeneralErrorCode00     -> "500"
  GeneralErrorCode01     -> "501"
  GeneralErrorCode02     -> "502"
  GeneralErrorCode03     -> "503"
  GeneralErrorCode04     -> "504"
  GeneralErrorCode05     -> "505"
  GeneralErrorCode06     -> "506"
