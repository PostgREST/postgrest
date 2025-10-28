module PostgREST.Error.ApiRequestError
  ( ApiRequestError(..),
    QPError(..),
    RaiseError(..),
    RangeError(..),
    PgRaiseErrMessage(..),
    PgRaiseErrDetails(..),
    parseRaisePGRST,
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Network.HTTP.Types.Status as HTTP

import PostgREST.Error.Algebra
import Protolude

data ApiRequestError
  = AggregatesNotAllowed
  | MediaTypeError [ByteString]
  | InvalidBody ByteString
  | InvalidFilters
  | InvalidPreferences [ByteString]
  | InvalidRange RangeError
  | InvalidRpcMethod ByteString
  | NotEmbedded Text
  | NotImplemented Text
  | PutLimitNotAllowedError
  | QueryParamError QPError
  | RelatedOrderNotToOne Text Text
  | UnacceptableFilter Text
  | UnacceptableSchema Text [Text]
  | UnsupportedMethod ByteString
  | GucHeadersError
  | GucStatusError
  | PutMatchingPkError
  | SingularityError Integer
  | PGRSTParseError RaiseError
  | MaxAffectedViolationError Integer
  | InvalidResourcePath
  | OpenAPIDisabled
  | MaxAffectedRpcViolation
  deriving Show

data QPError = QPError Text Text
  deriving Show

data RaiseError
  = MsgParseError ByteString
  | DetParseError ByteString
  | NoDetail
  deriving Show

data RangeError
  = NegativeLimit
  | LowerGTUpper
  | OutOfBounds Text Text
  deriving Show

instance PgrstError ApiRequestError where
  status AggregatesNotAllowed{}      = HTTP.status400
  status MediaTypeError{}            = HTTP.status406
  status InvalidBody{}               = HTTP.status400
  status InvalidFilters              = HTTP.status405
  status InvalidPreferences{}        = HTTP.status400
  status InvalidRpcMethod{}          = HTTP.status405
  status InvalidRange{}              = HTTP.status416

  status NotEmbedded{}               = HTTP.status400
  status NotImplemented{}            = HTTP.status400
  status PutLimitNotAllowedError     = HTTP.status400
  status QueryParamError{}           = HTTP.status400
  status RelatedOrderNotToOne{}      = HTTP.status400
  status UnacceptableFilter{}        = HTTP.status400
  status UnacceptableSchema{}        = HTTP.status406
  status UnsupportedMethod{}         = HTTP.status405
  status GucHeadersError             = HTTP.status500
  status GucStatusError              = HTTP.status500
  status PutMatchingPkError          = HTTP.status400
  status SingularityError{}          = HTTP.status406
  status PGRSTParseError{}           = HTTP.status500
  status MaxAffectedViolationError{} = HTTP.status400
  status InvalidResourcePath         = HTTP.status404
  status OpenAPIDisabled             = HTTP.status404
  status MaxAffectedRpcViolation     = HTTP.status400

  headers _ = mempty

-- Error codes:
--
-- Error codes are grouped by common modules or characteristics
-- New group of errors will be added at the end of all the groups and will have the next prefix in the sequence
-- Keep the "PGRST" prefix in every code for an easier search/grep
-- They are grouped as following:
--
-- PGRST0xx -> Connection Error
-- PGRST1xx -> ApiRequest Error
-- PGRST2xx -> SchemaCache Error
-- PGRST3xx -> JWT authentication Error
-- PGRSTXxx -> Internal Hasql Error

instance ErrorBody ApiRequestError where
  -- CODE: Text
  code QueryParamError{}           = "PGRST100"
  code InvalidRpcMethod{}          = "PGRST101"
  code InvalidBody{}               = "PGRST102"
  code InvalidRange{}              = "PGRST103"
  -- code ParseRequestError           = "PGRST104" -- no longer used
  code InvalidFilters              = "PGRST105"
  code UnacceptableSchema{}        = "PGRST106"
  code MediaTypeError{}            = "PGRST107"
  code NotEmbedded{}               = "PGRST108"
  -- code LimitNoOrderError           = "PGRST109" -- no longer used
  -- code OffLimitsChangesError       = "PGRST110" -- no longer used
  code GucHeadersError             = "PGRST111"
  code GucStatusError              = "PGRST112"
  -- code BinaryFieldError            = "PGRST113" -- no longer used
  code PutLimitNotAllowedError     = "PGRST114"
  code PutMatchingPkError          = "PGRST115"
  code SingularityError{}          = "PGRST116"
  code UnsupportedMethod{}         = "PGRST117"
  code RelatedOrderNotToOne{}      = "PGRST118"
  -- code SpreadNotToOne              = "PGRST109" -- no longer used
  code UnacceptableFilter{}        = "PGRST120"
  code PGRSTParseError{}           = "PGRST121"
  code InvalidPreferences{}        = "PGRST122"
  code AggregatesNotAllowed        = "PGRST123"
  code MaxAffectedViolationError{} = "PGRST124"
  code InvalidResourcePath         = "PGRST125"
  code OpenAPIDisabled             = "PGRST126"
  code NotImplemented{}            = "PGRST127"
  code MaxAffectedRpcViolation     = "PGRST128"

  -- MESSAGE: Text
  message (QueryParamError (QPError msg _)) = msg
  message (InvalidRpcMethod method)    = "Cannot use the " <> T.decodeUtf8 method <> " method on RPC"
  message (InvalidBody errorMessage)   = T.decodeUtf8 errorMessage
  message (InvalidRange _)             = "Requested range not satisfiable"
  message InvalidFilters               = "Filters must include all and only primary key columns with 'eq' operators"
  message (UnacceptableSchema sch _)   = "Invalid schema: " <> sch
  message (MediaTypeError cts)         = "None of these media types are available: " <> T.intercalate ", " (map T.decodeUtf8 cts)
  message (NotEmbedded resource)       = "'" <> resource <> "' is not an embedded resource in this request"
  message GucHeadersError              = "response.headers guc must be a JSON array composed of objects with a single key and a string value"
  message GucStatusError               = "response.status guc must be a valid status code"
  message PutLimitNotAllowedError      = "limit/offset querystring parameters are not allowed for PUT"
  message PutMatchingPkError           = "Payload values do not match URL in primary key column(s)"
  message (SingularityError _)         = "Cannot coerce the result to a single JSON object"
  message (UnsupportedMethod method)   = "Unsupported HTTP method: " <> T.decodeUtf8 method
  message (RelatedOrderNotToOne _ target) = "A related order on '" <> target <> "' is not possible"
  message (UnacceptableFilter target)    = "Bad operator on the '" <> target <> "' embedded resource"
  message (PGRSTParseError _)            = "Could not parse JSON in the \"RAISE SQLSTATE 'PGRST'\" error"
  message (InvalidPreferences _)         = "Invalid preferences given with handling=strict"
  message AggregatesNotAllowed           = "Use of aggregate functions is not allowed"
  message (MaxAffectedViolationError _)  = "Query result exceeds max-affected preference constraint"
  message InvalidResourcePath            = "Invalid path specified in request URL"
  message OpenAPIDisabled                = "Root endpoint metadata is disabled"
  message (NotImplemented _)             = "Feature not implemented"
  message MaxAffectedRpcViolation        = "Function must return SETOF or TABLE when max-affected preference is used with handling=strict"

  -- DETAILS: Maybe JSON.Value
  details (QueryParamError (QPError _ dets)) = Just $ JSON.String dets
  details (InvalidRange rangeError) = Just $
    case rangeError of
       NegativeLimit           -> "Limit should be greater than or equal to zero."
       LowerGTUpper            -> "The lower boundary must be lower than or equal to the upper boundary in the Range header."
       OutOfBounds lower total -> JSON.String $ "An offset of " <> lower <> " was requested, but there are only " <> total <> " rows."
  details (SingularityError n) = Just $ JSON.String $ T.unwords ["The result contains", show n, "rows"]
  details (RelatedOrderNotToOne origin target) = Just $ JSON.String $ "'" <> origin <> "' and '" <> target <> "' do not form a many-to-one or one-to-one relationship"
  details (UnacceptableFilter _)      = Just "Only is null or not is null filters are allowed on embedded resources"
  details (PGRSTParseError raiseErr) = Just $ JSON.String $ pgrstParseErrorDetails raiseErr
  details (InvalidPreferences prefs) = Just $ JSON.String $ T.decodeUtf8 ("Invalid preferences: " <> BS.intercalate ", " prefs)
  details (MaxAffectedViolationError n) = Just $ JSON.String $ T.unwords ["The query affects", show n, "rows"]
  details (NotImplemented details') = Just $ JSON.String details'

  details _ = Nothing

  -- HINT: Maybe JSON.Value
  hint (NotEmbedded resource) = Just $ JSON.String $ "Verify that '" <> resource <> "' is included in the 'select' query parameter."
  hint (PGRSTParseError raiseErr) = Just $ JSON.String $ pgrstParseErrorHint raiseErr
  hint (UnacceptableSchema _ schemas) = Just $ JSON.String $ "Only the following schemas are exposed: "  <> T.intercalate ", " schemas

  hint _ = Nothing

instance JSON.ToJSON ApiRequestError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

pgrstParseErrorHint :: RaiseError -> Text
pgrstParseErrorHint err = case err of
  MsgParseError _ -> "MESSAGE must be a JSON object with obligatory keys: 'code', 'message' and optional keys: 'details', 'hint'."
  _               -> "DETAIL must be a JSON object with obligatory keys: 'status', 'headers' and optional key: 'status_text'."

pgrstParseErrorDetails :: RaiseError -> Text
pgrstParseErrorDetails err = case err of
  MsgParseError m -> "Invalid JSON value for MESSAGE: '" <> T.decodeUtf8 m <> "'"
  DetParseError d -> "Invalid JSON value for DETAIL: '" <> T.decodeUtf8 d <> "'"
  NoDetail        -> "DETAIL is missing in the RAISE statement"



-- For parsing byteString to JSON Object, used for allowing full response control
data PgRaiseErrMessage = PgRaiseErrMessage {
  getCode    :: Text,
  getMessage :: Text,
  getDetails :: Maybe Text,
  getHint    :: Maybe Text
}

instance JSON.FromJSON PgRaiseErrMessage where
  parseJSON (JSON.Object m) =
    PgRaiseErrMessage
      <$> m JSON..: "code"
      <*> m JSON..: "message"
      <*> m JSON..:? "details"
      <*> m JSON..:? "hint"

  parseJSON _ = mzero

data PgRaiseErrDetails = PgRaiseErrDetails {
  getStatus     :: Int,
  getStatusText :: Maybe Text,
  getHeaders    :: Map Text Text
}

instance JSON.FromJSON PgRaiseErrDetails where
  parseJSON (JSON.Object d) =
    PgRaiseErrDetails
      <$> d JSON..: "status"
      <*> d JSON..:? "status_text"
      <*> d JSON..: "headers"

  parseJSON _ = mzero

parseRaisePGRST :: ByteString -> Maybe ByteString -> Either ApiRequestError (PgRaiseErrMessage, PgRaiseErrDetails)
parseRaisePGRST m d = do
  msgJson <- maybeToRight (PGRSTParseError $ MsgParseError m) (JSON.decodeStrict m)
  det <- maybeToRight (PGRSTParseError NoDetail) d
  detJson <- maybeToRight (PGRSTParseError $ DetParseError det) (JSON.decodeStrict det)
  return (msgJson, detJson)
