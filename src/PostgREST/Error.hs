{-|
Module      : PostgREST.Error
Description : PostgREST error HTTP responses
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Error
  ( errorResponseFor
  , ApiRequestError(..)
  , QPError(..)
  , RangeError(..)
  , PgError(..)
  , Error(..)
  , errorPayload
  , status
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.CaseInsensitive      as CI
import qualified Data.FuzzySet             as Fuzzy
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map.Internal         as M
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Encoding.Error  as T
import qualified Hasql.Pool                as SQL
import qualified Hasql.Session             as SQL
import qualified Network.HTTP.Types.Status as HTTP

import Data.Aeson  ((.:), (.:?), (.=))
import Network.Wai (Response, responseLBS)

import Network.HTTP.Types.Header (Header)

import           PostgREST.MediaType (MediaType (..))
import qualified PostgREST.MediaType as MediaType

import PostgREST.SchemaCache.Identifiers  (QualifiedIdentifier (..),
                                           Schema)
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..),
                                           RelationshipsMap)
import PostgREST.SchemaCache.Routine      (Routine (..),
                                           RoutineParam (..))
import PostgREST.SchemaCache.Table        (Table (..))
import Protolude


class (JSON.ToJSON a) => PgrstError a where
  status   :: a -> HTTP.Status
  headers  :: a -> [Header]

  errorPayload :: a -> LByteString
  errorPayload = JSON.encode

  errorResponseFor :: a -> Response
  errorResponseFor err =
    let baseHeader = MediaType.toContentType MTApplicationJSON in
    responseLBS (status err) (baseHeader : headers err) $ errorPayload err

data ApiRequestError
  = AggregatesNotAllowed
  | AmbiguousRelBetween Text Text [Relationship]
  | AmbiguousRpc [Routine]
  | MediaTypeError [ByteString]
  | InvalidBody ByteString
  | InvalidFilters
  | InvalidPreferences [ByteString]
  | InvalidRange RangeError
  | InvalidRpcMethod ByteString
  | NotFound
  | NoRelBetween Text Text (Maybe Text) Text RelationshipsMap
  | NoRpc Text Text [Text] MediaType Bool [QualifiedIdentifier] [Routine]
  | NotEmbedded Text
  | PutLimitNotAllowedError
  | QueryParamError QPError
  | RelatedOrderNotToOne Text Text
  | SpreadNotToOne Text Text
  | UnacceptableFilter Text
  | UnacceptableSchema [Text]
  | UnsupportedMethod ByteString
  | ColumnNotFound Text Text
  | TableNotFound Text Text [Table]
  | GucHeadersError
  | GucStatusError
  | PutMatchingPkError
  | SingularityError Integer
  | PGRSTParseError RaiseError
  | MaxAffectedViolationError Integer
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
  status AmbiguousRelBetween{}       = HTTP.status300
  status AmbiguousRpc{}              = HTTP.status300
  status MediaTypeError{}            = HTTP.status406
  status InvalidBody{}               = HTTP.status400
  status InvalidFilters              = HTTP.status405
  status InvalidPreferences{}        = HTTP.status400
  status InvalidRpcMethod{}          = HTTP.status405
  status InvalidRange{}              = HTTP.status416
  status NotFound                    = HTTP.status404

  status NoRelBetween{}              = HTTP.status400
  status NoRpc{}                     = HTTP.status404
  status NotEmbedded{}               = HTTP.status400
  status PutLimitNotAllowedError     = HTTP.status400
  status QueryParamError{}           = HTTP.status400
  status RelatedOrderNotToOne{}      = HTTP.status400
  status SpreadNotToOne{}            = HTTP.status400
  status UnacceptableFilter{}        = HTTP.status400
  status UnacceptableSchema{}        = HTTP.status406
  status UnsupportedMethod{}         = HTTP.status405
  status ColumnNotFound{}            = HTTP.status400
  status TableNotFound{}             = HTTP.status404
  status GucHeadersError             = HTTP.status500
  status GucStatusError              = HTTP.status500
  status PutMatchingPkError          = HTTP.status400
  status SingularityError{}          = HTTP.status406
  status PGRSTParseError{}           = HTTP.status500
  status MaxAffectedViolationError{} = HTTP.status400

  headers _ = mempty

toJsonPgrstError :: ErrorCode -> Text -> Maybe JSON.Value -> Maybe JSON.Value -> JSON.Value
toJsonPgrstError code msg details hint = JSON.object [
    "code"     .= code
  , "message"  .= msg
  , "details"  .= details
  , "hint"     .= hint
  ]

instance JSON.ToJSON ApiRequestError where
  toJSON (QueryParamError (QPError message details)) = toJsonPgrstError
    ApiRequestErrorCode00 message (Just (JSON.String details)) Nothing

  toJSON (InvalidRpcMethod method) = toJsonPgrstError
    ApiRequestErrorCode01 ("Cannot use the " <> T.decodeUtf8 method <> " method on RPC") Nothing Nothing

  toJSON (InvalidBody errorMessage) = toJsonPgrstError
    ApiRequestErrorCode02 (T.decodeUtf8 errorMessage) Nothing Nothing

  toJSON (InvalidRange rangeError) = toJsonPgrstError
    ApiRequestErrorCode03
    "Requested range not satisfiable"
    (Just $ case rangeError of
       NegativeLimit           -> "Limit should be greater than or equal to zero."
       LowerGTUpper            -> "The lower boundary must be lower than or equal to the upper boundary in the Range header."
       OutOfBounds lower total -> JSON.String $ "An offset of " <> lower <> " was requested, but there are only " <> total <> " rows.")
    Nothing

  toJSON InvalidFilters = toJsonPgrstError
    ApiRequestErrorCode05 "Filters must include all and only primary key columns with 'eq' operators" Nothing Nothing

  toJSON (UnacceptableSchema schemas) = toJsonPgrstError
    ApiRequestErrorCode06 ("The schema must be one of the following: " <> T.intercalate ", " schemas) Nothing Nothing

  toJSON (MediaTypeError cts) = toJsonPgrstError
    ApiRequestErrorCode07 ("None of these media types are available: " <> T.intercalate ", " (map T.decodeUtf8 cts)) Nothing Nothing

  toJSON NotFound = JSON.object []

  toJSON (NotEmbedded resource) = toJsonPgrstError
    ApiRequestErrorCode08
    ("'" <> resource <> "' is not an embedded resource in this request")
    Nothing
    (Just $ JSON.String $ "Verify that '" <> resource <> "' is included in the 'select' query parameter.")

  toJSON GucHeadersError = toJsonPgrstError
    ApiRequestErrorCode11 "response.headers guc must be a JSON array composed of objects with a single key and a string value" Nothing Nothing

  toJSON GucStatusError = toJsonPgrstError
    ApiRequestErrorCode12 "response.status guc must be a valid status code" Nothing Nothing

  toJSON PutLimitNotAllowedError = toJsonPgrstError
    ApiRequestErrorCode14 "limit/offset querystring parameters are not allowed for PUT" Nothing Nothing

  toJSON PutMatchingPkError = toJsonPgrstError
    ApiRequestErrorCode15 "Payload values do not match URL in primary key column(s)" Nothing Nothing

  toJSON (SingularityError n) = toJsonPgrstError
    ApiRequestErrorCode16
    "Cannot coerce the result to a single JSON object"
    (Just $ JSON.String $ T.unwords ["The result contains", show n, "rows"])
    Nothing

  toJSON (UnsupportedMethod method) = toJsonPgrstError
    ApiRequestErrorCode17 ("Unsupported HTTP method: " <> T.decodeUtf8 method) Nothing Nothing

  toJSON (RelatedOrderNotToOne origin target) = toJsonPgrstError
    ApiRequestErrorCode18
    ("A related order on '" <> target <> "' is not possible")
    (Just $ JSON.String $ "'" <> origin <> "' and '" <> target <> "' do not form a many-to-one or one-to-one relationship")
    Nothing

  toJSON (SpreadNotToOne origin target) = toJsonPgrstError
    ApiRequestErrorCode19
    ("A spread operation on '" <> target <> "' is not possible")
    (Just $ JSON.String $ "'" <> origin <> "' and '" <> target <> "' do not form a many-to-one or one-to-one relationship")
    Nothing

  toJSON (UnacceptableFilter target) = toJsonPgrstError
    ApiRequestErrorCode20
    ("Bad operator on the '" <> target <> "' embedded resource")
    (Just "Only is null or not is null filters are allowed on embedded resources")
    Nothing

  toJSON (PGRSTParseError raiseErr) = toJsonPgrstError
    ApiRequestErrorCode21
    "Could not parse JSON in the \"RAISE SQLSTATE 'PGRST'\" error"
    (Just $ JSON.String $ pgrstParseErrorDetails raiseErr)
    (Just $ JSON.String $ pgrstParseErrorHint raiseErr)

  toJSON (InvalidPreferences prefs) = toJsonPgrstError
    ApiRequestErrorCode22
    "Invalid preferences given with handling=strict"
    (Just $ JSON.String $ T.decodeUtf8 ("Invalid preferences: " <> BS.intercalate ", " prefs))
    Nothing

  toJSON AggregatesNotAllowed = toJsonPgrstError
    ApiRequestErrorCode23 "Use of aggregate functions is not allowed" Nothing Nothing

  toJSON (MaxAffectedViolationError n) = toJsonPgrstError
    ApiRequestErrorCode24
    "Query result exceeds max-affected preference constraint"
    (Just $ JSON.String $ T.unwords ["The query affects", show n, "rows"])
    Nothing

  toJSON (NoRelBetween parent child embedHint schema allRels) = toJsonPgrstError
    SchemaCacheErrorCode00
    ("Could not find a relationship between '" <> parent <> "' and '" <> child <> "' in the schema cache")
    (Just $ JSON.String $ "Searched for a foreign key relationship between '" <> parent <> "' and '" <> child <> maybe mempty ("' using the hint '" <>) embedHint <> "' in the schema '" <> schema <> "', but no matches were found.")
    (JSON.String <$> noRelBetweenHint parent child schema allRels)

  toJSON (AmbiguousRelBetween parent child rels) = toJsonPgrstError
    SchemaCacheErrorCode01
    ("Could not embed because more than one relationship was found for '" <> parent <> "' and '" <> child <> "'")
    (Just $ JSON.toJSONList (compressedRel <$> rels))
    (Just $ JSON.String $ "Try changing '" <> child <> "' to one of the following: " <> relHint rels <> ". Find the desired relationship in the 'details' key.")

  toJSON (NoRpc schema procName argumentKeys contentType isInvPost allProcs overloadedProcs)  =
    let func = schema <> "." <> procName
        prms = T.intercalate ", " argumentKeys
        prmsMsg = "(" <> prms <> ")"
        prmsDet = " with parameter" <> (if length argumentKeys > 1 then "s " else " ") <> prms
        fmtPrms p = if null argumentKeys then " without parameters" else p
        onlySingleParams = isInvPost && contentType `elem` [MTTextPlain, MTTextXML, MTOctetStream]
    in toJsonPgrstError
    SchemaCacheErrorCode02
    ("Could not find the function " <> func <> (if onlySingleParams then "" else fmtPrms prmsMsg) <> " in the schema cache")
    (Just $ JSON.String $ "Searched for the function " <> func <>
      (case (isInvPost, contentType) of
        (True, MTTextPlain)       -> " with a single unnamed text parameter"
        (True, MTTextXML)         -> " with a single unnamed xml parameter"
        (True, MTOctetStream)     -> " with a single unnamed bytea parameter"
        (True, MTApplicationJSON) -> fmtPrms prmsDet <> " or with a single unnamed json/jsonb parameter"
        _                         -> fmtPrms prmsDet) <>
      ", but no matches were found in the schema cache.")
    -- The hint will be null in the case of single unnamed parameter functions
    (if onlySingleParams
      then Nothing
      else JSON.String <$> noRpcHint schema procName argumentKeys allProcs overloadedProcs)

  toJSON (AmbiguousRpc procs)  = toJsonPgrstError
    SchemaCacheErrorCode03
    ("Could not choose the best candidate function between: " <> T.intercalate ", " [pdSchema p <> "." <> pdName p <> "(" <> T.intercalate ", " [ppName a <> " => " <> ppType a | a <- pdParams p] <> ")" | p <- procs])
    Nothing
    (Just "Try renaming the parameters or the function itself in the database so function overloading can be resolved")

  toJSON (ColumnNotFound relName colName) = toJsonPgrstError
    SchemaCacheErrorCode04 ("Could not find the '" <> colName <> "' column of '" <> relName <> "' in the schema cache") Nothing Nothing

  toJSON (TableNotFound schemaName relName tbls) = toJsonPgrstError
    SchemaCacheErrorCode05
    ("Could not find the table '" <> schemaName <> "." <> relName <> "' in the schema cache")
    Nothing
    (JSON.String <$> tableNotFoundHint schemaName relName tbls)

-- |
-- If no relationship is found then:
--
-- Looks for parent suggestions if parent not found
-- Looks for child suggestions if parent is found but child is not
-- Gives no suggestions if both are found (it means that there is a problem with the embed hint)
--
-- >>> :set -Wno-missing-fields
-- >>> let qi t = QualifiedIdentifier "api" t
-- >>> let rel ft = Relationship{relForeignTable = qi ft}
-- >>> let rels = HM.fromList [((qi "films", "api"), [rel "directors", rel "roles", rel "actors"])]
--
-- >>> noRelBetweenHint "film" "directors" "api" rels
-- Just "Perhaps you meant 'films' instead of 'film'."
--
-- >>> noRelBetweenHint "films" "role" "api" rels
-- Just "Perhaps you meant 'roles' instead of 'role'."
--
-- >>> noRelBetweenHint "films" "role" "api" rels
-- Just "Perhaps you meant 'roles' instead of 'role'."
--
-- >>> noRelBetweenHint "films" "actors" "api" rels
-- Nothing
--
-- >>> noRelBetweenHint "noclosealternative" "roles" "api" rels
-- Nothing
--
-- >>> noRelBetweenHint "films" "noclosealternative" "api" rels
-- Nothing
--
-- >>> noRelBetweenHint "films" "noclosealternative" "noclosealternative" rels
-- Nothing
--
noRelBetweenHint :: Text -> Text -> Schema -> RelationshipsMap -> Maybe Text
noRelBetweenHint parent child schema allRels = ("Perhaps you meant '" <>) <$>
  if isJust findParent
    then (<> "' instead of '" <> child <> "'.") <$> suggestChild
    else (<> "' instead of '" <> parent <> "'.") <$> suggestParent
  where
    findParent = HM.lookup (QualifiedIdentifier schema parent, schema) allRels
    fuzzySetOfParents  = Fuzzy.fromList [qiName (fst p) | p <- HM.keys allRels, snd p == schema]
    fuzzySetOfChildren = Fuzzy.fromList [qiName (relForeignTable c) | c <- fromMaybe [] findParent]
    suggestParent = Fuzzy.getOne fuzzySetOfParents parent
    -- Do not give suggestion if the child is found in the relations (weight = 1.0)
    suggestChild  = headMay [snd k | k <- Fuzzy.get fuzzySetOfChildren child, fst k < 1.0]

-- |
-- If no function is found with the given name, it does a fuzzy search to all the functions
-- in the same schema and shows the best match as hint.
--
-- >>> :set -Wno-missing-fields
-- >>> let procs = [(QualifiedIdentifier "api" "test"), (QualifiedIdentifier "api" "another"), (QualifiedIdentifier "private" "other")]
--
-- >>> noRpcHint "api" "testt" ["val", "param", "name"] procs []
-- Just "Perhaps you meant to call the function api.test"
--
-- >>> noRpcHint "api" "other" [] procs []
-- Just "Perhaps you meant to call the function api.another"
--
-- >>> noRpcHint "api" "noclosealternative" [] procs []
-- Nothing
--
-- If a function is found with the given name, but no params match, then it does a fuzzy search
-- to all the overloaded functions' params using the form "param1, param2, param3, ..."
-- and shows the best match as hint.
--
-- >>> let procsDesc = [Function {pdParams = [RoutineParam {ppName="val"}, RoutineParam {ppName="param"}, RoutineParam {ppName="name"}]}, Function {pdParams = [RoutineParam {ppName="id"}, RoutineParam {ppName="attr"}]}]
--
-- >>> noRpcHint "api" "test" ["vall", "pqaram", "nam"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(name, param, val)"
--
-- >>> noRpcHint "api" "test" ["val", "param"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(name, param, val)"
--
-- >>> noRpcHint "api" "test" ["id", "attrs"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(attr, id)"
--
-- >>> noRpcHint "api" "test" ["id"] procs procsDesc
-- Just "Perhaps you meant to call the function api.test(attr, id)"
--
-- >>> noRpcHint "api" "test" ["noclosealternative"] procs procsDesc
-- Nothing
--
noRpcHint :: Text -> Text -> [Text] -> [QualifiedIdentifier] -> [Routine] -> Maybe Text
noRpcHint schema procName params allProcs overloadedProcs =
  fmap (("Perhaps you meant to call the function " <> schema <> ".") <>) possibleProcs
  where
    fuzzySetOfProcs  = Fuzzy.fromList [qiName k | k <- allProcs, qiSchema k == schema]
    fuzzySetOfParams = Fuzzy.fromList $ listToText <$> [[ppName prm | prm <- pdParams ov] | ov <- overloadedProcs]
    -- Cannot do a fuzzy search like: Fuzzy.getOne [[Text]] [Text], where [[Text]] is the list of params for each
    -- overloaded function and [Text] the given params. This converts those lists to text to make fuzzy search possible.
    -- E.g. ["val", "param", "name"] into "(name, param, val)"
    listToText       = ("(" <>) . (<> ")") . T.intercalate ", " . sort
    possibleProcs
      | null overloadedProcs = Fuzzy.getOne fuzzySetOfProcs procName
      | otherwise            = (procName <>) <$> Fuzzy.getOne fuzzySetOfParams (listToText params)

-- |
-- Do a fuzzy search in all tables in the same schema and return closest result
tableNotFoundHint :: Text -> Text -> [Table] -> Maybe Text
tableNotFoundHint schema tblName tblList
  = fmap (\tbl -> "Perhaps you meant the table '" <> schema <> "." <> tbl <> "'") perhapsTable
    where
      perhapsTable = Fuzzy.getOne fuzzyTableSet tblName
      fuzzyTableSet = Fuzzy.fromList [ tableName tbl | tbl <- tblList, tableSchema tbl == schema]


compressedRel :: Relationship -> JSON.Value
-- An ambiguousness error cannot happen for computed relationships TODO refactor so this mempty is not needed
compressedRel ComputedRelationship{} = JSON.object mempty
compressedRel Relationship{..} =
  let
    fmtEls els = "(" <> T.intercalate ", " els <> ")"
  in
  JSON.object $
    ("embedding" .= (qiName relTable <> " with " <> qiName relForeignTable :: Text))
    : case relCardinality of
        M2M Junction{..} -> [
            "cardinality" .= ("many-to-many" :: Text)
          , "relationship" .= (qiName junTable <> " using " <> junConstraint1 <> fmtEls (snd <$> junColsSource) <> " and " <> junConstraint2 <> fmtEls (snd <$> junColsTarget))
          ]
        M2O cons relColumns -> [
            "cardinality" .= ("many-to-one" :: Text)
          , "relationship" .= (cons <> " using " <> qiName relTable <> fmtEls (fst <$> relColumns) <> " and " <> qiName relForeignTable <> fmtEls (snd <$> relColumns))
          ]
        O2O cons relColumns _ -> [
            "cardinality" .= ("one-to-one" :: Text)
          , "relationship" .= (cons <> " using " <> qiName relTable <> fmtEls (fst <$> relColumns) <> " and " <> qiName relForeignTable <> fmtEls (snd <$> relColumns))
          ]
        O2M cons relColumns -> [
            "cardinality" .= ("one-to-many" :: Text)
          , "relationship" .= (cons <> " using " <> qiName relTable <> fmtEls (fst <$> relColumns) <> " and " <> qiName relForeignTable <> fmtEls (snd <$> relColumns))
          ]

relHint :: [Relationship] -> Text
relHint rels = T.intercalate ", " (hintList <$> rels)
  where
    hintList Relationship{..} =
      let buildHint rel = "'" <> qiName relForeignTable <> "!" <> rel <> "'" in
      case relCardinality of
        M2M Junction{..} -> buildHint (qiName junTable)
        M2O cons _       -> buildHint cons
        O2O cons _ _     -> buildHint cons
        O2M cons _       -> buildHint cons
    -- An ambiguousness error cannot happen for computed relationships TODO refactor so this mempty is not needed
    hintList ComputedRelationship{} = mempty

pgrstParseErrorDetails :: RaiseError -> Text
pgrstParseErrorDetails err = case err of
  MsgParseError m -> "Invalid JSON value for MESSAGE: '" <> T.decodeUtf8 m <> "'"
  DetParseError d -> "Invalid JSON value for DETAIL: '" <> T.decodeUtf8 d <> "'"
  NoDetail        -> "DETAIL is missing in the RAISE statement"

pgrstParseErrorHint :: RaiseError -> Text
pgrstParseErrorHint err = case err of
  MsgParseError _ -> "MESSAGE must be a JSON object with obligatory keys: 'code', 'message' and optional keys: 'details', 'hint'."
  _               -> "DETAIL must be a JSON object with obligatory keys: 'status', 'headers' and optional key: 'status_text'."

data PgError = PgError Authenticated SQL.UsageError
type Authenticated = Bool

instance PgrstError PgError where
  status (PgError authed usageError) = pgErrorStatus authed usageError

  headers (PgError _ (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError (SQL.ServerError "PGRST" m d _ _p))))) =
    case parseRaisePGRST m d of
      Right (_, r) -> map intoHeader (M.toList $ getHeaders r)
      Left e       -> headers e
    where
      intoHeader (k,v) = (CI.mk $ T.encodeUtf8 k, T.encodeUtf8 v)

  headers err =
    if status err == HTTP.status401
       then [("WWW-Authenticate", "Bearer") :: Header]
       else mempty

instance JSON.ToJSON PgError where
  toJSON (PgError _ usageError) = JSON.toJSON usageError

instance JSON.ToJSON SQL.UsageError where
  toJSON (SQL.ConnectionUsageError e) = toJsonPgrstError
    ConnectionErrorCode00
    "Database connection error. Retrying the connection."
    (Just $ JSON.String $ T.decodeUtf8With T.lenientDecode $ fromMaybe "" e)
    Nothing

  toJSON (SQL.SessionUsageError e) = JSON.toJSON e -- SQL.Error

  toJSON SQL.AcquisitionTimeoutUsageError = toJsonPgrstError
    ConnectionErrorCode03 "Timed out acquiring connection from connection pool." Nothing Nothing

instance JSON.ToJSON SQL.QueryError where
  toJSON (SQL.QueryError _ _ e) = JSON.toJSON e

instance JSON.ToJSON SQL.CommandError where
  -- Special error raised with code PGRST, to allow full response control
  toJSON (SQL.ResultError (SQL.ServerError "PGRST" m d _ _p)) =
    case parseRaisePGRST m d of
      Right (r, _) -> JSON.object [
        "code"     .= getCode r,
        "message"  .= getMessage r,
        "details"  .= checkMaybe (getDetails r),
        "hint"     .= checkMaybe (getHint r)]
      Left e      -> JSON.toJSON e
    where
      checkMaybe = maybe JSON.Null JSON.String

  toJSON (SQL.ResultError (SQL.ServerError c m d h _p)) = JSON.object [
    "code"     .= (T.decodeUtf8 c      :: Text),
    "message"  .= (T.decodeUtf8 m      :: Text),
    "details"  .= (fmap T.decodeUtf8 d :: Maybe Text),
    "hint"     .= (fmap T.decodeUtf8 h :: Maybe Text)]

  toJSON (SQL.ResultError resultError) = toJsonPgrstError
    InternalErrorCode00 (show resultError) Nothing Nothing

  toJSON (SQL.ClientError d) = toJsonPgrstError
    ConnectionErrorCode01 "Database client error. Retrying the connection." (JSON.String <$> fmap T.decodeUtf8 d) Nothing

pgErrorStatus :: Bool -> SQL.UsageError -> HTTP.Status
pgErrorStatus _      (SQL.ConnectionUsageError _) = HTTP.status503
pgErrorStatus _      SQL.AcquisitionTimeoutUsageError = HTTP.status504
pgErrorStatus _      (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ClientError _)))      = HTTP.status503
pgErrorStatus authed (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError rError))) =
  case rError of
    (SQL.ServerError c m d _ _) ->
      case BS.unpack c of
        '0':'8':_ -> HTTP.status503 -- pg connection err
        '0':'9':_ -> HTTP.status500 -- triggered action exception
        '0':'L':_ -> HTTP.status403 -- invalid grantor
        '0':'P':_ -> HTTP.status403 -- invalid role specification
        "23503"   -> HTTP.status409 -- foreign_key_violation
        "23505"   -> HTTP.status409 -- unique_violation
        "25006"   -> HTTP.status405 -- read_only_sql_transaction
        "21000"   -> -- cardinality_violation
          if BS.isSuffixOf "requires a WHERE clause" m
            then HTTP.status400 -- special case for pg-safeupdate, which we consider as client error
            else HTTP.status500 -- generic function or view server error, e.g. "more than one row returned by a subquery used as an expression"
        '2':'5':_ -> HTTP.status500 -- invalid tx state
        '2':'8':_ -> HTTP.status403 -- invalid auth specification
        '2':'D':_ -> HTTP.status500 -- invalid tx termination
        '3':'8':_ -> HTTP.status500 -- external routine exception
        '3':'9':_ -> HTTP.status500 -- external routine invocation
        '3':'B':_ -> HTTP.status500 -- savepoint exception
        '4':'0':_ -> HTTP.status500 -- tx rollback
        "53400"   -> HTTP.status500 -- config limit exceeded
        '5':'3':_ -> HTTP.status503 -- insufficient resources
        '5':'4':_ -> HTTP.status500 -- too complex
        '5':'5':_ -> HTTP.status500 -- obj not on prereq state
        "57P01"   -> HTTP.status503 -- terminating connection due to administrator command
        '5':'7':_ -> HTTP.status500 -- operator intervention
        '5':'8':_ -> HTTP.status500 -- system error
        'F':'0':_ -> HTTP.status500 -- conf file error
        'H':'V':_ -> HTTP.status500 -- foreign data wrapper error
        "P0001"   -> HTTP.status400 -- default code for "raise"
        'P':'0':_ -> HTTP.status500 -- PL/pgSQL Error
        'X':'X':_ -> HTTP.status500 -- internal Error
        "42883"-> if BS.isPrefixOf "function xmlagg(" m
          then HTTP.status406
          else HTTP.status404 -- undefined function
        "42P01"   -> HTTP.status404 -- undefined table
        "42P17"   -> HTTP.status500 -- infinite recursion
        "42501"   -> if authed then HTTP.status403 else HTTP.status401 -- insufficient privilege
        'P':'T':n -> fromMaybe HTTP.status500 (HTTP.mkStatus <$> readMaybe n <*> pure m)
        "PGRST"   ->
          case parseRaisePGRST m d of
            Right (_, r) -> maybe (toEnum $ getStatus r) (HTTP.mkStatus (getStatus r) . T.encodeUtf8) (getStatusText r)
            Left e       -> status e
        _         -> HTTP.status400

    _                       -> HTTP.status500


data Error
  = ApiRequestError ApiRequestError
  | JwtTokenInvalid Text
  | JwtTokenMissing
  | JwtTokenRequired
  | NoSchemaCacheError
  | PgErr PgError

instance PgrstError Error where
  status (ApiRequestError err) = status err
  status JwtTokenInvalid{}     = HTTP.unauthorized401
  status JwtTokenMissing       = HTTP.status500
  status JwtTokenRequired      = HTTP.unauthorized401
  status NoSchemaCacheError    = HTTP.status503
  status (PgErr err)           = status err

  headers (ApiRequestError err) = headers err
  headers (JwtTokenInvalid m)   = [invalidTokenHeader m]
  headers JwtTokenRequired      = [requiredTokenHeader]
  headers (PgErr err)           = headers err
  headers _                     = mempty

instance JSON.ToJSON Error where
  toJSON NoSchemaCacheError = toJsonPgrstError
      ConnectionErrorCode02 "Could not query the database for the schema cache. Retrying." Nothing Nothing

  toJSON JwtTokenMissing = toJsonPgrstError
      JWTErrorCode00 "Server lacks JWT secret" Nothing Nothing

  toJSON (JwtTokenInvalid message) = toJsonPgrstError
      JWTErrorCode01 message Nothing Nothing

  toJSON JwtTokenRequired = toJsonPgrstError
      JWTErrorCode02 "Anonymous access is disabled" Nothing Nothing

  toJSON (PgErr err) = JSON.toJSON err
  toJSON (ApiRequestError err) = JSON.toJSON err

invalidTokenHeader :: Text -> Header
invalidTokenHeader m =
  ("WWW-Authenticate", "Bearer error=\"invalid_token\", " <> "error_description=" <> encodeUtf8 (show m))

requiredTokenHeader :: Header
requiredTokenHeader = ("WWW-Authenticate", "Bearer")

-- For parsing byteString to JSON Object, used for allowing full response control
data PgRaiseErrMessage = PgRaiseErrMessage {
  getCode    :: Text,
  getMessage :: Text,
  getDetails :: Maybe Text,
  getHint    :: Maybe Text
}

data PgRaiseErrDetails = PgRaiseErrDetails {
  getStatus     :: Int,
  getStatusText :: Maybe Text,
  getHeaders    :: Map Text Text
}

instance JSON.FromJSON PgRaiseErrMessage where
  parseJSON (JSON.Object m) =
    PgRaiseErrMessage
      <$> m .: "code"
      <*> m .: "message"
      <*> m .:? "details"
      <*> m .:? "hint"

  parseJSON _ = mzero

instance JSON.FromJSON PgRaiseErrDetails where
  parseJSON (JSON.Object d) =
    PgRaiseErrDetails
      <$> d .: "status"
      <*> d .:? "status_text"
      <*> d .: "headers"

  parseJSON _ = mzero

parseRaisePGRST :: ByteString -> Maybe ByteString -> Either ApiRequestError (PgRaiseErrMessage, PgRaiseErrDetails)
parseRaisePGRST m d = do
  msgJson <- maybeToRight (PGRSTParseError $ MsgParseError m) (JSON.decodeStrict m)
  det <- maybeToRight (PGRSTParseError NoDetail) d
  detJson <- maybeToRight (PGRSTParseError $ DetParseError det) (JSON.decodeStrict det)
  return (msgJson, detJson)

-- Error codes are grouped by common modules or characteristics
data ErrorCode
  -- PostgreSQL connection errors
  = ConnectionErrorCode00
  | ConnectionErrorCode01
  | ConnectionErrorCode02
  | ConnectionErrorCode03
  -- API Request errors
  | ApiRequestErrorCode00
  | ApiRequestErrorCode01
  | ApiRequestErrorCode02
  | ApiRequestErrorCode03
  -- | ApiRequestErrorCode04 -- no longer used (used to be mapped to ParseRequestError)
  | ApiRequestErrorCode05
  | ApiRequestErrorCode06
  | ApiRequestErrorCode07
  | ApiRequestErrorCode08
  -- | ApiRequestErrorCode09 -- no longer used (used to be mapped to LimitNoOrderError)
  -- | ApiRequestErrorCode10 -- no longer used (used to be mapped to OffLimitsChangesError)
  | ApiRequestErrorCode11
  -- | ApiRequestErrorCode13 -- no longer used (used to be mapped to BinaryFieldError)
  | ApiRequestErrorCode12
  | ApiRequestErrorCode14
  | ApiRequestErrorCode15
  | ApiRequestErrorCode16
  | ApiRequestErrorCode17
  | ApiRequestErrorCode18
  | ApiRequestErrorCode19
  | ApiRequestErrorCode20
  | ApiRequestErrorCode21
  | ApiRequestErrorCode22
  | ApiRequestErrorCode23
  | ApiRequestErrorCode24
  -- Schema Cache errors
  | SchemaCacheErrorCode00
  | SchemaCacheErrorCode01
  | SchemaCacheErrorCode02
  | SchemaCacheErrorCode03
  | SchemaCacheErrorCode04
  | SchemaCacheErrorCode05
  -- JWT authentication errors
  | JWTErrorCode00
  | JWTErrorCode01
  | JWTErrorCode02
  -- Internal errors related to the Hasql library
  | InternalErrorCode00

instance JSON.ToJSON ErrorCode where
  toJSON e = JSON.toJSON (buildErrorCode e)

-- New group of errors will be added at the end of all the groups and will have the next prefix in the sequence
-- New errors are added at the end of the group they belong to and will have the next code in the sequence
buildErrorCode :: ErrorCode -> Text
buildErrorCode code = case code of
  -- Keep the "PGRST" prefix in every code for an easier search/grep
  ConnectionErrorCode00  -> "PGRST000"
  ConnectionErrorCode01  -> "PGRST001"
  ConnectionErrorCode02  -> "PGRST002"
  ConnectionErrorCode03  -> "PGRST003"

  ApiRequestErrorCode00  -> "PGRST100"
  ApiRequestErrorCode01  -> "PGRST101"
  ApiRequestErrorCode02  -> "PGRST102"
  ApiRequestErrorCode03  -> "PGRST103"
  ApiRequestErrorCode05  -> "PGRST105"
  ApiRequestErrorCode06  -> "PGRST106"
  ApiRequestErrorCode07  -> "PGRST107"
  ApiRequestErrorCode08  -> "PGRST108"
  ApiRequestErrorCode11  -> "PGRST111"
  ApiRequestErrorCode12  -> "PGRST112"
  ApiRequestErrorCode14  -> "PGRST114"
  ApiRequestErrorCode15  -> "PGRST115"
  ApiRequestErrorCode16  -> "PGRST116"
  ApiRequestErrorCode17  -> "PGRST117"
  ApiRequestErrorCode18  -> "PGRST118"
  ApiRequestErrorCode19  -> "PGRST119"
  ApiRequestErrorCode20  -> "PGRST120"
  ApiRequestErrorCode21  -> "PGRST121"
  ApiRequestErrorCode22  -> "PGRST122"
  ApiRequestErrorCode23  -> "PGRST123"
  ApiRequestErrorCode24  -> "PGRST124"

  SchemaCacheErrorCode00 -> "PGRST200"
  SchemaCacheErrorCode01 -> "PGRST201"
  SchemaCacheErrorCode02 -> "PGRST202"
  SchemaCacheErrorCode03 -> "PGRST203"
  SchemaCacheErrorCode04 -> "PGRST204"
  SchemaCacheErrorCode05 -> "PGRST205"

  JWTErrorCode00         -> "PGRST300"
  JWTErrorCode01         -> "PGRST301"
  JWTErrorCode02         -> "PGRST302"

  InternalErrorCode00    -> "PGRSTX00"
