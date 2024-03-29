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
  , status
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.CaseInsensitive      as CI
import qualified Data.FuzzySet.Simple      as Fuzzy
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

import           PostgREST.ApiRequest.Types (ApiRequestError (..),
                                             QPError (..),
                                             RangeError (..))
import           PostgREST.MediaType        (MediaType (..))
import qualified PostgREST.MediaType        as MediaType

import PostgREST.SchemaCache.Identifiers  (QualifiedIdentifier (..),
                                           Schema)
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..),
                                           RelationshipsMap)
import PostgREST.SchemaCache.Routine      (Routine (..),
                                           RoutineParam (..))
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
  status LimitNoOrderError           = HTTP.status400
  status ColumnNotFound{}            = HTTP.status400
  status GucHeadersError             = HTTP.status500
  status GucStatusError              = HTTP.status500
  status OffLimitsChangesError{}     = HTTP.status400
  status PutMatchingPkError          = HTTP.status400
  status SingularityError{}          = HTTP.status406
  status PGRSTParseError             = HTTP.status500
  status MaxAffectedViolationError{} = HTTP.status400

  headers SingularityError{}     = [MediaType.toContentType $ MTVndSingularJSON False]
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

  toJSON LimitNoOrderError = toJsonPgrstError
    ApiRequestErrorCode09 "A 'limit' was applied without an explicit 'order'" Nothing (Just "Apply an 'order' using unique column(s)")

  toJSON (OffLimitsChangesError n maxs) = toJsonPgrstError
    ApiRequestErrorCode10
    "The maximum number of rows allowed to change was surpassed"
    (Just $ JSON.String $ T.unwords ["Results contain", show n, "rows changed but the maximum number allowed is", show maxs])
    Nothing

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
    "JSON object requested, multiple (or no) rows returned"
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

  toJSON PGRSTParseError = toJsonPgrstError
    ApiRequestErrorCode21 "The message and detail field of RAISE 'PGRST' error expects JSON" Nothing Nothing

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

  toJSON (NoRpc schema procName argumentKeys hasPreferSingleObject contentType isInvPost allProcs overloadedProcs)  =
    let func = schema <> "." <> procName
        prms = T.intercalate ", " argumentKeys
        prmsMsg = "(" <> prms <> ")"
        prmsDet = " with parameter" <> (if length argumentKeys > 1 then "s " else " ") <> prms
        fmtPrms p = if null argumentKeys then " without parameters" else p
        onlySingleParams = hasPreferSingleObject || (isInvPost && contentType `elem` [MTTextPlain, MTTextXML, MTOctetStream])
    in toJsonPgrstError
    SchemaCacheErrorCode02
    ("Could not find the function " <> func <> (if onlySingleParams then "" else fmtPrms prmsMsg) <> " in the schema cache")
    (Just $ JSON.String $ "Searched for the function " <> func <>
      (case (hasPreferSingleObject, isInvPost, contentType) of
        (True, _, _)                 -> " with a single json/jsonb parameter"
        (_, True, MTTextPlain)       -> " with a single unnamed text parameter"
        (_, True, MTTextXML)         -> " with a single unnamed xml parameter"
        (_, True, MTOctetStream)     -> " with a single unnamed bytea parameter"
        (_, True, MTApplicationJSON) -> fmtPrms prmsDet <> " or with a single unnamed json/jsonb parameter"
        _                            -> fmtPrms prmsDet) <>
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
    suggestParent = snd <$> Fuzzy.findOne parent fuzzySetOfParents
    -- Do not give suggestion if the child is found in the relations (weight = 1.0)
    suggestChild  = headMay [snd k | k <- Fuzzy.find child fuzzySetOfChildren, fst k < 1.0]

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
      | null overloadedProcs = snd <$> Fuzzy.findOne procName fuzzySetOfProcs
      | otherwise            = (procName <>) . snd <$> Fuzzy.findOne (listToText params) fuzzySetOfParams

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
        O2O cons relColumns -> [
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
        O2O cons _       -> buildHint cons
        O2M cons _       -> buildHint cons
    -- An ambiguousness error cannot happen for computed relationships TODO refactor so this mempty is not needed
    hintList ComputedRelationship{} = mempty

data PgError = PgError Authenticated SQL.UsageError
type Authenticated = Bool

instance PgrstError PgError where
  status (PgError authed usageError) = pgErrorStatus authed usageError

  headers (PgError _ (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError (SQL.ServerError "PGRST" m d _ _p))))) =
    case (parseMessage m, parseDetails d) of
      (Just _, Just r) -> headers PGRSTParseError ++ map intoHeader (M.toList $ getHeaders r)
      _                -> headers PGRSTParseError
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
    case (parseMessage m, parseDetails d) of
      (Just r, Just _) -> JSON.object [
        "code"     .= getCode r,
        "message"  .= getMessage r,
        "details"  .= checkMaybe (getDetails r),
        "hint"     .= checkMaybe (getHint r)]
      _ -> JSON.toJSON PGRSTParseError
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
        '5':'3':_ -> HTTP.status503 -- insufficient resources
        '5':'4':_ -> HTTP.status413 -- too complex
        '5':'5':_ -> HTTP.status500 -- obj not on prereq state
        '5':'7':'P':'0':'1':_ -> HTTP.status503 -- terminating connection due to administrator command
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
        "42501"   -> if authed then HTTP.status403 else HTTP.status401 -- insufficient privilege
        'P':'T':n -> fromMaybe HTTP.status500 (HTTP.mkStatus <$> readMaybe n <*> pure m)
        "PGRST"   ->
          case (parseMessage m, parseDetails d) of
            (Just _, Just r) -> maybe (toEnum $ getStatus r) (HTTP.mkStatus (getStatus r) . T.encodeUtf8) (getStatusText r)
            _                -> status PGRSTParseError
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

parseMessage :: ByteString -> Maybe PgRaiseErrMessage
parseMessage = JSON.decodeStrict

parseDetails :: Maybe ByteString -> Maybe PgRaiseErrDetails
parseDetails d = JSON.decodeStrict =<< d

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
  | ApiRequestErrorCode09
  | ApiRequestErrorCode10
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
buildErrorCode code = "PGRST" <> case code of
  ConnectionErrorCode00  -> "000"
  ConnectionErrorCode01  -> "001"
  ConnectionErrorCode02  -> "002"
  ConnectionErrorCode03  -> "003"

  ApiRequestErrorCode00  -> "100"
  ApiRequestErrorCode01  -> "101"
  ApiRequestErrorCode02  -> "102"
  ApiRequestErrorCode03  -> "103"
  ApiRequestErrorCode05  -> "105"
  ApiRequestErrorCode06  -> "106"
  ApiRequestErrorCode07  -> "107"
  ApiRequestErrorCode08  -> "108"
  ApiRequestErrorCode09  -> "109"
  ApiRequestErrorCode10  -> "110"
  ApiRequestErrorCode11  -> "111"
  ApiRequestErrorCode12  -> "112"
  ApiRequestErrorCode14  -> "114"
  ApiRequestErrorCode15  -> "115"
  ApiRequestErrorCode16  -> "116"
  ApiRequestErrorCode17  -> "117"
  ApiRequestErrorCode18  -> "118"
  ApiRequestErrorCode19  -> "119"
  ApiRequestErrorCode20  -> "120"
  ApiRequestErrorCode21  -> "121"
  ApiRequestErrorCode22  -> "122"
  ApiRequestErrorCode23  -> "123"
  ApiRequestErrorCode24  -> "124"

  SchemaCacheErrorCode00 -> "200"
  SchemaCacheErrorCode01 -> "201"
  SchemaCacheErrorCode02 -> "202"
  SchemaCacheErrorCode03 -> "203"
  SchemaCacheErrorCode04 -> "204"

  JWTErrorCode00         -> "300"
  JWTErrorCode01         -> "301"
  JWTErrorCode02         -> "302"

  InternalErrorCode00    -> "X00"
