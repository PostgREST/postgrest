{-|
Module      : PostgREST.Request.ApiRequest
Description : PostgREST functions to translate HTTP request to a domain type called ApiRequest.
-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
-- TODO: This module shouldn't depend on SchemaCache
module PostgREST.ApiRequest
  ( ApiRequest(..)
  , InvokeMethod(..)
  , Mutation(..)
  , MediaType(..)
  , Action(..)
  , DbAction(..)
  , Payload(..)
  , userApiRequest
  ) where

import qualified Data.Aeson            as JSON
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.CaseInsensitive  as CI
import qualified Data.Csv              as CSV
import qualified Data.HashMap.Strict   as HM
import qualified Data.List.NonEmpty    as NonEmptyList
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import qualified Data.Text.Encoding    as T
import qualified Data.Vector           as V

import Data.Either.Combinators (mapBoth)

import Control.Arrow             ((***))
import Data.Aeson.Types          (emptyArray, emptyObject)
import Data.List                 (lookup)
import Data.Ranged.Ranges        (emptyRange, rangeIntersection,
                                  rangeIsEmpty)
import Network.HTTP.Types.Header (RequestHeaders, hCookie)
import Network.HTTP.Types.URI    (parseSimpleQuery)
import Network.Wai               (Request (..))
import Network.Wai.Parse         (parseHttpAccept)
import Web.Cookie                (parseCookies)

import PostgREST.ApiRequest.QueryParams  (QueryParams (..))
import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.Error                   (ApiRequestError (..),
                                          RangeError (..))
import PostgREST.MediaType               (MediaType (..))
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          convertToLimitZeroRange,
                                          hasLimitZero,
                                          rangeRequested)
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)

import qualified PostgREST.ApiRequest.Preferences as Preferences
import qualified PostgREST.ApiRequest.QueryParams as QueryParams
import qualified PostgREST.MediaType              as MediaType

import Protolude


type RequestBody = LBS.ByteString

data Payload
  = ProcessedJSON -- ^ Cached attributes of a JSON payload
      { payRaw  :: LBS.ByteString
      -- ^ This is the raw ByteString that comes from the request body.  We
      -- cache this instead of an Aeson Value because it was detected that for
      -- large payloads the encoding had high memory usage, see
      -- https://github.com/PostgREST/postgrest/pull/1005 for more details
      , payKeys :: S.Set Text
      -- ^ Keys of the object or if it's an array these keys are guaranteed to
      -- be the same across all its objects
      }
  | ProcessedUrlEncoded { payArray  :: [(Text, Text)], payKeys :: S.Set Text }
  | RawJSON { payRaw  :: LBS.ByteString }
  | RawPay  { payRaw  :: LBS.ByteString }

data InvokeMethod = Inv | InvRead Bool  deriving Eq
data Mutation = MutationCreate | MutationDelete | MutationSingleUpsert | MutationUpdate deriving Eq

data Resource
  = ResourceRelation Text
  | ResourceRoutine Text
  | ResourceSchema

data DbAction
  = ActRelationRead {dbActQi :: QualifiedIdentifier, actHeadersOnly :: Bool}
  | ActRelationMut  {dbActQi :: QualifiedIdentifier, actMutation :: Mutation}
  | ActRoutine      {dbActQi :: QualifiedIdentifier, actInvMethod :: InvokeMethod}
  | ActSchemaRead   Schema Bool

data Action
  = ActDb           DbAction
  | ActRelationInfo QualifiedIdentifier
  | ActRoutineInfo  QualifiedIdentifier InvokeMethod
  | ActSchemaInfo

{-|
  Describes what the user wants to do. This data type is a
  translation of the raw elements of an HTTP request into domain
  specific language.  There is no guarantee that the intent is
  sensible, it is up to a later stage of processing to determine
  if it is an action we are able to perform.
-}
data ApiRequest = ApiRequest {
    iAction              :: Action                           -- ^ Action on the resource
  , iRange               :: HM.HashMap Text NonnegRange      -- ^ Requested range of rows within response
  , iTopLevelRange       :: NonnegRange                      -- ^ Requested range of rows from the top level
  , iPayload             :: Maybe Payload                    -- ^ Data sent by client and used for mutation actions
  , iPreferences         :: Preferences.Preferences          -- ^ Prefer header values
  , iQueryParams         :: QueryParams.QueryParams
  , iColumns             :: S.Set FieldName                  -- ^ parsed colums from &columns parameter and payload
  , iHeaders             :: [(ByteString, ByteString)]       -- ^ HTTP request headers
  , iCookies             :: [(ByteString, ByteString)]       -- ^ Request Cookies
  , iPath                :: ByteString                       -- ^ Raw request path
  , iMethod              :: ByteString                       -- ^ Raw request method
  , iSchema              :: Schema                           -- ^ The request schema. Can vary depending on profile headers.
  , iNegotiatedByProfile :: Bool                             -- ^ If schema was was chosen according to the profile spec https://www.w3.org/TR/dx-prof-conneg/
  , iAcceptMediaType     :: [MediaType]                      -- ^ The resolved media types in the Accept, considering quality(q) factors
  , iContentMediaType    :: MediaType                        -- ^ The media type in the Content-Type header
  }

-- | Examines HTTP request and translates it into user intent.
userApiRequest :: AppConfig -> Request -> RequestBody -> SchemaCache -> Either ApiRequestError ApiRequest
userApiRequest conf req reqBody sCache = do
  resource <- getResource conf $ pathInfo req
  (schema, negotiatedByProfile) <- getSchema conf hdrs method
  act <- getAction resource schema method
  qPrms <- first QueryParamError $ QueryParams.parse (actIsInvokeSafe act) $ rawQueryString req
  (topLevelRange, ranges) <- getRanges method qPrms hdrs
  (payload, columns) <- getPayload reqBody contentMediaType qPrms act
  return $ ApiRequest {
    iAction = act
  , iRange = ranges
  , iTopLevelRange = topLevelRange
  , iPayload = payload
  , iPreferences = Preferences.fromHeaders (configDbTxAllowOverride conf) (dbTimezones sCache) hdrs
  , iQueryParams = qPrms
  , iColumns = columns
  , iHeaders = iHdrs
  , iCookies = iCkies
  , iPath = rawPathInfo req
  , iMethod = method
  , iSchema = schema
  , iNegotiatedByProfile = negotiatedByProfile
  , iAcceptMediaType = maybe [MTAny] (map MediaType.decodeMediaType . parseHttpAccept) $ lookupHeader "accept"
  , iContentMediaType = contentMediaType
  }
  where
    method = requestMethod req
    hdrs = requestHeaders req
    lookupHeader    = flip lookup hdrs
    iHdrs = [ (CI.foldedCase k, v) | (k,v) <- hdrs, k /= hCookie]
    iCkies = maybe [] parseCookies $ lookupHeader "Cookie"
    contentMediaType = maybe MTApplicationJSON MediaType.decodeMediaType $ lookupHeader "content-type"
    actIsInvokeSafe x = case x of {ActDb (ActRoutine _  (InvRead _)) -> True; _ -> False}

getResource :: AppConfig -> [Text] -> Either ApiRequestError Resource
getResource AppConfig{configOpenApiMode, configDbRootSpec} = \case
  []             -> case configDbRootSpec of
                      Just (QualifiedIdentifier _ pathName)     -> Right $ ResourceRoutine pathName
                      Nothing | configOpenApiMode == OADisabled -> Left NotFound
                              | otherwise                       -> Right ResourceSchema
  [table]        -> Right $ ResourceRelation table
  ["rpc", pName] -> Right $ ResourceRoutine pName
  _              -> Left NotFound

getAction :: Resource -> Schema -> ByteString -> Either ApiRequestError Action
getAction resource schema method =
  case (resource, method) of
    (ResourceRoutine rout, "HEAD")    -> Right . ActDb $ ActRoutine (qi rout) $ InvRead True
    (ResourceRoutine rout, "GET")     -> Right . ActDb $ ActRoutine (qi rout) $ InvRead False
    (ResourceRoutine rout, "POST")    -> Right . ActDb $ ActRoutine (qi rout) Inv
    (ResourceRoutine rout, "OPTIONS") -> Right $ ActRoutineInfo (qi rout) $ InvRead True
    (ResourceRoutine _, _)            -> Left $ InvalidRpcMethod method

    (ResourceRelation rel, "HEAD")    -> Right . ActDb $ ActRelationRead (qi rel) True
    (ResourceRelation rel, "GET")     -> Right . ActDb $ ActRelationRead (qi rel) False
    (ResourceRelation rel, "POST")    -> Right . ActDb $ ActRelationMut  (qi rel) MutationCreate
    (ResourceRelation rel, "PUT")     -> Right . ActDb $ ActRelationMut  (qi rel) MutationSingleUpsert
    (ResourceRelation rel, "PATCH")   -> Right . ActDb $ ActRelationMut  (qi rel) MutationUpdate
    (ResourceRelation rel, "DELETE")  -> Right . ActDb $ ActRelationMut  (qi rel) MutationDelete
    (ResourceRelation rel, "OPTIONS") -> Right $ ActRelationInfo (qi rel)

    (ResourceSchema, "HEAD")          -> Right . ActDb $ ActSchemaRead schema True
    (ResourceSchema, "GET")           -> Right . ActDb $ ActSchemaRead schema False
    (ResourceSchema, "OPTIONS")       -> Right ActSchemaInfo

    _                                 -> Left $ UnsupportedMethod method
  where
    qi = QualifiedIdentifier schema


getSchema :: AppConfig -> RequestHeaders -> ByteString -> Either ApiRequestError (Schema, Bool)
getSchema AppConfig{configDbSchemas} hdrs method = do
  case profile of
    Just p | p `notElem` configDbSchemas -> Left $ UnacceptableSchema $ toList configDbSchemas
           | otherwise                   -> Right (p, True)
    Nothing -> Right (defaultSchema, length configDbSchemas /= 1) -- if we have many schemas, assume the default schema was negotiated
  where
    defaultSchema = NonEmptyList.head configDbSchemas
    profile = case method of
      -- POST/PATCH/PUT/DELETE don't use the same header as per the spec
      "DELETE" -> contentProfile
      "PATCH"  -> contentProfile
      "POST"   -> contentProfile
      "PUT"    -> contentProfile
      _        -> acceptProfile
    contentProfile = T.decodeUtf8 <$> lookupHeader "Content-Profile"
    acceptProfile = T.decodeUtf8 <$> lookupHeader "Accept-Profile"
    lookupHeader    = flip lookup hdrs

getRanges :: ByteString -> QueryParams -> RequestHeaders -> Either ApiRequestError (NonnegRange, HM.HashMap Text NonnegRange)
getRanges method QueryParams{qsRanges} hdrs
  | isInvalidRange = Left $ InvalidRange (if rangeIsEmpty headerRange then LowerGTUpper else NegativeLimit)
  | method == "PUT" && topLevelRange /= allRange = Left PutLimitNotAllowedError
  | otherwise = Right (topLevelRange, ranges)
  where
    -- According to the RFC (https://www.rfc-editor.org/rfc/rfc9110.html#name-range),
    -- the Range header must be ignored for all methods other than GET
    headerRange = if method == "GET" then rangeRequested hdrs else allRange
    limitRange = fromMaybe allRange (HM.lookup "limit" qsRanges)
    headerAndLimitRange = rangeIntersection headerRange limitRange
    -- Bypass all the ranges and send only the limit zero range (0 <= x <= -1) if
    -- limit=0 is present in the query params (not allowed for the Range header)
    ranges = HM.insert "limit" (convertToLimitZeroRange limitRange headerAndLimitRange) qsRanges
    -- The only emptyRange allowed is the limit zero range
    isInvalidRange = topLevelRange == emptyRange && not (hasLimitZero limitRange)
    topLevelRange = fromMaybe allRange $ HM.lookup "limit" ranges -- if no limit is specified, get all the request rows

getPayload :: RequestBody -> MediaType -> QueryParams.QueryParams -> Action -> Either ApiRequestError (Maybe Payload, S.Set FieldName)
getPayload reqBody contentMediaType QueryParams{qsColumns} action = do
  checkedPayload <- if shouldParsePayload then payload else Right Nothing
  let cols = case (checkedPayload, columns) of
        (Just ProcessedJSON{payKeys}, _)       -> payKeys
        (Just ProcessedUrlEncoded{payKeys}, _) -> payKeys
        (Just RawJSON{}, Just cls)             -> cls
        _                                      -> S.empty
  return (checkedPayload, cols)
  where
    payload :: Either ApiRequestError (Maybe Payload)
    payload = mapBoth InvalidBody Just $ case (contentMediaType, isProc) of
      (MTApplicationJSON, _) ->
        if isJust columns
          then Right $ RawJSON reqBody
          else note "All object keys must match" . payloadAttributes reqBody
                 =<< if LBS.null reqBody && isProc
                       then Right emptyObject
                       else first BS.pack $
                          -- Drop parsing error message in favor of generic one (https://github.com/PostgREST/postgrest/issues/2344)
                          maybe (Left "Empty or invalid json") Right $ JSON.decode reqBody
      (MTTextCSV, _) -> do
        json <- csvToJson <$> first BS.pack (CSV.decodeByName reqBody)
        note "All lines must have same number of fields" $ payloadAttributes (JSON.encode json) json
      (MTUrlEncoded, True) ->
        Right $ ProcessedUrlEncoded params (S.fromList $ fst <$> params)
      (MTUrlEncoded, False) ->
        let paramsMap = HM.fromList $ (identity *** JSON.String) <$> params in
        Right $ ProcessedJSON (JSON.encode paramsMap) $ S.fromList (HM.keys paramsMap)
      (MTTextPlain, True) -> Right $ RawPay reqBody
      (MTTextXML, True) -> Right $ RawPay reqBody
      (MTOctetStream, True) -> Right $ RawPay reqBody
      (ct, _) -> Left $ "Content-Type not acceptable: " <> MediaType.toMime ct

    shouldParsePayload = case action of
      ActDb (ActRelationMut _ MutationDelete) -> False
      ActDb (ActRelationMut _ _)              -> True
      ActDb (ActRoutine _  Inv)               -> True
      _                                       -> False

    columns = case action of
      ActDb (ActRelationMut _ MutationCreate) -> qsColumns
      ActDb (ActRelationMut _ MutationUpdate) -> qsColumns
      ActDb (ActRoutine     _ Inv)            -> qsColumns
      _                                       -> Nothing

    isProc = case action of
      ActDb (ActRoutine _ _) -> True
      _                      -> False
    params = (T.decodeUtf8 *** T.decodeUtf8) <$> parseSimpleQuery (LBS.toStrict reqBody)

type CsvData = V.Vector (M.Map Text LBS.ByteString)

{-|
  Converts CSV like
  a,b
  1,hi
  2,bye

  into a JSON array like
  [ {"a": "1", "b": "hi"}, {"a": 2, "b": "bye"} ]

  The reason for its odd signature is so that it can compose
  directly with CSV.decodeByName
-}
csvToJson :: (CSV.Header, CsvData) -> JSON.Value
csvToJson (_, vals) =
  JSON.Array $ V.map rowToJsonObj vals
 where
  rowToJsonObj = JSON.Object . KM.fromMapText .
    M.map (\str ->
        if str == "NULL"
          then JSON.Null
          else JSON.String . T.decodeUtf8 $ LBS.toStrict str
      )

payloadAttributes :: RequestBody -> JSON.Value -> Maybe Payload
payloadAttributes raw json =
  -- Test that Array contains only Objects having the same keys
  case json of
    JSON.Array arr ->
      case arr V.!? 0 of
        Just (JSON.Object o) ->
          let canonicalKeys = S.fromList $ K.toText <$> KM.keys o
              areKeysUniform = all (\case
                JSON.Object x -> S.fromList (K.toText <$> KM.keys x) == canonicalKeys
                _ -> False) arr in
          if areKeysUniform
            then Just $ ProcessedJSON raw canonicalKeys
            else Nothing
        Just _ -> Nothing
        Nothing -> Just emptyPJArray

    JSON.Object o -> Just $ ProcessedJSON raw (S.fromList $ K.toText <$> KM.keys o)

    -- truncate everything else to an empty array.
    _ -> Just emptyPJArray
  where
    emptyPJArray = ProcessedJSON (JSON.encode emptyArray) S.empty
