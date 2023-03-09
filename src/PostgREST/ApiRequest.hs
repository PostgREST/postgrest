{-|
Module      : PostgREST.Request.ApiRequest
Description : PostgREST functions to translate HTTP request to a domain type called ApiRequest.
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.ApiRequest
  ( ApiRequest(..)
  , InvokeMethod(..)
  , Mutation(..)
  , MediaType(..)
  , Action(..)
  , Target(..)
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
import qualified Data.List             as L
import qualified Data.List.NonEmpty    as NonEmptyList
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import qualified Data.Text.Encoding    as T
import qualified Data.Vector           as V

import Data.Either.Combinators (mapBoth)

import Control.Arrow             ((***))
import Data.Aeson.Types          (emptyArray, emptyObject)
import Data.List                 (lookup, union)
import Data.Ranged.Ranges        (emptyRange, rangeIntersection,
                                  rangeIsEmpty)
import Network.HTTP.Types.Header (RequestHeaders, hCookie)
import Network.HTTP.Types.URI    (parseSimpleQuery)
import Network.Wai               (Request (..))
import Network.Wai.Parse         (parseHttpAccept)
import Web.Cookie                (parseCookies)

import PostgREST.ApiRequest.QueryParams  (QueryParams (..))
import PostgREST.ApiRequest.Types        (ApiRequestError (..),
                                          RangeError (..))
import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.MediaType               (MTPlanAttrs (..),
                                          MTPlanFormat (..),
                                          MediaType (..))
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          convertToLimitZeroRange,
                                          hasLimitZero,
                                          rangeRequested)
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

data InvokeMethod = InvHead | InvGet | InvPost deriving Eq
data Mutation = MutationCreate | MutationDelete | MutationSingleUpsert | MutationUpdate deriving Eq

-- | Types of things a user wants to do to tables/views/procs
data Action
  = ActionMutate Mutation
  | ActionRead {isHead :: Bool}
  | ActionInvoke InvokeMethod
  | ActionInfo
  | ActionInspect {isHead :: Bool}
  deriving Eq
-- | The path info that will be mapped to a target (used to handle validations and errors before defining the Target)
data PathInfo
  = PathInfo
      { pathName       :: Text
      , pathIsProc     :: Bool
      , pathIsDefSpec  :: Bool
      , pathIsRootSpec :: Bool
      }
-- | The target db object of a user action
data Target = TargetIdent QualifiedIdentifier
            | TargetProc{tProc :: QualifiedIdentifier, tpIsRootSpec :: Bool}
            | TargetDefaultSpec{tdsSchema :: Schema} -- The default spec offered at root "/"

{-|
  Describes what the user wants to do. This data type is a
  translation of the raw elements of an HTTP request into domain
  specific language.  There is no guarantee that the intent is
  sensible, it is up to a later stage of processing to determine
  if it is an action we are able to perform.
-}
data ApiRequest = ApiRequest {
    iAction              :: Action                           -- ^ Similar but not identical to HTTP method, e.g. Create/Invoke both POST
  , iRange               :: HM.HashMap Text NonnegRange      -- ^ Requested range of rows within response
  , iTopLevelRange       :: NonnegRange                      -- ^ Requested range of rows from the top level
  , iTarget              :: Target                           -- ^ The target, be it calling a proc or accessing a table
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
  , iAcceptMediaType     :: MediaType                        -- ^ The media type in the Accept header
  , iContentMediaType    :: MediaType                        -- ^ The media type in the Content-Type header
  }

-- | Examines HTTP request and translates it into user intent.
userApiRequest :: AppConfig -> Request -> RequestBody -> Either ApiRequestError ApiRequest
userApiRequest conf req reqBody = do
  pInfo@PathInfo{..} <- getPathInfo conf $ pathInfo req
  act <- getAction pInfo method
  qPrms <- first QueryParamError $ QueryParams.parse (pathIsProc && act `elem` [ActionInvoke InvGet, ActionInvoke InvHead]) $ rawQueryString req
  (acceptMediaType, contentMediaType) <- getMediaTypes conf hdrs act pInfo
  (schema, negotiatedByProfile) <- getSchema conf hdrs method
  (topLevelRange, ranges) <- getRanges method qPrms hdrs
  (payload, columns) <- getPayload reqBody contentMediaType qPrms act pInfo
  return $ ApiRequest {
    iAction = act
  , iTarget = if | pathIsProc    -> TargetProc (QualifiedIdentifier schema pathName) pathIsRootSpec
                 | pathIsDefSpec -> TargetDefaultSpec schema
                 | otherwise     -> TargetIdent $ QualifiedIdentifier schema pathName
  , iRange = ranges
  , iTopLevelRange = topLevelRange
  , iPayload = payload
  , iPreferences = Preferences.fromHeaders hdrs
  , iQueryParams = qPrms
  , iColumns = columns
  , iHeaders = iHdrs
  , iCookies = iCkies
  , iPath = rawPathInfo req
  , iMethod = method
  , iSchema = schema
  , iNegotiatedByProfile = negotiatedByProfile
  , iAcceptMediaType = acceptMediaType
  , iContentMediaType = contentMediaType
  }
  where
    method = requestMethod req
    hdrs = requestHeaders req
    lookupHeader    = flip lookup hdrs
    iHdrs = [ (CI.foldedCase k, v) | (k,v) <- hdrs, k /= hCookie]
    iCkies = maybe [] parseCookies $ lookupHeader "Cookie"

getPathInfo :: AppConfig -> [Text] -> Either ApiRequestError PathInfo
getPathInfo AppConfig{configOpenApiMode, configDbRootSpec} path =
  case path of
    []             -> case configDbRootSpec of
                        Just (QualifiedIdentifier _ pathName)     -> Right $ PathInfo pathName True False True
                        Nothing | configOpenApiMode == OADisabled -> Left NotFound
                                | otherwise                       -> Right $ PathInfo mempty False True False
    [table]        -> Right $ PathInfo table False False False
    ["rpc", pName] -> Right $ PathInfo pName True False False
    _              -> Left NotFound

getAction :: PathInfo -> ByteString -> Either ApiRequestError Action
getAction PathInfo{pathIsProc, pathIsDefSpec} method =
  if pathIsProc && method `notElem` ["HEAD", "GET", "POST", "OPTIONS"]
    then Left $ InvalidRpcMethod method
    else case method of
      -- The HEAD method is identical to GET except that the server MUST NOT return a message-body in the response
      -- From https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.4
      "HEAD"     | pathIsDefSpec -> Right $ ActionInspect{isHead=True}
                 | pathIsProc    -> Right $ ActionInvoke InvHead
                 | otherwise     -> Right $ ActionRead{isHead=True}
      "GET"      | pathIsDefSpec -> Right $ ActionInspect{isHead=False}
                 | pathIsProc    -> Right $ ActionInvoke InvGet
                 | otherwise     -> Right $ ActionRead{isHead=False}
      "POST"     | pathIsProc    -> Right $ ActionInvoke InvPost
                 | otherwise     -> Right $ ActionMutate MutationCreate
      "PATCH"                    -> Right $ ActionMutate MutationUpdate
      "PUT"                      -> Right $ ActionMutate MutationSingleUpsert
      "DELETE"                   -> Right $ ActionMutate MutationDelete
      "OPTIONS"                  -> Right ActionInfo
      _                          -> Left $ UnsupportedMethod method

getMediaTypes :: AppConfig -> RequestHeaders -> Action -> PathInfo -> Either ApiRequestError (MediaType, MediaType)
getMediaTypes conf hdrs action path = do
   acceptMediaType <- findAcceptMediaType conf action path accepts
   pure (acceptMediaType, contentMediaType)
  where
    accepts = maybe [MTAny] (map MediaType.decodeMediaType . parseHttpAccept) $ lookupHeader "accept"
    contentMediaType = maybe MTApplicationJSON MediaType.decodeMediaType $ lookupHeader "content-type"
    lookupHeader    = flip lookup hdrs

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
getRanges method QueryParams{qsOrder,qsRanges} hdrs
  | isInvalidRange = Left $ InvalidRange (if rangeIsEmpty headerRange then LowerGTUpper else NegativeLimit)
  | method `elem` ["PATCH", "DELETE"] && not (null qsRanges) && null qsOrder = Left LimitNoOrderError
  | method == "PUT" && topLevelRange /= allRange = Left PutRangeNotAllowedError
  | otherwise = Right (topLevelRange, ranges)
  where
    headerRange = rangeRequested hdrs
    limitRange = fromMaybe allRange (HM.lookup "limit" qsRanges)
    headerAndLimitRange = rangeIntersection headerRange limitRange
    -- Bypass all the ranges and send only the limit zero range (0 <= x <= -1) if
    -- limit=0 is present in the query params (not allowed for the Range header)
    ranges = HM.insert "limit" (convertToLimitZeroRange limitRange headerAndLimitRange) qsRanges
    -- The only emptyRange allowed is the limit zero range
    isInvalidRange = topLevelRange == emptyRange && not (hasLimitZero limitRange)
    topLevelRange = fromMaybe allRange $ HM.lookup "limit" ranges -- if no limit is specified, get all the request rows

getPayload :: RequestBody -> MediaType -> QueryParams.QueryParams -> Action -> PathInfo -> Either ApiRequestError (Maybe Payload, S.Set FieldName)
getPayload reqBody contentMediaType QueryParams{qsColumns} action PathInfo{pathIsProc}= do
  checkedPayload <- if shouldParsePayload then payload else Right Nothing
  let cols = case (checkedPayload, columns) of
        (Just ProcessedJSON{payKeys}, _)       -> payKeys
        (Just ProcessedUrlEncoded{payKeys}, _) -> payKeys
        (Just RawJSON{}, Just cls)             -> cls
        _                                      -> S.empty
  return (checkedPayload, cols)
  where
    payload :: Either ApiRequestError (Maybe Payload)
    payload = mapBoth InvalidBody Just $ case (contentMediaType, pathIsProc) of
      (MTApplicationJSON, _) ->
        if isJust columns
          then Right $ RawJSON reqBody
          else note "All object keys must match" . payloadAttributes reqBody
                 =<< if LBS.null reqBody && pathIsProc
                       then Right emptyObject
                       else first BS.pack $ JSON.eitherDecode reqBody
      (MTTextCSV, _) -> do
        json <- csvToJson <$> first BS.pack (CSV.decodeByName reqBody)
        note "All lines must have same number of fields" $ payloadAttributes (JSON.encode json) json
      (MTUrlEncoded, isProc) -> do
        let params = (T.decodeUtf8 *** T.decodeUtf8) <$> parseSimpleQuery (LBS.toStrict reqBody)
        if isProc
          then Right $ ProcessedUrlEncoded params (S.fromList $ fst <$> params)
          else
            let paramsMap = HM.fromList $ (identity *** JSON.String) <$> params in
            Right $ ProcessedJSON (JSON.encode paramsMap) $ S.fromList (HM.keys paramsMap)
      (MTTextPlain, True) -> Right $ RawPay reqBody
      (MTTextXML, True) -> Right $ RawPay reqBody
      (MTOctetStream, True) -> Right $ RawPay reqBody
      (ct, _) -> Left $ "Content-Type not acceptable: " <> MediaType.toMime ct

    shouldParsePayload = case (action, contentMediaType) of
      (ActionMutate MutationCreate, _)       -> True
      (ActionInvoke InvPost, _)              -> True
      (ActionMutate MutationSingleUpsert, _) -> True
      (ActionMutate MutationUpdate, _)       -> True
      _                                      -> False

    columns = case action of
      ActionMutate MutationCreate -> qsColumns
      ActionMutate MutationUpdate -> qsColumns
      ActionInvoke InvPost        -> qsColumns
      _                           -> Nothing

{-|
  Find the best match from a list of media types accepted by the
  client in order of decreasing preference and a list of types
  producible by the server.  If there is no match but the client
  accepts */* then return the top server pick.
-}
mutuallyAgreeable :: [MediaType] -> [MediaType] -> Maybe MediaType
mutuallyAgreeable sProduces cAccepts =
  let exact = listToMaybe $ L.intersect cAccepts sProduces in
  if isNothing exact && MTAny `elem` cAccepts
     then listToMaybe sProduces
     else exact

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

findAcceptMediaType :: AppConfig -> Action -> PathInfo -> [MediaType] -> Either ApiRequestError MediaType
findAcceptMediaType conf action path accepts =
  case mutuallyAgreeable (requestMediaTypes conf action path) accepts of
    Just ct ->
      Right ct
    Nothing ->
      Left . MediaTypeError $ map MediaType.toMime accepts

requestMediaTypes :: AppConfig -> Action -> PathInfo -> [MediaType]
requestMediaTypes conf action path =
  case action of
    ActionRead _    -> defaultMediaTypes ++ rawMediaTypes
    ActionInvoke _  -> invokeMediaTypes
    ActionInspect _ -> [MTOpenAPI, MTApplicationJSON]
    ActionInfo      -> [MTTextCSV]
    _               -> defaultMediaTypes
  where
    invokeMediaTypes =
      defaultMediaTypes
        ++ rawMediaTypes
        ++ [MTOpenAPI | pathIsRootSpec path]
    defaultMediaTypes =
      [MTApplicationJSON, MTSingularJSON, MTGeoJSON, MTTextCSV] ++
      [MTPlan $ MTPlanAttrs Nothing PlanJSON mempty | configDbPlanEnabled conf]
    rawMediaTypes = configRawMediaTypes conf `union` [MTOctetStream, MTTextPlain, MTTextXML]
