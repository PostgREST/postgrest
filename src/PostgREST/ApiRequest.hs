{-|
Module      : PostgREST.Request.ApiRequest
Description : PostgREST functions to translate HTTP request to a domain type called ApiRequest.
-}
{-# LANGUAGE LambdaCase      #-}
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

import Control.Arrow             ((***))
import Data.Aeson.Types          (emptyArray, emptyObject)
import Data.List                 (lookup, union)
import Data.Ranged.Ranges        (emptyRange, rangeIntersection,
                                  rangeIsEmpty)
import Data.Tree                 (Tree (..))
import Network.HTTP.Types.Header (RequestHeaders, hCookie)
import Network.HTTP.Types.URI    (parseSimpleQuery)
import Network.Wai               (Request (..))
import Network.Wai.Parse         (parseHttpAccept)
import Web.Cookie                (parseCookies)

import PostgREST.ApiRequest.Preferences  (PreferCount (..),
                                          PreferParameters (..),
                                          PreferRepresentation (..),
                                          PreferResolution (..),
                                          PreferTransaction (..))
import PostgREST.ApiRequest.QueryParams  (QueryParams (..))
import PostgREST.ApiRequest.Types        (ApiRequestError (..),
                                          RangeError (..), SelectItem)
import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.MediaType               (MTPlanAttrs (..),
                                          MTPlanFormat (..),
                                          MediaType (..))
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          hasLimitZero,
                                          limitZeroRange,
                                          rangeRequested)
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)
import PostgREST.SchemaCache.Proc        (ProcDescription (..),
                                          ProcParam (..), ProcsMap,
                                          procReturnsScalar)

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
            | TargetProc{tProc :: ProcDescription, tpIsRootSpec :: Bool}
            | TargetDefaultSpec{tdsSchema :: Schema} -- The default spec offered at root "/"

-- | RPC query param value `/rpc/func?v=<value>`, used for VARIADIC functions on form-urlencoded POST and GETs
-- | It can be fixed `?v=1` or repeated `?v=1&v=2&v=3.
data RpcParamValue = Fixed Text | Variadic [Text]
instance JSON.ToJSON RpcParamValue where
  toJSON (Fixed    v) = JSON.toJSON v
  toJSON (Variadic v) = JSON.toJSON v

toRpcParamValue :: ProcDescription -> (Text, Text) -> (Text, RpcParamValue)
toRpcParamValue proc (k, v) | prmIsVariadic k = (k, Variadic [v])
                            | otherwise       = (k, Fixed v)
  where
    prmIsVariadic prm = isJust $ find (\ProcParam{ppName, ppVar} -> ppName == prm && ppVar) $ pdParams proc

-- | Convert rpc params `/rpc/func?a=val1&b=val2` to json `{"a": "val1", "b": "val2"}
jsonRpcParams :: ProcDescription -> [(Text, Text)] -> Payload
jsonRpcParams proc prms =
  if not $ pdHasVariadic proc then -- if proc has no variadic param, save steps and directly convert to json
    ProcessedJSON (JSON.encode $ HM.fromList $ second JSON.toJSON <$> prms) (S.fromList $ fst <$> prms)
  else
    let paramsMap = HM.fromListWith mergeParams $ toRpcParamValue proc <$> prms in
    ProcessedJSON (JSON.encode paramsMap) (S.fromList $ HM.keys paramsMap)
  where
    mergeParams :: RpcParamValue -> RpcParamValue -> RpcParamValue
    mergeParams (Variadic a) (Variadic b) = Variadic $ b ++ a
    mergeParams v _                       = v -- repeated params for non-variadic parameters are not merged

targetToJsonRpcParams :: Maybe Target -> [(Text, Text)] -> Maybe Payload
targetToJsonRpcParams target params =
  case target of
    Just TargetProc{tProc} -> Just $ jsonRpcParams tProc params
    _                      -> Nothing

{-|
  Describes what the user wants to do. This data type is a
  translation of the raw elements of an HTTP request into domain
  specific language.  There is no guarantee that the intent is
  sensible, it is up to a later stage of processing to determine
  if it is an action we are able to perform.
-}
data ApiRequest = ApiRequest {
    iAction               :: Action                           -- ^ Similar but not identical to HTTP method, e.g. Create/Invoke both POST
  , iRange                :: HM.HashMap Text NonnegRange      -- ^ Requested range of rows within response
  , iTopLevelRange        :: NonnegRange                      -- ^ Requested range of rows from the top level
  , iTarget               :: Target                           -- ^ The target, be it calling a proc or accessing a table
  , iPayload              :: Maybe Payload                    -- ^ Data sent by client and used for mutation actions
  , iPreferRepresentation :: PreferRepresentation             -- ^ If client wants created items echoed back
  , iPreferParameters     :: Maybe PreferParameters           -- ^ How to pass parameters to a stored procedure
  , iPreferCount          :: Maybe PreferCount                -- ^ Whether the client wants a result count
  , iPreferResolution     :: Maybe PreferResolution           -- ^ Whether the client wants to UPSERT or ignore records on PK conflict
  , iPreferTransaction    :: Maybe PreferTransaction          -- ^ Whether the clients wants to commit or rollback the transaction
  , iQueryParams          :: QueryParams.QueryParams
  , iColumns              :: S.Set FieldName                  -- ^ parsed colums from &columns parameter and payload
  , iHeaders              :: [(ByteString, ByteString)]       -- ^ HTTP request headers
  , iCookies              :: [(ByteString, ByteString)]       -- ^ Request Cookies
  , iPath                 :: ByteString                       -- ^ Raw request path
  , iMethod               :: ByteString                       -- ^ Raw request method
  , iSchema               :: Schema                           -- ^ The request schema. Can vary depending on profile headers.
  , iNegotiatedByProfile  :: Bool                             -- ^ If schema was was chosen according to the profile spec https://www.w3.org/TR/dx-prof-conneg/
  , iAcceptMediaType      :: MediaType                        -- ^ The media type in the Accept header
  , iBinaryField          :: Maybe FieldName                  -- ^ field used for raw output
  }

-- | Examines HTTP request and translates it into user intent.
userApiRequest :: AppConfig -> SchemaCache -> Request -> RequestBody -> Either ApiRequestError ApiRequest
userApiRequest conf sCache req reqBody = do
  qPrms <- first QueryParamError $ QueryParams.parse $ rawQueryString req
  pInfo <- getPathInfo conf $ pathInfo req
  act <- getAction pInfo $ requestMethod req
  mediaTypes <- getMediaTypes conf (requestHeaders req) act pInfo
  negotiatedSchema <- getSchema conf (requestHeaders req) (requestMethod req)
  apiRequest conf sCache req reqBody qPrms pInfo act mediaTypes negotiatedSchema

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

apiRequest :: AppConfig -> SchemaCache -> Request -> RequestBody -> QueryParams.QueryParams -> PathInfo -> Action -> (MediaType, MediaType) -> (Schema, Bool) -> Either ApiRequestError ApiRequest
apiRequest conf sCache req reqBody queryparams@QueryParams{..} PathInfo{pathName, pathIsProc, pathIsRootSpec, pathIsDefSpec} action (acceptMediaType, contentMediaType) (schema, negotiatedByProfile)
  | isInvalidRange = Left $ InvalidRange (if rangeIsEmpty headerRange then LowerGTUpper else NegativeLimit)
  | shouldParsePayload && isLeft payload = either (Left . InvalidBody) witness payload
  | not expectParams && not (L.null qsParams) = Left $ ParseRequestError "Unexpected param or filter missing operator" ("Failed to parse " <> show qsParams)
  | method `elem` ["PATCH", "DELETE"] && not (null qsRanges) && null qsOrder = Left LimitNoOrderError
  | method == "PUT" && topLevelRange /= allRange = Left PutRangeNotAllowedError
  | otherwise = do
     checkedTarget <- target
     bField <- binaryField conf acceptMediaType checkedTarget queryparams
     return ApiRequest {
      iAction = action
      , iTarget = checkedTarget
      , iRange = ranges
      , iTopLevelRange = topLevelRange
      , iPayload = relevantPayload
      , iPreferRepresentation = fromMaybe None preferRepresentation
      , iPreferParameters = preferParameters
      , iPreferCount = preferCount
      , iPreferResolution = preferResolution
      , iPreferTransaction = preferTransaction
      , iQueryParams = queryparams
      , iColumns = payloadColumns
      , iHeaders = [ (CI.foldedCase k, v) | (k,v) <- hdrs, k /= hCookie]
      , iCookies = maybe [] parseCookies $ lookupHeader "Cookie"
      , iPath = rawPathInfo req
      , iMethod = method
      , iSchema = schema
      , iNegotiatedByProfile = negotiatedByProfile
      , iAcceptMediaType = acceptMediaType
      , iBinaryField = bField
      }
 where
  expectParams = pathIsProc && method /= "POST"

  columns = case action of
    ActionMutate MutationCreate -> qsColumns
    ActionMutate MutationUpdate -> qsColumns
    ActionInvoke InvPost        -> qsColumns
    _                           -> Nothing

  payloadColumns =
    case (contentMediaType, action) of
      (_, ActionInvoke InvGet)  -> S.fromList $ fst <$> qsParams
      (_, ActionInvoke InvHead) -> S.fromList $ fst <$> qsParams
      (MTUrlEncoded, _)         -> S.fromList $ map (T.decodeUtf8 . fst) $ parseSimpleQuery $ LBS.toStrict reqBody
      _ -> case (relevantPayload, columns) of
        (Just ProcessedJSON{payKeys}, _) -> payKeys
        (Just RawJSON{}, Just cls)       -> cls
        _                                -> S.empty
  payload :: Either ByteString Payload
  payload = case (contentMediaType, pathIsProc) of
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
    (MTUrlEncoded, _) ->
      let paramsMap = HM.fromList $ (T.decodeUtf8 *** JSON.String . T.decodeUtf8) <$> parseSimpleQuery (LBS.toStrict reqBody) in
      Right $ ProcessedJSON (JSON.encode paramsMap) $ S.fromList (HM.keys paramsMap)
    (MTTextPlain, True) -> Right $ RawPay reqBody
    (MTTextXML, True) -> Right $ RawPay reqBody
    (MTOctetStream, True) -> Right $ RawPay reqBody
    (ct, _) -> Left $ "Content-Type not acceptable: " <> MediaType.toMime ct
  topLevelRange = fromMaybe allRange $ HM.lookup "limit" ranges -- if no limit is specified, get all the request rows

  target
    | pathIsProc    = (`TargetProc` pathIsRootSpec) <$> callFindProc schema pathName
    | pathIsDefSpec = Right $ TargetDefaultSpec schema
    | otherwise     = Right $ TargetIdent $ QualifiedIdentifier schema pathName
    where
      callFindProc procSch procNam = findProc
        (QualifiedIdentifier procSch procNam) payloadColumns (preferParameters == Just SingleObject) (dbProcs sCache)
        contentMediaType (action == ActionInvoke InvPost)

  shouldParsePayload = case (action, contentMediaType) of
    (ActionMutate MutationCreate, _)       -> True
    (ActionInvoke InvPost, MTUrlEncoded)   -> False
    (ActionInvoke InvPost, _)              -> True
    (ActionMutate MutationSingleUpsert, _) -> True
    (ActionMutate MutationUpdate, _)       -> True
    _                                      -> False
  relevantPayload = case (contentMediaType, action) of
    -- Though ActionInvoke GET/HEAD doesn't really have a payload, we use the payload variable as a way
    -- to store the query string arguments to the function.
    (_, ActionInvoke InvGet)             -> targetToJsonRpcParams (rightToMaybe target) qsParams
    (_, ActionInvoke InvHead)            -> targetToJsonRpcParams (rightToMaybe target) qsParams
    (MTUrlEncoded, ActionInvoke InvPost) -> targetToJsonRpcParams (rightToMaybe target) $ (T.decodeUtf8 *** T.decodeUtf8) <$> parseSimpleQuery (LBS.toStrict reqBody)
    _ | shouldParsePayload               -> rightToMaybe payload
      | otherwise                        -> Nothing
  method          = requestMethod req
  hdrs            = requestHeaders req
  lookupHeader    = flip lookup hdrs
  Preferences.Preferences{..} = Preferences.fromHeaders hdrs
  headerRange = rangeRequested hdrs
  limitRange = fromMaybe allRange (HM.lookup "limit" qsRanges)
  headerAndLimitRange = rangeIntersection headerRange limitRange

  -- Bypass all the ranges and send only the limit zero range (0 <= x <= -1) if
  -- limit=0 is present in the query params (not allowed for the Range header)
  ranges = HM.insert "limit" (if hasLimitZero limitRange then limitZeroRange else headerAndLimitRange) qsRanges
  -- The only emptyRange allowed is the limit zero range
  isInvalidRange = topLevelRange == emptyRange && not (hasLimitZero limitRange)

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

{-|
  Search a pg proc by matching name and arguments keys to parameters. Since a function can be overloaded,
  the name is not enough to find it. An overloaded function can have a different volatility or even a different return type.
-}
findProc :: QualifiedIdentifier -> S.Set Text -> Bool -> ProcsMap -> MediaType -> Bool -> Either ApiRequestError ProcDescription
findProc qi argumentsKeys paramsAsSingleObject allProcs contentMediaType isInvPost =
  case matchProc of
    ([], [])     -> Left $ NoRpc (qiSchema qi) (qiName qi) (S.toList argumentsKeys) paramsAsSingleObject contentMediaType isInvPost
    -- If there are no functions with named arguments, fallback to the single unnamed argument function
    ([], [proc]) -> Right proc
    ([], procs)  -> Left $ AmbiguousRpc (toList procs)
    -- Matches the functions with named arguments
    ([proc], _)  -> Right proc
    (procs, _)   -> Left $ AmbiguousRpc (toList procs)
  where
    matchProc = overloadedProcPartition $ HM.lookupDefault mempty qi allProcs -- first find the proc by name
    -- The partition obtained has the form (overloadedProcs,fallbackProcs)
    -- where fallbackProcs are functions with a single unnamed parameter
    overloadedProcPartition = foldr select ([],[])
    select proc ~(ts,fs)
      | matchesParams proc         = (proc:ts,fs)
      | hasSingleUnnamedParam proc = (ts,proc:fs)
      | otherwise                  = (ts,fs)
    -- If the function is called with post and has a single unnamed parameter
    -- it can be called depending on content type and the parameter type
    hasSingleUnnamedParam ProcDescription{pdParams=[ProcParam{ppType}]} = isInvPost && case (contentMediaType, ppType) of
      (MTApplicationJSON, "json")  -> True
      (MTApplicationJSON, "jsonb") -> True
      (MTTextPlain, "text")        -> True
      (MTTextXML, "xml")           -> True
      (MTOctetStream, "bytea")     -> True
      _                            -> False
    hasSingleUnnamedParam _ = False
    matchesParams proc =
      let
        params = pdParams proc
        firstType = (ppType <$> headMay params)
      in
      -- exceptional case for Prefer: params=single-object
      if paramsAsSingleObject
        then length params == 1 && (firstType == Just "json" || firstType == Just "jsonb")
      -- If the function has no parameters, the arguments keys must be empty as well
      else if null params
        then null argumentsKeys && not (isInvPost && contentMediaType `elem` [MTOctetStream, MTTextPlain, MTTextXML])
      -- A function has optional and required parameters. Optional parameters have a default value and
      -- don't require arguments for the function to be executed, required parameters must have an argument present.
      else case L.partition ppReq params of
      -- If the function only has required parameters, the arguments keys must match those parameters
        (reqParams, [])        -> argumentsKeys == S.fromList (ppName <$> reqParams)
      -- If the function only has optional parameters, the arguments keys can match none or any of them(a subset)
        ([], optParams)        -> argumentsKeys `S.isSubsetOf` S.fromList (ppName <$> optParams)
      -- If the function has required and optional parameters, the arguments keys have to match the required parameters
      -- and can match any or none of the default parameters.
        (reqParams, optParams) -> argumentsKeys `S.difference` S.fromList (ppName <$> optParams) == S.fromList (ppName <$> reqParams)

-- | If raw(binary) output is requested, check that MediaType is one of the
-- admitted rawMediaTypes and that`?select=...` contains only one field other
-- than `*`
binaryField :: AppConfig -> MediaType -> Target -> QueryParams -> Either ApiRequestError (Maybe FieldName)
binaryField AppConfig{configRawMediaTypes} acceptMediaType target QueryParams{qsSelect}
  | returnsScalar target && isRawMediaType =
      Right $ Just "pgrst_scalar"
  | isRawMediaType =
      let
        fieldName = fstFieldName qsSelect
      in
      case fieldName of
        Just fld -> Right $ Just fld
        Nothing  -> Left $ BinaryFieldError acceptMediaType
  | otherwise =
      Right Nothing
  where
    isRawMediaType = acceptMediaType `elem` configRawMediaTypes `union` [MTOctetStream, MTTextPlain, MTTextXML] || isRawPlan acceptMediaType
    isRawPlan mt = case mt of
      MTPlan (MTPlanAttrs (Just MTOctetStream) _ _) -> True
      MTPlan (MTPlanAttrs (Just MTTextPlain) _ _)   -> True
      MTPlan (MTPlanAttrs (Just MTTextXML) _ _)     -> True
      _                                             -> False
    returnsScalar :: Target -> Bool
    returnsScalar (TargetProc proc _) = procReturnsScalar proc
    returnsScalar _                   = False

    fstFieldName :: [Tree SelectItem] -> Maybe FieldName
    fstFieldName [Node (("*", _), Nothing, Nothing, Nothing, Nothing) []] = Nothing
    fstFieldName [Node ((fld, _), Nothing, Nothing, Nothing, Nothing) []] = Just fld
    fstFieldName _                           = Nothing