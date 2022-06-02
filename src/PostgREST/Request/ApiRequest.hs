{-|
Module      : PostgREST.Request.ApiRequest
Description : PostgREST functions to translate HTTP request to a domain type called ApiRequest.
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Request.ApiRequest
  ( ApiRequest(..)
  , InvokeMethod(..)
  , Mutation(..)
  , ContentType(..)
  , Action(..)
  , Target(..)
  , Payload(..)
  , userApiRequest
  ) where

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.CaseInsensitive  as CI
import qualified Data.Csv              as CSV
import qualified Data.HashMap.Strict   as M
import qualified Data.List             as L
import qualified Data.List.NonEmpty    as NonEmptyList
import qualified Data.Set              as S
import qualified Data.Text.Encoding    as T
import qualified Data.Vector           as V

import Control.Arrow             ((***))
import Data.Aeson.Types          (emptyArray, emptyObject)
import Data.List                 (lookup, union)
import Data.Maybe                (fromJust)
import Data.Ranged.Ranges        (emptyRange, rangeIntersection)
import Network.HTTP.Types.Header (hCookie)
import Network.HTTP.Types.URI    (parseSimpleQuery)
import Network.Wai               (Request (..))
import Network.Wai.Parse         (parseHttpAccept)
import Web.Cookie                (parseCookies)

import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.ContentType             (ContentType (..))
import PostgREST.DbStructure             (DbStructure (..))
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)
import PostgREST.DbStructure.Proc        (ProcDescription (..),
                                          ProcParam (..), ProcsMap)
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          hasLimitZero,
                                          limitZeroRange,
                                          rangeRequested)
import PostgREST.Request.Preferences     (PreferCount (..),
                                          PreferParameters (..),
                                          PreferRepresentation (..),
                                          PreferResolution (..),
                                          PreferTransaction (..))
import PostgREST.Request.QueryParams     (QueryParams (..))
import PostgREST.Request.Types           (ApiRequestError (..))

import qualified PostgREST.ContentType         as ContentType
import qualified PostgREST.Request.Preferences as Preferences
import qualified PostgREST.Request.QueryParams as QueryParams

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
  | ActionUnknown Text
  deriving Eq
-- | The path info that will be mapped to a target (used to handle validations and errors before defining the Target)
data Path
  = PathInfo
      { pSchema        :: Schema,
        pName          :: Text,
        pHasRpc        :: Bool,
        pIsDefaultSpec :: Bool,
        pIsRootSpec    :: Bool
      }
  | PathUnknown
-- | The target db object of a user action
data Target = TargetIdent QualifiedIdentifier
            | TargetProc{tProc :: ProcDescription, tpIsRootSpec :: Bool}
            | TargetDefaultSpec{tdsSchema :: Schema} -- The default spec offered at root "/"
            | TargetUnknown

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
    ProcessedJSON (JSON.encode $ M.fromList $ second JSON.toJSON <$> prms) (S.fromList $ fst <$> prms)
  else
    let paramsMap = M.fromListWith mergeParams $ toRpcParamValue proc <$> prms in
    ProcessedJSON (JSON.encode paramsMap) (S.fromList $ M.keys paramsMap)
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
    iAction               :: Action                           -- ^ Similar but not identical to HTTP verb, e.g. Create/Invoke both POST
  , iRange                :: M.HashMap Text NonnegRange       -- ^ Requested range of rows within response
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
  , iProfile              :: Maybe Schema                     -- ^ The request profile for enabling use of multiple schemas. Follows the spec in hhttps://www.w3.org/TR/dx-prof-conneg/ttps://www.w3.org/TR/dx-prof-conneg/.
  , iSchema               :: Schema                           -- ^ The request schema. Can vary depending on iProfile.
  , iAcceptContentType    :: ContentType
  }

-- | Examines HTTP request and translates it into user intent.
userApiRequest :: AppConfig -> DbStructure -> Request -> RequestBody -> Either ApiRequestError ApiRequest
userApiRequest conf dbStructure req reqBody =
  apiRequest conf dbStructure req reqBody =<< first QueryParamError (QueryParams.parse (rawQueryString req))

apiRequest :: AppConfig -> DbStructure -> Request -> RequestBody -> QueryParams.QueryParams -> Either ApiRequestError ApiRequest
apiRequest conf@AppConfig{..} dbStructure req reqBody queryparams@QueryParams{..}
  | isJust profile && fromJust profile `notElem` configDbSchemas = Left $ UnacceptableSchema $ toList configDbSchemas
  | isTargetingProc && method `notElem` ["HEAD", "GET", "POST"] = Left ActionInappropriate
  | isInvalidRange = Left InvalidRange
  | shouldParsePayload && isLeft payload = either (Left . InvalidBody) witness payload
  | not expectParams && not (L.null qsParams) = Left $ ParseRequestError "Unexpected param or filter missing operator" ("Failed to parse " <> show qsParams)
  | method `elem` ["PATCH", "DELETE"] && not (null qsRanges) && null qsOrder = Left LimitNoOrderError
  | method == "PUT" && topLevelRange /= allRange = Left PutRangeNotAllowedError
  | otherwise = do
     acceptContentType <- findAcceptContentType conf action path accepts
     checkedTarget <- target
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
      , iProfile = profile
      , iSchema = schema
      , iAcceptContentType = acceptContentType
      }
 where
  accepts = maybe [CTAny] (map ContentType.decodeContentType . parseHttpAccept) $ lookupHeader "accept"

  expectParams = isTargetingProc && method /= "POST"

  isTargetingProc = case path of
    PathInfo{pHasRpc, pIsRootSpec} -> pHasRpc || pIsRootSpec
    _                              -> False
  isTargetingDefaultSpec = case path of
    PathInfo{pIsDefaultSpec=True} -> True
    _                             -> False
  contentType = maybe CTApplicationJSON ContentType.decodeContentType $ lookupHeader "content-type"

  columns = case action of
    ActionMutate MutationCreate -> qsColumns
    ActionMutate MutationUpdate -> qsColumns
    ActionInvoke InvPost        -> qsColumns
    _                           -> Nothing

  payloadColumns =
    case (contentType, action) of
      (_, ActionInvoke InvGet)  -> S.fromList $ fst <$> qsParams
      (_, ActionInvoke InvHead) -> S.fromList $ fst <$> qsParams
      (CTUrlEncoded, _)         -> S.fromList $ map (T.decodeUtf8 . fst) $ parseSimpleQuery $ LBS.toStrict reqBody
      _ -> case (relevantPayload, columns) of
        (Just ProcessedJSON{payKeys}, _) -> payKeys
        (Just RawJSON{}, Just cls)       -> cls
        _                                -> S.empty
  payload :: Either ByteString Payload
  payload = case (contentType, isTargetingProc) of
    (CTApplicationJSON, _) ->
      if isJust columns
        then Right $ RawJSON reqBody
        else note "All object keys must match" . payloadAttributes reqBody
               =<< if LBS.null reqBody && isTargetingProc
                     then Right emptyObject
                     else first BS.pack $ JSON.eitherDecode reqBody
    (CTTextCSV, _) -> do
      json <- csvToJson <$> first BS.pack (CSV.decodeByName reqBody)
      note "All lines must have same number of fields" $ payloadAttributes (JSON.encode json) json
    (CTUrlEncoded, _) ->
      let paramsMap = M.fromList $ (T.decodeUtf8 *** JSON.String . T.decodeUtf8) <$> parseSimpleQuery (LBS.toStrict reqBody) in
      Right $ ProcessedJSON (JSON.encode paramsMap) $ S.fromList (M.keys paramsMap)
    (CTTextPlain, True) -> Right $ RawPay reqBody
    (CTTextXML, True) -> Right $ RawPay reqBody
    (CTOctetStream, True) -> Right $ RawPay reqBody
    (ct, _) -> Left $ "Content-Type not acceptable: " <> ContentType.toMime ct
  topLevelRange = fromMaybe allRange $ M.lookup "limit" ranges -- if no limit is specified, get all the request rows
  action =
    case method of
      -- The HEAD method is identical to GET except that the server MUST NOT return a message-body in the response
      -- From https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.4
      "HEAD"     | isTargetingDefaultSpec -> ActionInspect{isHead=True}
                 | isTargetingProc        -> ActionInvoke InvHead
                 | otherwise              -> ActionRead{isHead=True}
      "GET"      | isTargetingDefaultSpec -> ActionInspect{isHead=False}
                 | isTargetingProc        -> ActionInvoke InvGet
                 | otherwise              -> ActionRead{isHead=False}
      "POST"    -> if isTargetingProc
                    then ActionInvoke InvPost
                    else ActionMutate MutationCreate
      "PATCH"   -> ActionMutate MutationUpdate
      "PUT"     -> ActionMutate MutationSingleUpsert
      "DELETE"  -> ActionMutate MutationDelete
      "OPTIONS" -> ActionInfo
      _         -> ActionUnknown $ T.decodeUtf8 method

  defaultSchema = NonEmptyList.head configDbSchemas
  profile
    | length configDbSchemas <= 1 -- only enable content negotiation by profile when there are multiple schemas specified in the config
      = Nothing
    | otherwise = case method of
        -- POST/PATCH/PUT/DELETE don't use the same header as per the spec
        "DELETE" -> contentProfile
        "PATCH"  -> contentProfile
        "POST"   -> contentProfile
        "PUT"    -> contentProfile
        _        -> acceptProfile
    where
      contentProfile = Just $ maybe defaultSchema T.decodeUtf8 $ lookupHeader "Content-Profile"
      acceptProfile = Just $ maybe defaultSchema T.decodeUtf8 $ lookupHeader "Accept-Profile"
  schema = fromMaybe defaultSchema profile
  target =
    let
      callFindProc procSch procNam = findProc
        (QualifiedIdentifier procSch procNam) payloadColumns (preferParameters == Just SingleObject) (dbProcs dbStructure)
        contentType (action == ActionInvoke InvPost)
    in
    case path of
      PathInfo{pSchema, pName, pHasRpc, pIsRootSpec, pIsDefaultSpec}
        | pHasRpc || pIsRootSpec -> (`TargetProc` pIsRootSpec) <$> callFindProc pSchema pName
        | pIsDefaultSpec         -> Right $ TargetDefaultSpec pSchema
        | otherwise              -> Right $ TargetIdent $ QualifiedIdentifier pSchema pName
      PathUnknown -> Right TargetUnknown

  shouldParsePayload = case (action, contentType) of
    (ActionMutate MutationCreate, _)       -> True
    (ActionInvoke InvPost, CTUrlEncoded)   -> False
    (ActionInvoke InvPost, _)              -> True
    (ActionMutate MutationSingleUpsert, _) -> True
    (ActionMutate MutationUpdate, _)       -> True
    _                                      -> False
  relevantPayload = case (contentType, action) of
    -- Though ActionInvoke GET/HEAD doesn't really have a payload, we use the payload variable as a way
    -- to store the query string arguments to the function.
    (_, ActionInvoke InvGet)             -> targetToJsonRpcParams (rightToMaybe target) qsParams
    (_, ActionInvoke InvHead)            -> targetToJsonRpcParams (rightToMaybe target) qsParams
    (CTUrlEncoded, ActionInvoke InvPost) -> targetToJsonRpcParams (rightToMaybe target) $ (T.decodeUtf8 *** T.decodeUtf8) <$> parseSimpleQuery (LBS.toStrict reqBody)
    _ | shouldParsePayload               -> rightToMaybe payload
      | otherwise                        -> Nothing
  path =
    case pathInfo req of
      []             -> case configDbRootSpec of
                          Just (QualifiedIdentifier pSch pName)     -> PathInfo (if pSch == mempty then schema else pSch) pName False False True
                          Nothing | configOpenApiMode == OADisabled -> PathUnknown
                                  | otherwise                       -> PathInfo schema "" False True False
      [table]        -> PathInfo schema table False False False
      ["rpc", pName] -> PathInfo schema pName True False False
      _              -> PathUnknown
  method          = requestMethod req
  hdrs            = requestHeaders req
  lookupHeader    = flip lookup hdrs
  Preferences.Preferences{..} = Preferences.fromHeaders hdrs
  headerRange = rangeRequested hdrs
  limitRange = fromMaybe allRange (M.lookup "limit" qsRanges)
  headerAndLimitRange = rangeIntersection headerRange limitRange

  -- Bypass all the ranges and send only the limit zero range (0 <= x <= -1) if
  -- limit=0 is present in the query params (not allowed for the Range header)
  ranges = M.insert "limit" (if hasLimitZero limitRange then limitZeroRange else headerAndLimitRange) qsRanges
  -- The only emptyRange allowed is the limit zero range
  isInvalidRange = topLevelRange == emptyRange && not (hasLimitZero limitRange)

{-|
  Find the best match from a list of content types accepted by the
  client in order of decreasing preference and a list of types
  producible by the server.  If there is no match but the client
  accepts */* then return the top server pick.
-}
mutuallyAgreeable :: [ContentType] -> [ContentType] -> Maybe ContentType
mutuallyAgreeable sProduces cAccepts =
  let exact = listToMaybe $ L.intersect cAccepts sProduces in
  if isNothing exact && CTAny `elem` cAccepts
     then listToMaybe sProduces
     else exact

type CsvData = V.Vector (M.HashMap Text LBS.ByteString)

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
  rowToJsonObj = JSON.Object .
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
          let canonicalKeys = S.fromList $ M.keys o
              areKeysUniform = all (\case
                JSON.Object x -> S.fromList (M.keys x) == canonicalKeys
                _ -> False) arr in
          if areKeysUniform
            then Just $ ProcessedJSON raw canonicalKeys
            else Nothing
        Just _ -> Nothing
        Nothing -> Just emptyPJArray

    JSON.Object o -> Just $ ProcessedJSON raw (S.fromList $ M.keys o)

    -- truncate everything else to an empty array.
    _ -> Just emptyPJArray
  where
    emptyPJArray = ProcessedJSON (JSON.encode emptyArray) S.empty

findAcceptContentType :: AppConfig -> Action -> Path -> [ContentType] -> Either ApiRequestError ContentType
findAcceptContentType conf action path accepts =
  case mutuallyAgreeable (requestContentTypes conf action path) accepts of
    Just ct ->
      Right ct
    Nothing ->
      Left . ContentTypeError $ map ContentType.toMime accepts

requestContentTypes :: AppConfig -> Action -> Path -> [ContentType]
requestContentTypes conf action path =
  case action of
    ActionRead _    -> defaultContentTypes ++ rawContentTypes conf
    ActionInvoke _  -> invokeContentTypes
    ActionInspect _ -> [CTOpenAPI, CTApplicationJSON]
    ActionInfo      -> [CTTextCSV]
    _               -> defaultContentTypes
  where
    invokeContentTypes =
      defaultContentTypes
        ++ rawContentTypes conf
        ++ [CTOpenAPI | pIsRootSpec path]
    defaultContentTypes =
      [CTApplicationJSON, CTSingularJSON, CTTextCSV]

rawContentTypes :: AppConfig -> [ContentType]
rawContentTypes AppConfig{..} =
  (ContentType.decodeContentType <$> configRawMediaTypes) `union` [CTOctetStream, CTTextPlain, CTTextXML]

{-|
  Search a pg proc by matching name and arguments keys to parameters. Since a function can be overloaded,
  the name is not enough to find it. An overloaded function can have a different volatility or even a different return type.
-}
findProc :: QualifiedIdentifier -> S.Set Text -> Bool -> ProcsMap -> ContentType -> Bool -> Either ApiRequestError ProcDescription
findProc qi argumentsKeys paramsAsSingleObject allProcs contentType isInvPost =
  case matchProc of
    ([], [])     -> Left $ NoRpc (qiSchema qi) (qiName qi) (S.toList argumentsKeys) paramsAsSingleObject contentType isInvPost
    -- If there are no functions with named arguments, fallback to the single unnamed argument function
    ([], [proc]) -> Right proc
    ([], procs)  -> Left $ AmbiguousRpc (toList procs)
    -- Matches the functions with named arguments
    ([proc], _)  -> Right proc
    (procs, _)   -> Left $ AmbiguousRpc (toList procs)
  where
    matchProc = overloadedProcPartition $ M.lookupDefault mempty qi allProcs -- first find the proc by name
    -- The partition obtained has the form (overloadedProcs,fallbackProcs)
    -- where fallbackProcs are functions with a single unnamed parameter
    overloadedProcPartition procs = foldr select ([],[]) procs
    select proc ~(ts,fs)
      | matchesParams proc         = (proc:ts,fs)
      | hasSingleUnnamedParam proc = (ts,proc:fs)
      | otherwise                  = (ts,fs)
    -- If the function is called with post and has a single unnamed parameter
    -- it can be called depending on content type and the parameter type
    hasSingleUnnamedParam ProcDescription{pdParams=[ProcParam{ppType}]} = isInvPost && case (contentType, ppType) of
      (CTApplicationJSON, "json")  -> True
      (CTApplicationJSON, "jsonb") -> True
      (CTTextPlain, "text")        -> True
      (CTTextXML, "xml")           -> True
      (CTOctetStream, "bytea")     -> True
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
        then null argumentsKeys && not (isInvPost && contentType `elem` [CTOctetStream, CTTextPlain, CTTextXML])
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
