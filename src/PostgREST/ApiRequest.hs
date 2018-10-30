{-# LANGUAGE LambdaCase #-}
{-|
Module      : PostgREST.ApiRequest
Description : PostgREST functions to translate HTTP request to a domain type called ApiRequest.
-}
module PostgREST.ApiRequest ( ApiRequest(..)
                            , ContentType(..)
                            , Action(..)
                            , Target(..)
                            , PreferRepresentation (..)
                            , mutuallyAgreeable
                            , userApiRequest
                            ) where

import           Protolude
import qualified Data.Aeson                as JSON
import           Data.Aeson.Types          (emptyObject, emptyArray)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Internal  as BS (c2w)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Csv                  as CSV
import qualified Data.List                 as L
import           Data.List                 (lookup, last, partition)
import qualified Data.HashMap.Strict       as M
import qualified Data.Set                  as S
import           Data.Maybe                (fromJust)
import           Control.Arrow             ((***))
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Network.HTTP.Base         (urlEncodeVars)
import           Network.HTTP.Types.Header (hAuthorization, hCookie)
import           Network.HTTP.Types.URI    (parseSimpleQuery)
import           Network.Wai               (Request (..))
import           Network.Wai.Parse         (parseHttpAccept)
import           PostgREST.RangeQuery      (NonnegRange, rangeRequested, restrictRange, rangeGeq, allRange, rangeLimit, rangeOffset)
import           Data.Ranged.Boundaries
import           PostgREST.Types
import           Data.Ranged.Ranges        (Range(..), rangeIntersection, emptyRange)
import qualified Data.CaseInsensitive      as CI
import           Web.Cookie                (parseCookiesText)

type RequestBody = BL.ByteString

-- | Types of things a user wants to do to tables/views/procs
data Action = ActionCreate  | ActionRead
            | ActionUpdate  | ActionDelete
            | ActionInfo    | ActionInvoke{isReadOnly :: Bool}
            | ActionInspect | ActionSingleUpsert
            deriving Eq
-- | The target db object of a user action
data Target = TargetIdent QualifiedIdentifier
            | TargetProc  QualifiedIdentifier
            | TargetRoot
            | TargetUnknown [Text]
            deriving Eq
-- | How to return the inserted data
data PreferRepresentation = Full | HeadersOnly | None deriving Eq

{-|
  Describes what the user wants to do. This data type is a
  translation of the raw elements of an HTTP request into domain
  specific language.  There is no guarantee that the intent is
  sensible, it is up to a later stage of processing to determine
  if it is an action we are able to perform.
-}
data ApiRequest = ApiRequest {
  -- | Similar but not identical to HTTP verb, e.g. Create/Invoke both POST
    iAction :: Action
  -- | Requested range of rows within response
  , iRange  :: M.HashMap ByteString NonnegRange
  -- | The target, be it calling a proc or accessing a table
  , iTarget :: Target
  -- | Content types the client will accept, [CTAny] if no Accept header
  , iAccepts :: [ContentType]
  -- | Data sent by client and used for mutation actions
  , iPayload :: Maybe PayloadJSON
  -- | If client wants created items echoed back
  , iPreferRepresentation :: PreferRepresentation
  -- | Pass all parameters as a single json object to a stored procedure
  , iPreferSingleObjectParameter :: Bool
  -- | Whether the client wants a result count (slower)
  , iPreferCount :: Bool
  -- | Whether the client wants to UPSERT or ignore records on PK conflict
  , iPreferResolution :: Maybe PreferResolution
  -- | Filters on the result ("id", "eq.10")
  , iFilters :: [(Text, Text)]
  -- | &and and &or parameters used for complex boolean logic
  , iLogic :: [(Text, Text)]
  -- | &select parameter used to shape the response
  , iSelect :: Text
  -- | &order parameters for each level
  , iOrder :: [(Text, Text)]
  -- | Alphabetized (canonical) request query string for response URLs
  , iCanonicalQS :: ByteString
  -- | JSON Web Token
  , iJWT :: Text
  -- | HTTP request headers
  , iHeaders :: [(Text, Text)]
  -- | Request Cookies
  , iCookies :: [(Text, Text)]
  }

-- | Examines HTTP request and translates it into user intent.
userApiRequest :: Schema -> Request -> RequestBody -> Either ApiRequestError ApiRequest
userApiRequest schema req reqBody
  | isTargetingProc && method `notElem` ["GET", "POST"] = Left ActionInappropriate
  | topLevelRange == emptyRange = Left InvalidRange
  | shouldParsePayload && isLeft payload = either (Left . InvalidBody . toS) witness payload
  | otherwise = Right ApiRequest {
      iAction = action
      , iTarget = target
      , iRange = ranges
      , iAccepts = maybe [CTAny] (map decodeContentType . parseHttpAccept) $ lookupHeader "accept"
      , iPayload = relevantPayload
      , iPreferRepresentation = representation
      , iPreferSingleObjectParameter = singleObject
      , iPreferCount = hasPrefer "count=exact"
      , iPreferResolution = if hasPrefer (show MergeDuplicates) then Just MergeDuplicates
                            else if hasPrefer (show IgnoreDuplicates) then Just IgnoreDuplicates
                            else Nothing
      , iFilters = filters
      , iLogic = [(toS k, toS $ fromJust v) | (k,v) <- qParams, isJust v, endingIn ["and", "or"] k ]
      , iSelect = toS $ fromMaybe "*" $ fromMaybe (Just "*") $ lookup "select" qParams
      , iOrder = [(toS k, toS $ fromJust v) | (k,v) <- qParams, isJust v, endingIn ["order"] k ]
      , iCanonicalQS = toS $ urlEncodeVars
        . L.sortBy (comparing fst)
        . map (join (***) toS)
        . parseSimpleQuery
        $ rawQueryString req
      , iJWT = tokenStr
      , iHeaders = [ (toS $ CI.foldedCase k, toS v) | (k,v) <- hdrs, k /= hAuthorization, k /= hCookie]
      , iCookies = maybe [] parseCookiesText $ lookupHeader "Cookie"
      }
 where
  -- rpcQParams = Rpc query params e.g. /rpc/name?param1=val1, similar to filter but with no operator(eq, lt..)
  (filters, rpcQParams) =
    case action of
      ActionInvoke{isReadOnly=True} -> partition (liftM2 (||) (isEmbedPath . fst) (hasOperator . snd)) flts
      _ -> (flts, [])
  flts = [ (toS k, toS $ fromJust v) | (k,v) <- qParams, isJust v, k /= "select", not (endingIn ["order", "limit", "offset", "and", "or"] k) ]
  hasOperator val = any (`T.isPrefixOf` val) $
                      ((<> ".") <$> "not":M.keys operators) ++
                      ((<> "(") <$> M.keys ftsOperators)
  isEmbedPath = T.isInfixOf "."
  isTargetingProc = (== Just "rpc") $ listToMaybe path
  payload =
    case (decodeContentType . fromMaybe "application/json" $ lookupHeader "content-type", action) of
      (_, ActionInvoke{isReadOnly=True}) ->
        Right $ PayloadJSON (JSON.encode $ M.fromList $ second JSON.toJSON <$> rpcQParams) PJObject (S.fromList $ fst <$> rpcQParams)
      (CTApplicationJSON, _) ->
        note "All object keys must match" . payloadAttributes reqBody
          =<< if BL.null reqBody && isTargetingProc
               then Right emptyObject
               else JSON.eitherDecode reqBody
      (CTTextCSV, _) -> do
        json <- csvToJson <$> CSV.decodeByName reqBody
        note "All lines must have same number of fields" $ payloadAttributes (JSON.encode json) json
      (CTOther "application/x-www-form-urlencoded", _) ->
        let json = M.fromList . map (toS *** JSON.String . toS) . parseSimpleQuery $ toS reqBody
            keys = S.fromList $ M.keys json in
        Right $ PayloadJSON (JSON.encode json) PJObject keys
      (ct, _) ->
        Left $ toS $ "Content-Type not acceptable: " <> toMime ct
  topLevelRange = fromMaybe allRange $ M.lookup "limit" ranges
  action =
    case method of
      "GET"      | target == TargetRoot -> ActionInspect
                 | isTargetingProc -> ActionInvoke{isReadOnly=True}
                 | otherwise -> ActionRead

      "POST"    -> if isTargetingProc
                    then ActionInvoke{isReadOnly=False}
                    else ActionCreate
      "PATCH"   -> ActionUpdate
      "PUT"     -> ActionSingleUpsert
      "DELETE"  -> ActionDelete
      "OPTIONS" -> ActionInfo
      _         -> ActionInspect
  target = case path of
              []            -> TargetRoot
              [table]       -> TargetIdent
                              $ QualifiedIdentifier schema table
              ["rpc", proc] -> TargetProc
                              $ QualifiedIdentifier schema proc
              other         -> TargetUnknown other
  shouldParsePayload = action `elem` [ActionCreate, ActionUpdate, ActionSingleUpsert, ActionInvoke{isReadOnly=False}, ActionInvoke{isReadOnly=True}]
  relevantPayload | shouldParsePayload = rightToMaybe payload
                  | otherwise = Nothing
  path            = pathInfo req
  method          = requestMethod req
  hdrs            = requestHeaders req
  qParams         = [(toS k, v)|(k,v) <- queryString req]
  lookupHeader    = flip lookup hdrs
  hasPrefer :: Text -> Bool
  hasPrefer val   = any (\(h,v) -> h == "Prefer" && val `elem` split v) hdrs
    where
        split :: BS.ByteString -> [Text]
        split = map T.strip . T.split (==',') . toS
  singleObject    = hasPrefer "params=single-object"
  representation
    | hasPrefer "return=representation" = Full
    | hasPrefer "return=minimal" = None
    | otherwise = HeadersOnly
  auth = fromMaybe "" $ lookupHeader hAuthorization
  tokenStr = case T.split (== ' ') (toS auth) of
    ("Bearer" : t : _) -> t
    _                  -> ""
  endingIn:: [Text] -> Text -> Bool
  endingIn xx key = lastWord `elem` xx
    where lastWord = last $ T.split (=='.') key

  headerRange = rangeRequested hdrs
  replaceLast x s = T.intercalate "." $ L.init (T.split (=='.') s) ++ [x]
  limitParams :: M.HashMap ByteString NonnegRange
  limitParams  = M.fromList [(toS (replaceLast "limit" k), restrictRange (readMaybe =<< (toS <$> v)) allRange) | (k,v) <- qParams, isJust v, endingIn ["limit"] k]
  offsetParams :: M.HashMap ByteString NonnegRange
  offsetParams = M.fromList [(toS (replaceLast "limit" k), maybe allRange rangeGeq (readMaybe =<< (toS <$> v))) | (k,v) <- qParams, isJust v, endingIn ["offset"] k]

  urlRange = M.unionWith f limitParams offsetParams
    where
      f rl ro = Range (BoundaryBelow o) (BoundaryAbove $ o + l - 1)
        where
          l = fromMaybe 0 $ rangeLimit rl
          o = rangeOffset ro
  ranges = M.insert "limit" (rangeIntersection headerRange (fromMaybe allRange (M.lookup "limit" urlRange))) urlRange

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

-- PRIVATE ---------------------------------------------------------------

{-|
  Warning: discards MIME parameters
-}
decodeContentType :: BS.ByteString -> ContentType
decodeContentType ct =
  case BS.takeWhile (/= BS.c2w ';') ct of
    "application/json"                  -> CTApplicationJSON
    "text/csv"                          -> CTTextCSV
    "application/openapi+json"          -> CTOpenAPI
    "application/vnd.pgrst.object+json" -> CTSingularJSON
    "application/vnd.pgrst.object"      -> CTSingularJSON
    "application/octet-stream"          -> CTOctetStream
    "*/*"                               -> CTAny
    ct'                                 -> CTOther ct'

type CsvData = V.Vector (M.HashMap Text BL.ByteString)

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
          else JSON.String $ toS str
      )

payloadAttributes :: RequestBody -> JSON.Value -> Maybe PayloadJSON
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
            then Just $ PayloadJSON raw (PJArray $ V.length arr) canonicalKeys
            else Nothing
        Just _ -> Nothing
        Nothing -> Just emptyPJArray

    JSON.Object o -> Just $ PayloadJSON raw PJObject (S.fromList $ M.keys o)

    -- truncate everything else to an empty array.
    _ -> Just emptyPJArray
  where
    emptyPJArray = PayloadJSON (JSON.encode emptyArray) (PJArray 0) S.empty
