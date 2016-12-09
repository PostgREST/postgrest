{-|
Module      : PostgREST.ApiRequest
Description : PostgREST functions to translate HTTP request to a domain type called ApiRequest.
-}
module PostgREST.ApiRequest ( ApiRequest(..)
                            , ApiRequestError(..)
                            , ContentType(..)
                            , Action(..)
                            , Target(..)
                            , PreferRepresentation (..)
                            , mutuallyAgreeable
                            , toHeader
                            , userApiRequest
                            , toMime
                            ) where

import           Protolude
import qualified Data.Aeson                as JSON
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Internal  as BS (c2w)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Csv                  as CSV
import qualified Data.List                 as L
import           Data.List                 (lookup, last)
import qualified Data.HashMap.Strict       as M
import qualified Data.Set                  as S
import           Data.Maybe                (fromJust)
import           Control.Arrow             ((***))
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Network.HTTP.Base         (urlEncodeVars)
import           Network.HTTP.Types.Header (hAuthorization, hContentType, Header)
import           Network.HTTP.Types.URI    (parseSimpleQuery)
import           Network.Wai               (Request (..))
import           Network.Wai.Parse         (parseHttpAccept)
import           PostgREST.RangeQuery      (NonnegRange, rangeRequested, restrictRange, rangeGeq, allRange, rangeLimit, rangeOffset)
import           Data.Ranged.Boundaries
import           PostgREST.Types           (QualifiedIdentifier (..),
                                            Schema,
                                            PayloadJSON(..))
import           Data.Ranged.Ranges        (Range(..), rangeIntersection, emptyRange)

type RequestBody = BL.ByteString

-- | Types of things a user wants to do to tables/views/procs
data Action = ActionCreate | ActionRead
            | ActionUpdate | ActionDelete
            | ActionInfo   | ActionInvoke
            | ActionInspect
            deriving Eq
-- | The target db object of a user action
data Target = TargetIdent QualifiedIdentifier
            | TargetProc  QualifiedIdentifier
            | TargetRoot
            | TargetUnknown [Text]
            deriving Eq
-- | How to return the inserted data
data PreferRepresentation = Full | HeadersOnly | None deriving Eq
                          --
-- | Enumeration of currently supported response content types
data ContentType = CTApplicationJSON | CTTextCSV | CTOpenAPI
                 | CTSingularJSON
                 | CTAny | CTOther BS.ByteString deriving Eq

data ApiRequestError = ErrorActionInappropriate
                     | ErrorInvalidBody ByteString
                     | ErrorInvalidRange
                     deriving (Show, Eq)

-- | Convert from ContentType to a full HTTP Header
toHeader :: ContentType -> Header
toHeader ct = (hContentType, toMime ct <> "; charset=utf-8")

-- | Convert from ContentType to a ByteString representing the mime type
toMime :: ContentType -> ByteString
toMime CTApplicationJSON = "application/json"
toMime CTTextCSV         = "text/csv"
toMime CTOpenAPI         = "application/openapi+json"
toMime CTSingularJSON    = "application/vnd.pgrst.object+json"
toMime CTAny             = "*/*"
toMime (CTOther ct)      = ct

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
  -- | Filters on the result ("id", "eq.10")
  , iFilters :: [(Text, Text)]
  -- | &select parameter used to shape the response
  , iSelect :: Text
  -- | &order parameters for each level
  , iOrder :: [(Text, Text)]
  -- | Alphabetized (canonical) request query string for response URLs
  , iCanonicalQS :: ByteString
  -- | JSON Web Token
  , iJWT :: Text
  }

-- | Examines HTTP request and translates it into user intent.
userApiRequest :: Schema -> Request -> RequestBody -> Either ApiRequestError ApiRequest
userApiRequest schema req reqBody
  | isTargetingProc && method /= "POST" = Left ErrorActionInappropriate
  | topLevelRange == emptyRange = Left ErrorInvalidRange
  | shouldParsePayload && isLeft payload = either (Left . ErrorInvalidBody . toS) undefined payload
  | otherwise = Right ApiRequest {
      iAction = action
      , iTarget = target
      , iRange = ranges
      , iAccepts = fromMaybe [CTAny] $
        map decodeContentType . parseHttpAccept <$> lookupHeader "accept"
      , iPayload = relevantPayload
      , iPreferRepresentation = representation
      , iPreferSingleObjectParameter = singleObject
      , iPreferCount = hasPrefer "count=exact"
      , iFilters = [ (toS k, toS $ fromJust v) | (k,v) <- qParams, isJust v, k /= "select", not (endingIn ["order", "limit", "offset"] k) ]
      , iSelect = toS $ fromMaybe "*" $ fromMaybe (Just "*") $ lookup "select" qParams
      , iOrder = [(toS k, toS $ fromJust v) | (k,v) <- qParams, isJust v, endingIn ["order"] k ]
      , iCanonicalQS = toS $ urlEncodeVars
        . L.sortBy (comparing fst)
        . map (join (***) toS)
        . parseSimpleQuery
        $ rawQueryString req
      , iJWT = tokenStr
      }
 where
  isTargetingProc = fromMaybe False $ (== "rpc") <$> listToMaybe path
  payload =
    case decodeContentType . fromMaybe "application/json" $ lookupHeader "content-type" of
      CTApplicationJSON ->
          either Left (\val -> case ensureUniform (pluralize val) of
            Nothing -> Left "All object keys must match"
            Just json -> Right json) (JSON.eitherDecode reqBody)
      CTTextCSV ->
          either Left (\val -> case ensureUniform (csvToJson val) of
            Nothing -> Left "All lines must have same number of fields"
            Just json -> Right json) (CSV.decodeByName reqBody)
      CTOther "application/x-www-form-urlencoded" ->
        Right . PayloadJSON . V.singleton . M.fromList
                    . map (toS *** JSON.String . toS) . parseSimpleQuery
                    $ toS reqBody
      ct ->
        Left $ toS $ "Content-Type not acceptable: " <> toMime ct
  topLevelRange = fromMaybe allRange $ M.lookup "limit" ranges
  action = case method of
            "GET"     -> if target == TargetRoot
                          then ActionInspect
                          else ActionRead
            "POST"    -> if isTargetingProc
                          then ActionInvoke
                          else ActionCreate
            "PATCH"   -> ActionUpdate
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
  shouldParsePayload = action `elem` [ActionCreate, ActionUpdate, ActionInvoke]
  relevantPayload = if shouldParsePayload
                      then rightToMaybe payload
                      else Nothing
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
  offsetParams = M.fromList [(toS (replaceLast "limit" k), fromMaybe allRange (rangeGeq <$> (readMaybe =<< (toS <$> v)))) | (k,v) <- qParams, isJust v, endingIn ["offset"] k]

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
csvToJson :: (CSV.Header, CsvData) -> JSON.Array
csvToJson (_, vals) =
  V.map rowToJsonObj vals
 where
  rowToJsonObj = JSON.Object .
    M.map (\str ->
        if str == "NULL"
          then JSON.Null
          else JSON.String $ toS str
      )

-- | Convert {foo} to [{foo}], leave arrays unchanged
-- and truncate everything else to an empty array.
pluralize :: JSON.Value -> JSON.Array
pluralize obj@(JSON.Object _) = V.singleton obj
pluralize (JSON.Array arr)    = arr
pluralize _                   = V.empty

-- | Test that Array contains only Objects having the same keys
-- and if so mark it as PayloadJSON
ensureUniform :: JSON.Array -> Maybe PayloadJSON
ensureUniform arr =
  let objs :: V.Vector JSON.Object
      objs = foldr -- filter non-objects, map to raw objects
               (\val result -> case val of
                  JSON.Object o -> V.cons o result
                  _ -> result)
               V.empty arr
      keysPerObj = V.map (S.fromList . M.keys) objs
      canonicalKeys = fromMaybe S.empty $ keysPerObj V.!? 0
      areKeysUniform = all (==canonicalKeys) keysPerObj in

  if (V.length objs == V.length arr) && areKeysUniform
    then Just (PayloadJSON objs)
    else Nothing
