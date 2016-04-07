module PostgREST.ApiRequest where

import qualified Data.Aeson              as JSON
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Csv                as CSV
import           Data.List               (find)
import qualified Data.HashMap.Strict     as M
import qualified Data.Set                as S
import           Data.Maybe              (fromMaybe, isJust, isNothing,
                                          listToMaybe, fromJust)
import           Control.Monad           (join)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           Network.Wai             (Request (..))
import           Network.Wai.Parse       (parseHttpAccept)
import           PostgREST.RangeQuery    (NonnegRange, rangeRequested)
import           PostgREST.Types         (QualifiedIdentifier (..),
                                          Schema, Payload(..),
                                          UniformObjects(..))
import           Data.Ranged.Ranges      (singletonRange)

type RequestBody = BL.ByteString

-- | Types of things a user wants to do to tables/views/procs
data Action = ActionCreate | ActionRead
            | ActionUpdate | ActionDelete
            | ActionInfo   | ActionInvoke
            | ActionInappropriate
            deriving Eq
-- | The target db object of a user action
data Target = TargetIdent QualifiedIdentifier
            | TargetProc  QualifiedIdentifier
            | TargetRoot
            | TargetUnknown [T.Text]
-- | How to return the inserted data
data PreferRepresentation = Full | HeadersOnly | None deriving Eq
-- | Enumeration of currently supported content types for
-- route responses and upload payloads
data ContentType = ApplicationJSON | TextCSV deriving Eq
instance Show ContentType where
  show ApplicationJSON = "application/json; charset=utf-8"
  show TextCSV         = "text/csv; charset=utf-8"

{-|
  Describes what the user wants to do. This data type is a
  translation of the raw elements of an HTTP request into domain
  specific language.  There is no guarantee that the intent is
  sensible, it is up to a later stage of processing to determine
  if it is an action we are able to perform.
-}
data ApiRequest = ApiRequest {
  -- | Set to Nothing for unknown HTTP verbs
    iAction :: Action
  -- | Set to Nothing for malformed range
  , iRange  :: NonnegRange
  -- | Set to Nothing for strangely nested urls
  , iTarget :: Target
  -- | The content type the client most desires (or JSON if undecided)
  , iAccepts :: Either BS.ByteString ContentType
  -- | Data sent by client and used for mutation actions
  , iPayload :: Maybe Payload
  -- | If client wants created items echoed back
  , iPreferRepresentation :: PreferRepresentation
  -- | If client wants first row as raw object
  , iPreferSingular :: Bool
  -- | Whether the client wants a result count (slower)
  , iPreferCount :: Bool
  -- | Filters on the result ("id", "eq.10")
  , iFilters :: [(String, String)]
  -- | &select parameter used to shape the response
  , iSelect :: String
  -- | &order parameter
  , iOrder :: Maybe String
  }

-- | Examines HTTP request and translates it into user intent.
userApiRequest :: Schema -> Request -> RequestBody -> ApiRequest
userApiRequest schema req reqBody =
  let action =
        if isTargetingProc
          then
            if method == "POST"
               then ActionInvoke
               else ActionInappropriate
          else
            case method of
               "GET"     -> ActionRead
               "POST"    -> ActionCreate
               "PATCH"   -> ActionUpdate
               "DELETE"  -> ActionDelete
               "OPTIONS" -> ActionInfo
               _         -> ActionInappropriate
      target = case path of
                 []            -> TargetRoot
                 [table]       -> TargetIdent
                                  $ QualifiedIdentifier schema table
                 ["rpc", proc] -> TargetProc
                                  $ QualifiedIdentifier schema proc
                 other         -> TargetUnknown other
      payload = case pickContentType (lookupHeader "content-type") of
        Right ApplicationJSON ->
          either (PayloadParseError . cs)
            (\val -> case ensureUniform (pluralize val) of
              Nothing -> PayloadParseError "All object keys must match"
              Just json -> PayloadJSON json)
            (JSON.eitherDecode reqBody)
        Right TextCSV ->
          either (PayloadParseError . cs)
            (\val -> case ensureUniform (csvToJson val) of
              Nothing -> PayloadParseError "All lines must have same number of fields"
              Just json -> PayloadJSON json)
            (CSV.decodeByName reqBody)
        Left accept ->
          PayloadParseError $
            "Content-type not acceptable: " <> accept
      relevantPayload = case action of
        ActionCreate -> Just payload
        ActionUpdate -> Just payload
        ActionInvoke -> Just payload
        _            -> Nothing in

  ApiRequest {
    iAction = action
  , iRange  = if singular then singletonRange 0 else rangeRequested hdrs
  , iTarget = target
  , iAccepts = pickContentType $ lookupHeader "accept"
  , iPayload = relevantPayload
  , iPreferRepresentation = representation
  , iPreferSingular = singular
  , iPreferCount = not $ singular || hasPrefer "count=none"
  , iFilters = [ (k, fromJust v) | (k,v) <- qParams, k `notElem` ["select", "order"], isJust v ]
  , iSelect = if method == "DELETE"
              then "*"
              else fromMaybe "*" $ fromMaybe (Just "*") $ lookup "select" qParams
  , iOrder = join $ lookup "order" qParams
  }

 where
  path            = pathInfo req
  method          = requestMethod req
  isTargetingProc = fromMaybe False $ (== "rpc") <$> listToMaybe path
  hdrs            = requestHeaders req
  qParams         = [(cs k, cs <$> v)|(k,v) <- queryString req]
  lookupHeader    = flip lookup hdrs
  hasPrefer :: T.Text -> Bool
  hasPrefer val   = any (\(h,v) -> h == "Prefer" && val `elem` split v) hdrs
    where
        split :: BS.ByteString -> [T.Text]
        split = map T.strip . T.split (==';') . cs
  singular        = hasPrefer "plurality=singular"
  representation
    | hasPrefer "return=representation" = Full
    | hasPrefer "return=minimal" = None
    | otherwise = HeadersOnly

-- PRIVATE ---------------------------------------------------------------

{-|
  Picks a preferred content type from an Accept header (or from
  Content-Type as a degenerate case).

  For example
  text/csv -> TextCSV
  */*      -> ApplicationJSON
  text/csv, application/json -> TextCSV
  application/json, text/csv -> ApplicationJSON
-}
pickContentType :: Maybe BS.ByteString -> Either BS.ByteString ContentType
pickContentType accept
  | isNothing accept || has ctAll || has ctJson = Right ApplicationJSON
  | has ctCsv = Right TextCSV
  | otherwise = Left accept'
 where
  ctAll  = "*/*"
  ctCsv  = "text/csv"
  ctJson = "application/json"
  Just accept' = accept
  findInAccept = flip find $ parseHttpAccept accept'
  has          = isJust . findInAccept . BS.isPrefixOf

type CsvData = V.Vector (M.HashMap T.Text BL.ByteString)

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
          else JSON.String $ cs str
      )

-- | Convert {foo} to [{foo}], leave arrays unchanged
-- and truncate everything else to an empty array.
pluralize :: JSON.Value -> JSON.Array
pluralize obj@(JSON.Object _) = V.singleton obj
pluralize (JSON.Array arr)    = arr
pluralize _                   = V.empty

-- | Test that Array contains only Objects having the same keys
-- and if so mark it as UniformObjects
ensureUniform :: JSON.Array -> Maybe UniformObjects
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
    then Just (UniformObjects objs)
    else Nothing
