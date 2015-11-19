module PostgREST.RequestIntent where

import qualified Data.Aeson              as JSON
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Csv                as CSV
import           Data.List               (find)
import qualified Data.HashMap.Strict     as M
import           Data.Maybe              (fromMaybe, isJust, isNothing,
                                          listToMaybe)
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           Network.Wai             (Request (..))
import           Network.Wai.Parse       (parseHttpAccept)
import           PostgREST.RangeQuery    (NonnegRange, rangeRequested)
import           PostgREST.Types         (QualifiedIdentifier (..), Schema)

type RequestBody = BL.ByteString

-- | Types of things a user wants to do to tables/views/procs
data Action = ActionCreate | ActionRead
            | ActionUpdate | ActionDelete
            | ActionInfo   | ActionInvoke
-- | The target db object of a user action
data Target = TargetIdent QualifiedIdentifier
            | TargetRoot
-- | Enumeration of currently supported content types for
-- route responses and upload payloads
data ContentType = ApplicationJSON | TextCSV
-- | When Hasql supports the COPY command then we can
-- have a special payload just for CSV, but until
-- then CSV is converted to a JSON array.
data Payload = PayloadJSON JSON.Array
             | PayloadParseError BS.ByteString

-- | Describes what the user wants to do. This data type is a
-- translation of the raw elements of an HTTP request into domain
-- specific language.  There is no guarantee that the intent is
-- sensible, it is up to a later stage of processing to determine
-- if it is an action we are able to perform.
data Intent = Intent {
  -- | Set to Nothing for unknown HTTP verbs
    iAction :: Maybe Action
  -- | Set to Nothing for malformed range
  , iRange  :: Maybe NonnegRange
  -- | Set to Nothing for strangely nested urls
  , iTarget :: Maybe Target
  -- | The content type the client most desires (or JSON if undecided)
  , iAccepts :: Either BS.ByteString ContentType
  -- | Set to Nothing when client sends no data
  , iPayload :: Maybe Payload
  -- | Taken from JSON Web Token
  , iTrustedClaims :: Maybe JSON.Object
  -- | If client wants created items echoed back
  , iPreferRepresentation :: Bool
  -- | If client wants first row as raw object
  , iPreferSingular :: Bool
  }

-- | Examines HTTP request and translates it into user intent.
userIntent :: Schema -> Request -> RequestBody -> Intent
userIntent schema req _ =
  let action = case requestMethod req of
                 "GET"     -> Just ActionRead
                 "POST"    -> Just $ if isTargetingProc
                                then ActionInvoke
                                else ActionCreate
                 "PATCH"   -> Just ActionUpdate
                 "DELETE"  -> Just ActionDelete
                 "OPTIONS" -> Just ActionInfo
                 _         -> Nothing
      target = case path of
                 []            -> Just TargetRoot
                 [table]       -> Just $ TargetIdent
                                       $ QualifiedIdentifier schema table
                 ["rpc", proc] -> Just $ TargetIdent
                                       $ QualifiedIdentifier schema proc
                 _             -> Nothing  in

  Intent action
    (rangeRequested hdrs)
    target
    (pickContentType $ lookupHeader "accept")
    Nothing -- TODO: calculate payload
    Nothing -- TODO: decode jwt
    (hasPrefer "return=representation")
    (hasPrefer "plurality=singular")

 where
  path            = pathInfo req
  isTargetingProc = fromMaybe False $ (== "rpc") <$> listToMaybe path
  hdrs            = requestHeaders req
  lookupHeader    = flip lookup hdrs
  hasPrefer val   = any (\(h,v) -> h == "Prefer" && v == val) hdrs

-- PRIVATE ---------------------------------------------------------------

-- | Chooses a payload from the items in an accept header.
-- When possible it picks JSON.
pickContentType :: Maybe BS.ByteString -> Either BS.ByteString ContentType
pickContentType accept
  | isNothing accept || has ctAll || has ctJson = Right ApplicationJSON
  | has ctCsv = Right TextCSV
  | otherwise = Left acceptH
 where
  ctAll  = "*/*"
  ctCsv  = "text/csv"
  ctJson = "application/json"
  Just acceptH = accept
  findInAccept = flip find $ parseHttpAccept acceptH
  has          = isJust . findInAccept . BS.isPrefixOf

type CsvData = V.Vector (V.Vector BL.ByteString)

-- | Convert
-- a,b
-- 1,2
-- 3,4
--
-- into
-- [ {"a": 1, "b": 2}, {"a": 3, "b": 4} ]
csvToJson :: (CSV.Header, CsvData) -> JSON.Array
csvToJson (cols, vals) =
  V.map rowToJson vals
 where
  cols' = V.map cs cols :: V.Vector T.Text
  rowToJson :: V.Vector BL.ByteString -> JSON.Value
  rowToJson val = JSON.Object $
    let val' = V.map (JSON.String . cs) val in
    M.fromList . V.toList $ V.zip cols' val'
