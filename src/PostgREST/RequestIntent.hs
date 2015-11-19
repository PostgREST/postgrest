module PostgREST.RequestIntent where

import qualified Data.Aeson              as JSON
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Csv                as CSV
import           Data.List               (find)
import qualified Data.HashMap.Strict     as M
import           Data.Maybe              (fromMaybe, isJust, isNothing,
                                          listToMaybe)
import           Data.Monoid             ((<>))
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
            | ActionUnknown BS.ByteString
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
    iAction :: Action
  -- | Set to Nothing for malformed range
  , iRange  :: Maybe NonnegRange
  -- | Set to Nothing for strangely nested urls
  , iTarget :: Maybe Target
  -- | The content type the client most desires (or JSON if undecided)
  , iAccepts :: Either BS.ByteString ContentType
  -- | Data sent by client and used for mutation actions
  , iPayload :: Payload
  -- | Taken from JSON Web Token
  , iTrustedClaims :: Maybe JSON.Object
  -- | If client wants created items echoed back
  , iPreferRepresentation :: Bool
  -- | If client wants first row as raw object
  , iPreferSingular :: Bool
  }

-- | Examines HTTP request and translates it into user intent.
userIntent :: Schema -> Request -> RequestBody -> Intent
userIntent schema req reqBody =
  let action = case requestMethod req of
                 "GET"     -> ActionRead
                 "POST"    -> if isTargetingProc
                                then ActionInvoke
                                else ActionCreate
                 "PATCH"   -> ActionUpdate
                 "DELETE"  -> ActionDelete
                 "OPTIONS" -> ActionInfo
                 other     -> ActionUnknown other
      target = case path of
                 []            -> Just TargetRoot
                 [table]       -> Just $ TargetIdent
                                       $ QualifiedIdentifier schema table
                 ["rpc", proc] -> Just $ TargetIdent
                                       $ QualifiedIdentifier schema proc
                 _             -> Nothing
      reqPayload = case pickContentType (lookupHeader "content-type") of
                     Right ApplicationJSON ->
                       either (PayloadParseError . cs)
                         (PayloadJSON . pluralize)
                         (JSON.eitherDecode reqBody)
                     Right TextCSV ->
                       either (PayloadParseError . cs)
                         (PayloadJSON . csvToJson)
                         (CSV.decodeByName reqBody)
                     Left accept ->
                       PayloadParseError $
                         "Content-type not acceptable: " <> accept in

  Intent action
    (rangeRequested hdrs)
    target
    (pickContentType $ lookupHeader "accept")
    reqPayload
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
  | otherwise = Left accept'
 where
  ctAll  = "*/*"
  ctCsv  = "text/csv"
  ctJson = "application/json"
  Just accept' = accept
  findInAccept = flip find $ parseHttpAccept accept'
  has          = isJust . findInAccept . BS.isPrefixOf

type CsvData = V.Vector (M.HashMap T.Text BL.ByteString)

-- | Converts CSV like
-- a,b
-- 1,hi
-- 2,bye
--
-- into a JSON array like
-- [ {"a": "1", "b": "hi"}, {"a": 2, "b": "bye"} ]
--
-- The reason for its odd signature is so that it can compose
-- directly with CSV.decodeByName
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
