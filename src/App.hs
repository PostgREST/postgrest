module App where

-- import Types (SqlRow, getRow)

import Control.Monad (join)
import Data.Monoid ( (<>) )
import Control.Arrow ((***))
import Control.Applicative
-- import Options.Applicative hiding (columns)

import Data.Text hiding (map)
import Data.Maybe (listToMaybe, fromMaybe)
import Text.Regex.TDFA ((=~))
import Data.Ord (comparing)
-- import Data.Map (intersection, fromList, toList, Map)
import Data.List (sortBy)
-- import qualified Data.Set as S
-- import Data.Convertible.Base (convert)
-- import Data.Text (strip, Text)

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI (parseSimpleQuery)

import Network.HTTP.Base (urlEncodeVars)

import Network.Wai
-- import Network.Wai.Internal
-- import Network.Wai.Middleware.Cors (CorsResourcePolicy(..))

import Data.ByteString.Char8 hiding (zip, map)
import Data.String.Conversions (cs)
-- import qualified Data.CaseInsensitive as CI

-- import PgStructure (printTables, printColumns, primaryKeyColumns,
--                     columns, Column(colName))

import Data.Aeson
import Database.PostgreSQL.Simple

import PgQuery
import RangeQuery
import PgStructure
import Data.Ranged.Ranges (emptyRange)

app :: Connection -> Application
app conn req respond =
  respond =<< case (path, verb) of
    ([], _) -> do
      body <- encode <$> tables conn (cs schema)
      return $ responseLBS status200 [jsonH] $ cs body

    ([table], "OPTIONS") -> do
      let t = QualifiedTable schema (cs table)
      cols <- columns conn t
      pkey <- map cs <$> primaryKeyColumns conn t
      return $ responseLBS status200 [jsonH, allOrigins]
        $ encode (TableOptions cols pkey)

    ([table], "GET") ->
      if range == Just emptyRange
      then return $ responseLBS status416 [] "HTTP Range error"
      else do
        let qt = QualifiedTable schema (cs table)
        let select =
              ("select ",[]) <>
                  parentheticT (
                    whereT qq $ countRows qt
                  ) <> commaq <> (
                  asJsonWithCount
                  . limitT range
                  . orderT (orderParse qq)
                  . whereT qq
                  $ selectStar qt
                )

        row <- listToMaybe <$> uncurry (query conn) select
        let (tableTotal, queryTotal, body) =
              fromMaybe (0, 0, "" :: ByteString) row
            from = fromMaybe 0 $ rangeOffset <$> range
            to = from+queryTotal
            contentRange = contentRangeH from to tableTotal
            status = rangeStatus from to tableTotal
            canonical = urlEncodeVars
                          . sortBy (comparing fst)
                          . map (join (***) cs)
                          . parseSimpleQuery
                          $ rawQueryString req

        return $ responseLBS status
          [jsonH, contentRange,
            ("Content-Location",
             "/" <> cs table <> if Prelude.null canonical then "" else "?" <> cs canonical
            )
          ] (cs body)

    (_, _) ->
      return $ responseLBS status404 [] ""

  where
    path   = pathInfo req
    verb   = requestMethod req
    qq     = queryString req
    hdrs   = requestHeaders req
    schema = requestedSchema hdrs
    range  = rangeRequested hdrs
    allOrigins = ("Access-Control-Allow-Origin", "*") :: Header


rangeStatus :: Int -> Int -> Int -> Status
rangeStatus from to total
  | from > total            = status416
  | (1 + to - from) < total = status206
  | otherwise               = status200

contentRangeH :: Int -> Int -> Int -> Header
contentRangeH from to total =
  ("Content-Range",
    if total == 0 || from > total
    then "*/" <> cs (show total)
    else cs (show from)  <> "-"
       <> cs (show to)    <> "/"
       <> cs (show total)
  )

requestedSchema :: RequestHeaders -> ByteString
requestedSchema hdrs =
  case verStr of
       Just [[_, ver]] -> ver
       _ -> "1"

  where verRegex = "version[ ]*=[ ]*([0-9]+)" :: String
        accept = lookup hAccept hdrs :: Maybe ByteString
        verStr = (=~ verRegex) <$> accept :: Maybe [[ByteString]]

parsePayload :: FromJSON j => Request -> IO (Either String j)
parsePayload = fmap eitherDecode . strictRequestBody

jsonH :: Header
jsonH = (hContentType, "application/json")


data TableOptions = TableOptions {
  tblOptcolumns :: [Column]
, tblOptpkey :: [Text]
}

instance ToJSON TableOptions where
  toJSON t = object [
      "columns" .= tblOptcolumns t
    , "pkey"   .= tblOptpkey t ]

-- jsonBodyAction :: Request -> (SqlRow -> IO Response) -> IO Response
-- jsonBodyAction req handler = do
--   parse <- jsonBody req
--   case parse of
--     Left err -> return $ responseLBS status400 [jsonContentType] json
--       where json = JSON.encode . JSON.object $ [("error", JSON.String $ "Failed to parse JSON payload. " <> cs err) ]
--     Right body -> handler body


-- filterByKeys :: Ord a => Map a b -> [a] -> Map a b
-- filterByKeys m keys =
--   if null keys then m else
--     m `intersection` fromList (zip keys $ repeat undefined)

-- app :: Connection -> Application
-- app conn req respond =
--   respond =<< case (path, verb) of
--     ([], _) ->
--       responseLBS status200 [jsonContentType] <$> printTables ver conn

--     (["dbapi", "users"], "POST") -> do
--       body <- strictRequestBody req
--       let parse = JSON.eitherDecode body

--       case parse of
--         Left err -> return $ responseLBS status400 [jsonContentType] json
--           where json = JSON.encode . JSON.object $ [("error", JSON.String $ "Failed to parse JSON payload. " <> cs err) ]
--         Right u -> do
--           addUser (cs $ userId u) (cs $ userPass u) (cs $ userRole u) conn
--           return $ responseLBS status201
--             [ jsonContentType
--             , (hLocation, "/dbapi/users?id=eq." <> cs (userId u))
--             ] ""

--     ([table], "OPTIONS") ->
--       responseLBS status200 [jsonContentType, allOrigins] <$>
--         printColumns ver (cs table) conn

--     ([table], "GET") ->
--       if range == Just emptyRange
--       then return $ responseLBS status416 [] "HTTP Range error"
--       else do
--         r <- respondWithRangedResult <$> getRows ver (cs table) qq range conn
--         let canonical = urlEncodeVars $ sort $
--                         map (join (***) cs) $
--                         parseSimpleQuery $
--                         rawQueryString req
--         return $ addHeaders [
--           ("Content-Location",
--            "/" <> cs table <> if null canonical then "" else "?" <> cs canonical
--           )] r

--     ([table], "POST") ->
--       jsonBodyAction req (\row -> do
--         allvals <- insert ver table row conn
--         keys <- map cs <$> primaryKeyColumns ver (cs table) conn
--         let params = urlEncodeVars $ map (\t -> (fst t, "eq." <> convert (snd t) :: String)) $ toList $ filterByKeys allvals keys
--         return $ responseLBS status201
--           [ jsonContentType
--           , (hLocation, "/" <> cs table <> "?" <> cs params)
--           ] ""
--       )

--     ([table], "PUT") ->
--       jsonBodyAction req (\row -> do
--         keys <- primaryKeyColumns ver (cs table) conn
--         let specifiedKeys = map (cs . fst) qq
--         if S.fromList keys /= S.fromList specifiedKeys
--           then return $ responseLBS status405 []
--                "You must speficy all and only primary keys as params"
--           else
--             if isJust cRange
--                then return $ responseLBS status400 []
--                     "Content-Range is not allowed in PUT request"
--             else do
--               cols <- columns ver (cs table) conn
--               let colNames = S.fromList $ map (cs . colName) cols
--               let specifiedCols = S.fromList $ map fst $ getRow row
--               if colNames == specifiedCols then do
--                 _ <- upsert ver table row qq conn
--                 return $ responseLBS status204 [ jsonContentType ] ""

--                 else return $ if S.null colNames then responseLBS status404 [] ""
--                   else responseLBS status400 []
--                      "You must specify all columns in PUT request"
--       )

--     ([table], "PATCH") ->
--       jsonBodyAction req (\row -> do
--         _ <- update ver table row qq conn
--         return $ responseLBS status204 [ jsonContentType ] ""
--       )

--     (_, _) ->
--       return $ responseLBS status404 [] ""

--   where
--     path   = pathInfo req
--     verb   = requestMethod req
--     qq     = queryString req
--     hdrs   = requestHeaders req
--     ver    = fromMaybe "1" $ requestedVersion hdrs
--     range  = requestedRange hdrs
--     cRange = requestedContentRange hdrs
--     allOrigins = ("Access-Control-Allow-Origin", "*") :: Header

-- defaultCorsPolicy :: CorsResourcePolicy
-- defaultCorsPolicy =  CorsResourcePolicy Nothing
--   ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"] ["Authorization"] Nothing
--   (Just $ 60*60*24) False False True

-- corsPolicy :: Request -> Maybe CorsResourcePolicy
-- corsPolicy req = case lookup "origin" headers of
--   Just origin -> Just defaultCorsPolicy {
--       corsOrigins = Just ([origin], True)
--     , corsRequestHeaders = "Authentication":accHeaders
--     }
--   Nothing -> Nothing
--   where
--     headers = requestHeaders req
--     accHeaders = case lookup "access-control-request-headers" headers of
--       Just hdrs -> map (CI.mk . cs . strip . cs) $ BS.split ',' hdrs
--       Nothing -> []


-- respondWithRangedResult :: RangedResult -> Response
-- respondWithRangedResult rr =
--   responseLBS status [
--     jsonContentType,
--     ("Content-Range",
--       if total == 0 || from > total
--       then "*/" <> cs (show total)
--       else cs (show from)  <> "-"
--          <> cs (show to)    <> "/"
--          <> cs (show total)
--     )
--   ] (rrBody rr)

--   where
--     from   = rrFrom rr
--     to     = rrTo   rr
--     total  = rrTotal rr
--     status
--       | from > total            = status416
--       | (1 + to - from) < total = status206
--       | otherwise               = status200


-- addHeaders :: ResponseHeaders -> Response -> Response
-- addHeaders hdrs (ResponseFile    s headers fp m) =
--                  ResponseFile    s (headers ++ hdrs) fp m
-- addHeaders hdrs (ResponseBuilder s headers b)    =
--                  ResponseBuilder s (headers ++ hdrs) b
-- addHeaders hdrs (ResponseStream  s headers b)    =
--                  ResponseStream  s (headers ++ hdrs) b
-- addHeaders hdrs (ResponseRaw     s resp)         =
--                  ResponseRaw     s (addHeaders hdrs resp)
