{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PostgREST.App (
  app
, sqlError
, isSqlError
, contentTypeForAccept
, jsonH
, TableOptions(..)
) where

import qualified Blaze.ByteString.Builder  as BB
import           Control.Applicative
import           Control.Arrow             (second, (***))
import           Control.Monad             (join)
import           Data.Bifunctor            (first)
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BL
import           Data.CaseInsensitive      (original)
import qualified Data.Csv                  as CSV
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as M
import           Data.List                 (find, sortBy)
import           Data.Maybe                (fromMaybe, isJust, isNothing,
                                            mapMaybe)
import           Data.Ord                  (comparing)
import           Data.Ranged.Ranges        (emptyRange)
import qualified Data.Set                  as S
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text, replace, strip)

import           Text.Parsec.Error

import           Network.HTTP.Base         (urlEncodeVars)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI    (parseSimpleQuery)
import           Network.Wai
import           Network.Wai.Internal      (Response (..))
import           Network.Wai.Parse         (parseHttpAccept)

import           Data.Aeson
import           Data.Monoid
import qualified Data.Vector               as V
import qualified Hasql                     as H
import qualified Hasql.Backend             as B
import qualified Hasql.Postgres            as P

import           PostgREST.Auth
import           PostgREST.Config          (AppConfig (..))
import           PostgREST.Parsers
import           PostgREST.PgQuery
import           PostgREST.PgStructure
import           PostgREST.QueryBuilder
import           PostgREST.RangeQuery
import           PostgREST.Types

import           Prelude

app :: DbStructure -> AppConfig -> BL.ByteString -> Request -> H.Tx P.Postgres s Response
app dbstructure conf reqBody req =
  case (path, verb) of

    ([], _) -> do
      Identity (dbrole :: Text) <- H.singleEx $ [H.stmt|SELECT current_user|]
      let body = encode $ filter (filterTableAcl dbrole) $ filter ((cs schema==).tableSchema) allTabs
      return $ responseLBS status200 [jsonH] $ cs body

    ([table], "OPTIONS") -> do
      let cols = filter (filterCol schema table) allCols
          pkeys = map pkName $ filter (filterPk schema table) allPrKeys
          body = encode (TableOptions cols pkeys)
      return $ responseLBS status200 [jsonH, allOrigins] $ cs body

    ([table], "GET") ->
      if range == Just emptyRange
      then return $ responseLBS status416 [] "HTTP Range error"
      else
        case queries of
          Left e -> return $ responseLBS status400 [("Content-Type", "application/json")] $ cs e
          Right (qs, cqs) -> do
            let qt = qualify table
                count = if hasPrefer "count=none"
                      then countNone
                      else cqs
                q = B.Stmt "select " V.empty True <>
                    parentheticT count
                    <> commaq <> (
                    bodyForAccept contentType qt -- TODO! when in csv mode, the first row (columns) is not correct when requesting sub tables
                    . limitT range
                    $ qs
                  )
            row <- H.maybeEx q
            let (tableTotal, queryTotal, body) = fromMaybe (Just (0::Int), 0::Int, Just "" :: Maybe Text) row
                to = from+queryTotal-1
                contentRange = contentRangeH from to tableTotal
                status = rangeStatus from to tableTotal
                canonical = urlEncodeVars
                  . sortBy (comparing fst)
                  . map (join (***) cs)
                  . parseSimpleQuery
                  $ rawQueryString req
            return $ responseLBS status
              [contentTypeH, contentRange,
                ("Content-Location",
                  "/" <> cs table <>
                    if Prelude.null canonical then "" else "?" <> cs canonical
                )
              ] (cs $ fromMaybe "[]" body)

        where
            from = fromMaybe 0 $ rangeOffset <$> range
            apiRequest = first formatParserError (parseGetRequest req)
                     >>= first formatRelationError . addRelations schema allRels Nothing
                     >>= addJoinConditions schema allCols
                     where
                       formatRelationError :: Text -> Text
                       formatRelationError e = cs $ encode $ object [
                         "mesage" .= ("could not find foreign keys between these entities"::String),
                         "details" .= e]
                       formatParserError :: ParseError -> Text
                       formatParserError e = cs $ encode $ object [
                         "message" .= message,
                         "details" .= details]
                         where
                           message = show (errorPos e)
                           details = strip $ replace "\n" " " $ cs
                             $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)

            query = requestToQuery schema <$> apiRequest
            countQuery = requestToCountQuery schema <$> apiRequest
            queries = (,) <$> query <*> countQuery

    ([table], "POST") -> do
      let qt = qualify table
          echoRequested = hasPrefer "return=representation"
          parsed :: Either String (V.Vector Text, V.Vector (V.Vector Value))
          parsed = if lookupHeader "Content-Type" == Just csvMT
            then do
              rows <- CSV.decode CSV.NoHeader reqBody
              if V.null rows then Left "CSV requires header"
                else Right (V.head rows, (V.map $ V.map $ parseCsvCell . cs) (V.tail rows))
            else eitherDecode reqBody >>= \val ->
              case val of
                Object obj -> Right .  second V.singleton .  V.unzip .  V.fromList $
                  M.toList obj
                _ -> Left "Expecting single JSON object or CSV rows"
      case parsed of
        Left err -> return $ responseLBS status400 [] $
          encode . object $ [("message", String $ "Failed to parse JSON payload. " <> cs err)]
        Right toBeInserted -> do
          rows :: [Identity Text] <- H.listEx $ uncurry (insertInto qt) toBeInserted
          let inserted :: [Object] = mapMaybe (decode . cs . runIdentity) rows
              pKeys = map pkName $ filter (filterPk schema table) allPrKeys
              responses = flip map inserted $ \obj -> do
                let primaries =
                      if Prelude.null pKeys
                        then obj
                        else M.filterWithKey (const . (`elem` pKeys)) obj
                let params = urlEncodeVars
                      $ map (\t -> (cs $ fst t, cs (paramFilter $ snd t)))
                      $ sortBy (comparing fst) $ M.toList primaries
                responseLBS status201
                  [ jsonH
                  , (hLocation, "/" <> cs table <> "?" <> cs params)
                  ] $ if echoRequested then encode obj else ""
          return $ multipart status201 responses

    (["rpc", proc], "POST") -> do
      let qi = QualifiedIdentifier schema (cs proc)
      exists <- doesProcExist schema proc
      if exists
        then do
          let call = B.Stmt "select " V.empty True <>
                asJson (callProc qi $ fromMaybe M.empty (decode reqBody))
          body :: Maybe (Identity Text) <- H.maybeEx call
          return $ responseLBS status200 [jsonH]
            (cs $ fromMaybe "[]" $ runIdentity <$> body)
        else return $ responseLBS status404 [] ""

      -- check that proc exists
      -- check that arg names are all specified
      -- select * from public.proc(a := "foo"::undefined) where whereT limit limitT

    ([table], "PUT") ->
      handleJsonObj reqBody $ \obj -> do
        let qt = qualify table
            pKeys = map pkName $ filter (filterPk schema table) allPrKeys
            specifiedKeys = map (cs . fst) qq
        if S.fromList pKeys /= S.fromList specifiedKeys
          then return $ responseLBS status405 []
            "You must speficy all and only primary keys as params"
          else do
            let tableCols = map (cs . colName) $ filter (filterCol schema table) allCols
                cols = map cs $ M.keys obj
            if S.fromList tableCols == S.fromList cols
              then do
                let vals = M.elems obj
                H.unitEx $ iffNotT
                  (whereT qt qq $ update qt cols vals)
                  (insertSelect qt cols vals)
                return $ responseLBS status204 [ jsonH ] ""

              else return $ if Prelude.null tableCols
                then responseLBS status404 [] ""
                else responseLBS status400 []
                  "You must specify all columns in PUT request"

    ([table], "PATCH") ->
      handleJsonObj reqBody $ \obj -> do
        let qt = qualify table
            up = returningStarT
              . whereT qt qq
              $ update qt (map cs $ M.keys obj) (M.elems obj)
            patch = withT up "t" $ B.Stmt
              "select count(t), array_to_json(array_agg(row_to_json(t)))::character varying"
              V.empty True

        row <- H.maybeEx patch
        let (queryTotal, body) =
              fromMaybe (0 :: Int, Just "" :: Maybe Text) row
            r = contentRangeH 0 (queryTotal-1) (Just queryTotal)
            echoRequested = hasPrefer "return=representation"
            s = case () of _ | queryTotal == 0 -> status404
                             | echoRequested -> status200
                             | otherwise -> status204
        return $ responseLBS s [ jsonH, r ] $ if echoRequested then cs $ fromMaybe "[]" body else ""

    ([table], "DELETE") -> do
      let qt = qualify table
          del = countT
            . returningStarT
            . whereT qt qq
            $ deleteFrom qt
      row <- H.maybeEx del
      let (Identity deletedCount) = fromMaybe (Identity 0 :: Identity Int) row
      return $ if deletedCount == 0
        then responseLBS status404 [] ""
        else responseLBS status204 [("Content-Range", "*/"<> cs (show deletedCount))] ""

    (_, _) ->
      return $ responseLBS status404 [] ""

  where
    allTabs = tables dbstructure
    allRels = relations dbstructure
    allCols = columns dbstructure
    allPrKeys = primaryKeys dbstructure
    filterCol sc table (Column{colSchema=s, colTable=t}) =  s==sc && table==t
    filterCol _ _ _ =  False
    filterPk sc table pk = sc == pkSchema pk && table == pkTable pk

    filterTableAcl :: Text -> Table -> Bool
    filterTableAcl r (Table{tableAcl=a}) = r `elem` a
    path          = pathInfo req
    verb          = requestMethod req
    qq            = queryString req
    qualify       = QualifiedIdentifier schema
    hdrs          = requestHeaders req
    lookupHeader  = flip lookup hdrs
    hasPrefer val = any (\(h,v) -> h == "Prefer" && v == val) hdrs
    accept        = lookupHeader hAccept
    schema        = cs $ configSchema conf
    jwtSecret     = (cs $ configJwtSecret conf) :: Text
    range         = rangeRequested hdrs
    allOrigins    = ("Access-Control-Allow-Origin", "*") :: Header
    contentType   = fromMaybe "application/json" $ contentTypeForAccept accept
    contentTypeH  = (hContentType, contentType)

sqlError :: t
sqlError = undefined

isSqlError :: t
isSqlError = undefined

rangeStatus :: Int -> Int -> Maybe Int -> Status
rangeStatus _ _ Nothing = status200
rangeStatus from to (Just total)
  | from > total            = status416
  | (1 + to - from) < total = status206
  | otherwise               = status200

contentRangeH :: Int -> Int -> Maybe Int -> Header
contentRangeH from to total =
    ("Content-Range", cs headerValue)
    where
      headerValue   = rangeString <> "/" <> totalString
      rangeString
        | totalNotZero && fromInRange = show from <> "-" <> cs (show to)
        | otherwise = "*"
      totalString   = fromMaybe "*" (show <$> total)
      totalNotZero  = fromMaybe True ((/=) 0 <$> total)
      fromInRange   = from <= to

jsonMT :: BS.ByteString
jsonMT = "application/json"

csvMT :: BS.ByteString
csvMT = "text/csv"

allMT :: BS.ByteString
allMT = "*/*"

jsonH :: Header
jsonH = (hContentType, jsonMT)

contentTypeForAccept :: Maybe BS.ByteString -> Maybe BS.ByteString
contentTypeForAccept accept
  | isNothing accept || has allMT || has jsonMT = Just jsonMT
  | has csvMT = Just csvMT
  | otherwise = Nothing
  where
    Just acceptH = accept
    findInAccept = flip find $ parseHttpAccept acceptH
    has          = isJust . findInAccept . BS.isPrefixOf

bodyForAccept :: BS.ByteString -> QualifiedIdentifier  -> StatementT
bodyForAccept contentType table
  | contentType == csvMT = asCsvWithCount table
  | otherwise = asJsonWithCount -- defaults to JSON

handleJsonObj :: BL.ByteString -> (Object -> H.Tx P.Postgres s Response)
              -> H.Tx P.Postgres s Response
handleJsonObj reqBody handler = do
  let p = eitherDecode reqBody
  case p of
    Left err ->
      return $ responseLBS status400 [jsonH] jErr
      where
        jErr = encode . object $
          [("message", String $ "Failed to parse JSON payload. " <> cs err)]
    Right (Object o) -> handler o
    Right _ ->
      return $ responseLBS status400 [jsonH] jErr
      where
        jErr = encode . object $
          [("message", String "Expecting a JSON object")]

parseCsvCell :: BL.ByteString -> Value
parseCsvCell s = if s == "NULL" then Null else String $ cs s

multipart :: Status -> [Response] -> Response
multipart _ [] = responseLBS status204 [] ""
multipart _ [r] = r
multipart s rs =
  responseLBS s [(hContentType, "multipart/mixed; boundary=\"postgrest_boundary\"")] $
    BL.intercalate "\n--postgrest_boundary\n" (map renderResponseBody rs)

  where
    renderHeader :: Header -> BL.ByteString
    renderHeader (k, v) = cs (original k) <> ": " <> cs v

    renderResponseBody :: Response -> BL.ByteString
    renderResponseBody (ResponseBuilder _ headers b) =
      BL.intercalate "\n" (map renderHeader headers)
        <> "\n\n" <> BB.toLazyByteString b
    renderResponseBody _ = error
      "Unable to create multipart response from non-ResponseBuilder"

data TableOptions = TableOptions {
  tblOptcolumns :: [Column]
, tblOptpkey    :: [Text]
}

instance ToJSON TableOptions where
  toJSON t = object [
      "columns" .= tblOptcolumns t
    , "pkey"   .= tblOptpkey t ]
