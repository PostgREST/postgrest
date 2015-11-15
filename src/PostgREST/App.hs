{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
--module PostgREST.App where
module PostgREST.App (
  app
, contentTypeForAccept
) where

import           Control.Applicative
import           Control.Arrow             ((***))
import           Control.Monad             (join)
import           Data.Bifunctor            (first)
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Csv                  as CSV
import           Data.Functor.Identity
import qualified Data.HashMap.Strict       as HM
import           Data.List                 (find, sortBy, delete, transpose)
import           Data.Maybe                (fromMaybe, fromJust, isJust, isNothing, mapMaybe)
import           Data.Ord                  (comparing)
import           Data.Ranged.Ranges        (emptyRange, singletonRange)
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text, replace, strip)
import qualified Data.Text                 as T
import           Data.Tree
import qualified Data.Map                  as M

import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec (parse)

import           Network.HTTP.Base         (urlEncodeVars)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI    (parseSimpleQuery)
import           Network.Wai
import           Network.Wai.Parse         (parseHttpAccept)

import           Data.Aeson
import           Data.Aeson.Types          (emptyArray)
import           Data.Monoid
import qualified Data.Vector               as V
import qualified Hasql                     as H
import qualified Hasql.Backend             as B
import qualified Hasql.Postgres            as P

import           PostgREST.Config          (AppConfig (..))
import           PostgREST.Parsers
import           PostgREST.DbStructure
import           PostgREST.QueryBuilder ( asJson
                                        , callProc
                                        , asCsvF
                                        , asJsonF
                                        , selectStarF
                                        , countF
                                        , locationF
                                        , asJsonSingleF
                                        , addJoinConditions
                                        , sourceSubqueryName
                                        , requestToQuery
                                        , wrapQuery
                                        , countAllF
                                        , countNoneF
                                        , addRelations
                                        )
import           PostgREST.RangeQuery
import           PostgREST.Types
import           PostgREST.Auth            (tokenJWT)
import           PostgREST.Error           (errResponse)
import           PostgREST.Router

import           Prelude

app :: DbStructure -> AppConfig -> BL.ByteString -> Request -> H.Tx P.Postgres s Response
app dbStructure conf reqBody req =
  case route RootE dbStructure schema path of
    Just endpoint ->
      case endpoint of
        RootE ->
          case verb of
            "GET" -> do
              body <- encode <$> accessibleTables (filter ((== cs schema) . tableSchema) allTabs)
              return $ responseLBS status200 [jsonH] $ cs body

            _ -> notFound

        TableE table ->
          case verb of
            "GET" ->
              if range == Just emptyRange
                then return $ errResponse status416 "HTTP Range error"
                else
                  case request of
                    Left e -> requestErr e
                    Right (selectQuery, _, _) -> do
                      let q = B.Stmt (createReadStatement selectQuery range False (not $ hasPrefer "count=none") isCsv) V.empty True
                      row <- H.maybeEx q
                      let (tableTotal, queryTotal, _ , body) = extractQueryResult row
                          to = frm+queryTotal-1
                          contentRange = contentRangeH frm to tableTotal
                          status = rangeStatus frm to tableTotal
                          canonical = urlEncodeVars -- should this be moved to the dbStructure (location)?
                            . sortBy (comparing fst)
                            . map (join (***) cs)
                            . parseSimpleQuery
                            $ rawQueryString req
                      return $ responseLBS status
                        [contentTypeH, contentRange,
                          ("Content-Location",
                            "/" <> cs (tableName table) <>
                              if Prelude.null canonical then "" else "?" <> cs canonical
                          )
                        ] (fromMaybe "[]" body)
                where
                  frm = fromMaybe 0 $ rangeOffset <$> range

            "POST" ->
              case request of
                Left e -> requestErr e
                Right (selectQuery, mutateQuery, isSingle) -> do
                  row <- H.maybeEx $ B.Stmt (createWriteStatement selectQuery mutateQuery isSingle echoRequested pKeys isCsv) V.empty True
                  let (_, _, location, body) = extractQueryResult row
                      contentLocation = cs fullPath <> "?" <> cs (fromMaybe "" location)
                  return $ responseLBS status201
                    [contentTypeH, (hLocation, contentLocation)]
                    $ if echoRequested then fromMaybe "[]" body else ""

            "PATCH" ->
              case request of
                Left e -> requestErr e
                Right (selectQuery, mutateQuery, _) -> do
                  let q = B.Stmt (createWriteStatement selectQuery mutateQuery False echoRequested [] isCsv) V.empty True
                  row <- H.maybeEx q
                  let (_, queryTotal, _, body) = extractQueryResult row
                      r = contentRangeH 0 (queryTotal-1) (Just queryTotal)
                      s = case () of _ | queryTotal == 0 -> status404
                                       | echoRequested -> status200
                                       | otherwise -> status204
                  return $ responseLBS s [contentTypeH, r]
                    $ if echoRequested then fromMaybe "[]" body else ""

            "DELETE" ->
              case request of
                Left e -> requestErr e
                Right (selectQuery, mutateQuery, _) -> do
                  let q = B.Stmt (createWriteStatement selectQuery mutateQuery False False [] isCsv) V.empty True
                  row <- H.maybeEx q
                  let (_, queryTotal, _, _) = extractQueryResult row
                  return $ if queryTotal == 0
                    then responseLBS status404 [] ""
                    else responseLBS status204 [("Content-Range", "*/" <> cs (show queryTotal))] ""

            "OPTIONS" -> do
              let cols = filter (filterCol schema tableN) allCols
                  body = encode (TableOptions cols pKeys)
              return $ responseLBS status200 [jsonH, allOrigins] $ cs body

            _ -> notFound

          where
            tableN = tableName table
            request = parseRequest schema allRels tableN req [] reqBody
            range = rangeRequested hdrs
            pKeys = map pkName $ dbFindPrimaryKeys dbStructure table

        RowE (Locus table filters) ->
          case verb of
            "GET" ->
              case request of
                Left e -> requestErr e
                Right (selectQuery, _, _) -> do
                  let q = B.Stmt (createReadStatement selectQuery Nothing True False isCsv) V.empty True
                  row <- H.maybeEx q
                  let (_, queryTotal, _, body) = extractQueryResult row
                  if queryTotal <= 0
                    then notFound
                    else return $ responseLBS status200 [contentTypeH] (fromMaybe "{}" body)

            "PATCH" ->
              case request of
                Left e -> requestErr e
                Right (selectQuery, mutateQuery, _) -> do
                  let q = B.Stmt (createWriteStatement selectQuery mutateQuery True echoRequested [] isCsv) V.empty True
                  row <- H.maybeEx q
                  let (_, queryTotal, _, body) = extractQueryResult row
                      (s,b) = if echoRequested
                        then (status200, fromMaybe "{}" body)
                        else (status204, "")
                  if queryTotal <= 0
                    then notFound
                    else return $ responseLBS s [contentTypeH] b

            "DELETE" ->
              case request of
                Left e -> requestErr e
                Right (selectQuery, mutateQuery, _) -> do
                  let q = B.Stmt (createWriteStatement selectQuery mutateQuery True False [] isCsv) V.empty True
                  row <- H.maybeEx q
                  let (_, queryTotal, _, _) = extractQueryResult row
                  if queryTotal <= 0
                    then notFound
                    else return $ responseLBS status204 [] ""

            _ -> notFound

            where
              tableN = tableName table
              request = parseRequest schema allRels tableN req filters reqBody

        ProcedureE proc ->
          case verb of
            "POST" -> do
              let qi = QualifiedIdentifier schema (cs proc)
              exists <- doesProcExist schema proc
              if exists
                then do
                  let call = B.Stmt "select " V.empty True <>
                        asJson (callProc qi $ fromMaybe HM.empty (decode reqBody))
                  bodyJson :: Maybe (Identity Value) <- H.maybeEx call
                  returnJWT <- doesProcReturnJWT schema proc
                  return $ responseLBS status200 [jsonH]
                         (let body = fromMaybe emptyArray $ runIdentity <$> bodyJson in
                            if returnJWT
                            then "{\"token\":\"" <> cs (tokenJWT jwtSecret body) <> "\"}"
                            else cs $ encode body)
                else return $ responseLBS status404 [] ""

            _ -> notFound

    _ -> notFound

  where
    notFound = return $ responseLBS status404 [] ""
    requestErr e = return $ errResponse status400 $ cs e
    allTabs  = dbTables dbStructure
    allRels  = dbRelations dbStructure
    allCols  = dbColumns dbStructure
    filterCol sc table (Column{colTable=Table{tableSchema=s,tableName=t}}) =  s==sc && table==t
    filterCol _ _ _ =  False
    path          = pathInfo req
    verb          = requestMethod req
    fullPath      = T.cons '/' (T.intercalate (T.singleton '/') path)
    hdrs          = requestHeaders req
    lookupHeader  = flip lookup hdrs
    hasPrefer val = any (\(h,v) -> h == "Prefer" && v == val) hdrs
    accept        = lookupHeader hAccept
    schema        = cs $ configSchema conf
    jwtSecret     = (cs $ configJwtSecret conf) :: Text
    allOrigins    = ("Access-Control-Allow-Origin", "*") :: Header
    contentType   = fromMaybe "application/json" $ contentTypeForAccept accept
    isCsv         = contentType == csvMT
    contentTypeH  = (hContentType, contentType)
    echoRequested = hasPrefer "return=representation"

rangeStatus :: Int -> Int -> Maybe Int -> Status
rangeStatus _ _ Nothing = status200
rangeStatus frm to (Just total)
  | frm > total            = status416
  | (1 + to - frm) < total = status206
  | otherwise               = status200

contentRangeH :: Int -> Int -> Maybe Int -> Header
contentRangeH frm to total =
    ("Content-Range", cs headerValue)
    where
      headerValue   = rangeString <> "/" <> totalString
      rangeString
        | totalNotZero && fromInRange = show frm <> "-" <> cs (show to)
        | otherwise = "*"
      totalString   = fromMaybe "*" (show <$> total)
      totalNotZero  = fromMaybe True ((/=) 0 <$> total)
      fromInRange   = frm <= to

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

parseCsvCell :: BL.ByteString -> Value
parseCsvCell s = if s == "NULL" then Null else String $ cs s

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

parseRequestBody :: Bool -> BL.ByteString -> Either Text ([Text],[[Value]])
parseRequestBody isCsv reqBody = first cs $
  checkStructure =<<
  if isCsv
  then do
    rows <- (map V.toList . V.toList) <$> CSV.decode CSV.NoHeader reqBody
    if null rows then Left "CSV requires header" -- TODO! should check if length rows > 1 (header and 1 row)
      else Right (head rows, (map $ map $ parseCsvCell . cs) (tail rows))
  else eitherDecode reqBody >>= convertJson
  where
    checkStructure :: ([Text], [[Value]]) -> Either String ([Text], [[Value]])
    checkStructure v
      | headerMatchesContent v = Right v
      | isCsv = Left "CSV header does not match rows length"
      | otherwise = Left "The number of keys in objects do not match"

    headerMatchesContent :: ([Text], [[Value]]) -> Bool
    headerMatchesContent (header, vals) = all ( (headerLength ==) . length) vals
      where headerLength = length header

convertJson :: Value -> Either String ([Text],[[Value]])
convertJson v = (,) <$> (header <$> normalized) <*> (vals <$> normalized)
  where
    invalidMsg = "Expecting single JSON object or JSON array of objects"
    normalized :: Either String [(Text, [Value])]
    normalized = groupByKey =<< normalizeValue v

    vals :: [(Text, [Value])] -> [[Value]]
    vals = transpose . map snd

    header :: [(Text, [Value])] -> [Text]
    header = map fst

    groupByKey :: Value -> Either String [(Text,[Value])]
    groupByKey (Array a) = HM.toList . foldr (HM.unionWith (++)) (HM.fromList []) <$> maps
      where
        maps :: Either String [HM.HashMap Text [Value]]
        maps = mapM getElems $ V.toList a
        getElems (Object o) = Right $ HM.map (:[]) o
        getElems _ = Left invalidMsg
    groupByKey _ = Left invalidMsg

    normalizeValue :: Value -> Either String Value
    normalizeValue val =
      case val of
        Object obj  -> Right $ Array (V.fromList[Object obj])
        a@(Array _) -> Right a
        _ -> Left invalidMsg

augumentRequestWithJoin :: Schema ->  [Relation] ->  ApiRequest -> Either Text ApiRequest
augumentRequestWithJoin schema allRels request =
  (first formatRelationError . addRelations schema allRels Nothing) request
  >>= addJoinConditions schema

-- we use strings here because most of this data will be sent to parsers (which need strings for now)
queryParams :: Request -> [(String, Maybe String)]
queryParams httpRequest = [(cs k, cs <$> v)|(k,v) <- queryString httpRequest]

selectStr :: [(String, Maybe String)] -> String
selectStr qParams = fromMaybe "*" $ fromMaybe (Just "*") $ lookup "select" qParams

whereFilters :: [(String, Maybe String)] -> [(String, String)]
whereFilters qParams = [ (k, fromJust v) | (k,v) <- qParams, k `notElem` ["select", "order"], isJust v ]

orderStr :: [(String, Maybe String)] -> Maybe String
orderStr qParams = join $ lookup "order" qParams

buildSelectApiRequest :: Text -> String -> [(String, String)] -> Maybe String -> Either Text ApiRequest
buildSelectApiRequest rootTableName sel wher orderS =
  first formatParserError $ foldr addFilter <$> (addOrder <$> apiRequest <*> ord) <*> flts
  where
    apiRequest = parse (pRequestSelect rootTableName) ("failed to parse select parameter <<"++sel++">>") sel
    addOrder (Node (q,i) f) o = Node (q{order=o}, i) f
    flts = mapM pRequestFilter wher
    ord = traverse (parse pOrder ("failed to parse order parameter <<"++fromMaybe "" orderS++">>")) orderS

addFilter :: (Path, Filter) -> ApiRequest -> ApiRequest
addFilter ([], flt) (Node (q@(Select {where_=flts}), i) forest) = Node (q {where_=flt:flts}, i) forest
addFilter (path, flt) (Node rn forest) =
  case targetNode of
    Nothing -> Node rn forest -- the filter is silenty dropped in the Request does not contain the required path
    Just tn -> Node rn (addFilter (remainingPath, flt) tn:restForest)
  where
    targetNodeName:remainingPath = path
    (targetNode,restForest) = splitForest targetNodeName forest
    splitForest name forst =
      case maybeNode of
        Nothing -> (Nothing,forest)
        Just node -> (Just node, delete node forest)
      where maybeNode = find ((name==).fst.snd.rootLabel) forst

toSourceRelation :: Text -> Relation -> Maybe Relation
toSourceRelation mt r@(Relation t _ ft _ _ rt _ _)
  | mt == tableName t = Just $ r {relTable=t {tableName=sourceSubqueryName}}
  | mt == tableName ft = Just $ r {relFTable=t {tableName=sourceSubqueryName}}
  | Just mt == (tableName <$> rt) = Just $ r {relLTable=(\tbl -> tbl {tableName=sourceSubqueryName}) <$> rt}
  | otherwise = Nothing

data TableOptions = TableOptions {
  tblOptcolumns :: [Column]
, tblOptpkey    :: [Text]
}

instance ToJSON TableOptions where
  toJSON t = object [
      "columns" .= tblOptcolumns t
    , "pkey"   .= tblOptpkey t ]

parseRequest :: Schema -> [Relation] -> NodeName -> Request -> [Filter] -> BL.ByteString -> Either Text (Text, Text, Bool)
parseRequest schema allRels rootTableName httpRequest extraFilters reqBody =
  (,,) <$> selectQuery
       <*> (if method == "GET" then pure "" else mutateQuery)
       <*> (if method == "GET" then pure False else pure isSingleRecord)
  where
    hdrs = requestHeaders httpRequest
    lookupHeader = flip lookup hdrs
    isCsv = lookupHeader "Content-Type" == Just csvMT
    method = requestMethod httpRequest
    qParams = queryParams httpRequest
    parsedBody = parseRequestBody isCsv reqBody
    isSingleRecord = either (const False) ((==1) . length . snd ) parsedBody
    parseField f = parse pField ("failed to parse field <<"++f++">>") f
    flds =  join $ first formatParserError . mapM (parseField . cs) <$> (fst <$> parsedBody)
    vals = snd <$> parsedBody
    setWith = if isSingleRecord
          then M.fromList <$> (zip <$> flds <*> (head <$> vals))
          else Left "Expecting a sigle CSV line with header or a JSON object"
    addExtraFilters (Node (q@Insert{},x) y) = Node (q,x) y
    addExtraFilters (Node (q,x) y) = Node (q{where_=where_ q ++ extraFilters},x) y
    allFilters = whereFilters qParams
    mutateFilters = filter (not . ( '.' `elem` ) . fst) allFilters -- update/delete filters can be only on the root table
    cond = first formatParserError $ map snd <$> mapM pRequestFilter mutateFilters
    fakeSourceRelations = mapMaybe (toSourceRelation rootTableName) allRels
    rels = case method of
      "POST"  -> fakeSourceRelations ++ allRels
      "PATCH" -> fakeSourceRelations ++ allRels
      _       -> allRels
    selectApiRequest = augumentRequestWithJoin schema rels
      =<< buildSelectApiRequest rootName sel filters (orderStr qParams)
      where
        sel = if method == "DELETE"
          then "*" -- we are not returning the records so no need to consider nested items
          else selectStr qParams
        rootName = if method == "GET"
          then rootTableName
          else sourceSubqueryName
        filters = if method == "GET"
          then allFilters
          else filter (( '.' `elem` ) . fst) allFilters -- there can be no filters on the root table whre we are doing insert/update
    selectQuery = requestToQuery schema . addExtraFilters <$> selectApiRequest
    mutateQuery = requestToQuery schema . addExtraFilters <$> case method of
      "POST"   -> Node <$> ((,) <$> (Insert rootTableName <$> flds <*> vals)    <*> pure (rootTableName, Nothing)) <*> pure []
      "PATCH"  -> Node <$> ((,) <$> (Update rootTableName <$> setWith <*> cond) <*> pure (rootTableName, Nothing)) <*> pure []
      "DELETE" -> Node <$> ((,) <$> (Delete [rootTableName] <$> cond) <*> pure (rootTableName, Nothing)) <*> pure []
      _        -> undefined

createReadStatement :: Text -> Maybe NonnegRange -> Bool -> Bool -> Bool -> Text
createReadStatement selectQuery range isSingle countTable asCsv =
  wrapQuery selectQuery [
    if countTable then countAllF else countNoneF, -- Count for *all* table rows
    countF,                                       -- Count for selection
    "null",                                       -- Updated row location (doesn’t make sense when selecting)
    if asCsv                                      -- Formatted data
      then asCsvF
      else if isSingle then asJsonSingleF else asJsonF
  ] selectStarF (if isNothing range && isSingle then Just $ singletonRange 0 else range)

createWriteStatement :: Text -> Text -> Bool -> Bool -> [Text] -> Bool -> Text
createWriteStatement selectQuery mutateQuery isSingle echoRequested pKeys asCsv =
  wrapQuery mutateQuery [
    countNoneF,                                   -- Count for *all* table rows (doesn’t make sense when updating)
    countF,                                       -- Count for selection
    if isSingle then locationF pKeys else "null", -- Updated row location
    if echoRequested                              -- Formatted data
    then
      if asCsv
        then asCsvF
        else if isSingle then asJsonSingleF else asJsonF
    else "null"
  ] selectQuery (if isSingle then Just $ singletonRange 0 else Nothing)

extractQueryResult :: Maybe (Maybe Int, Int, Maybe BL.ByteString, Maybe BL.ByteString)
                         -> (Maybe Int, Int, Maybe BL.ByteString, Maybe BL.ByteString)
extractQueryResult = fromMaybe (Just 0, 0, Just "", Just "")
