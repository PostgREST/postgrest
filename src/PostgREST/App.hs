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
import           Data.Tree
import qualified Data.Map as M

import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec (parse)

import           Network.HTTP.Base         (urlEncodeVars)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI    (parseSimpleQuery)
import           Network.Wai
import           Network.Wai.Parse         (parseHttpAccept)

import           Data.Aeson
import           Data.Aeson.Types (emptyArray)
import           Data.Monoid
import qualified Data.Vector               as V
import qualified Hasql                     as H
import qualified Hasql.Backend             as B
import qualified Hasql.Postgres            as P

import           PostgREST.Config          (AppConfig (..))
import           PostgREST.Parsers
import           PostgREST.DbStructure
import           PostgREST.RangeQuery
import           PostgREST.RequestIntent   (Intent(..), ContentType(..)
                                            , Action(..), Target(..)
                                            , Payload(..), userIntent)
import           PostgREST.Types
import           PostgREST.Auth            (tokenJWT)
import           PostgREST.Error           (errResponse)

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

import           Prelude

app :: DbStructure -> AppConfig -> RequestBody -> Request -> H.Tx P.Postgres s Response
app dbStructure conf reqBody req =
  let schema = configSchema conf
      intent = userIntent schema req reqBody
      -- TODO: blow up for Left values
      contentType = either (const ApplicationJSON) id (iAccepts intent)
      contentTypeH = (hContentType, contentType) in

  case (iAction intent, iTarget intent, iPayload intent) of
    (ActionUnknown _, _, _) -> return notFound
    (_, TargetUnknown _, _) -> return notFound
    (_, _, PayloadParseError e) ->
      return $ responseLBS status400 [jsonH]
        (formatGeneralError "Cannot parse request payload" e)

    (ActionInfo, TargetIdent tSchema tTable, _) -> do
      let cols = filter (filterCol tSchema tTable) $ dbColumns dbStructure
          pkeys = map pkName $ filter (filterPk tSchema tTable) allPrKeys
          body = encode (TableOptions cols pkeys)
          filterCol :: Schema -> TableName -> Column -> Bool
          filterCol sc tb (Column{colTable=Table{tableSchema=s, tableName=t}}) = s==sc && t==tb
          filterCol _ _ _ =  False
      return $ responseLBS status200 [jsonH, allOrigins] $ cs body

    (ActionRead, TargetRoot, _) -> do
      body <- encode <$> accessibleTables (filter ((== cs schema) . tableSchema) (dbTables dbStructure))
      return $ responseLBS status200 [jsonH] $ cs body

    (ActionInvoke, TargetIdent qi, PayloadJSON payload) -> do
      exists <- doesProcExist (qiSchema qi) (qiName qi)
      if exists
        then do
          let call = B.Stmt "select " V.empty True <>
                asJson (callProc qi payload)
              jwtSecret = configJwtSecret conf

          bodyJson :: Maybe (Identity Value) <- H.maybeEx call
          returnJWT <- doesProcReturnJWT qi
          return $ responseLBS status200 [jsonH]
                 (let body = fromMaybe emptyArray $ runIdentity <$> bodyJson in
                    if returnJWT
                    then "{\"token\":\"" <> cs (tokenJWT jwtSecret body) <> "\"}"
                    else cs $ encode body)
        else return notFound

    (ActionRead, TargetIdent qi, _) -> do
      let range = iRange intent
          singular = iPreferSingular intent
          selectQuery = requestToQuery schema <$> selectApiRequest
          q = createReadStatement selectQuery range singular
                (not $ iPreferCount intent) contentType
      if range == Just emptyRange
      then return $ errResponse status416 "HTTP Range error"
      else do
        row <- H.maybeEx q
        let (tableTotal, queryTotal, _ , body) = extractQueryResult row
        if singular
        then return $ if queryTotal <= 0
          then responseLBS status404 [] ""
          else responseLBS status200 [contentTypeH] (fromMaybe "{}" body)
        else do
          let frm = fromMaybe 0 $ rangeOffset <$> range
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
                "/" <> cs (qiName qi) <>
                  if Prelude.null canonical then "" else "?" <> cs canonical
              )
            ] (fromMaybe "[]" body)
    (ActionCreate, TargetIdent qi, PayloadJSON payload) -> undefined
    (ActionUpdate, TargetIdent qi, PayloadJSON payload) -> undefined
    (ActionDelete, TargetIdent qi, _) -> undefined
    (ActionRead, TargetIdent qi, _) -> undefined

    (_, _, _) -> return notFound

 where
  notFound = responseLBS status404 [] ""
  filterPk sc table pk = sc == (tableSchema . pkTable) pk && table == (tableName . pkTable) pk
  allPrKeys = dbPrimaryKeys dbStructure
  allOrigins = ("Access-Control-Allow-Origin", "*") :: Header
  --   path          = pathInfo req
  --   verb          = requestMethod req
  --   hdrs          = requestHeaders req
  --   lookupHeader  = flip lookup hdrs
  --   hasPrefer val = any (\(h,v) -> h == "Prefer" && v == val) hdrs
  --   schema        = cs $ configSchema conf
  --   range         = rangeRequested hdrs
  --   request = parseRequest schema (dbRelations dbStructure) (head path) req reqBody --TODO! is head safe?



  -- case (path, verb) of
  --   ([table], _) ->
  --     case request of
  --       Left e -> return $ responseLBS status400 [jsonH] $ cs e
  --       Right (selectQuery, Nothing) -> -- should we do sanity check to make sure its a GET request?
  --       Right (selectQuery, Just (mutateQuery, isSingle)) ->
  --         case verb of
  --           "POST"    -> do
  --             let pKeys = map pkName $ filter (filterPk schema table) allPrKeys -- would it be ok to move primary key detection in the query itself?
  --                 q = createWriteStatement selectQuery mutateQuery isSingle echoRequested pKeys isCsv
  --             row <- H.maybeEx q
  --             let (_, _, location, body) = extractQueryResult row
  --             return $ responseLBS status201
  --               [
  --                 contentTypeH,
  --                 (hLocation, "/" <> cs table <> "?" <> cs (fromMaybe "" location))
  --               ]
  --               $ if echoRequested then fromMaybe "[]" body else ""
  --           "PATCH"   -> do
  --             let q = createWriteStatement selectQuery mutateQuery False echoRequested [] isCsv
  --             row <- H.maybeEx q
  --             let (_, queryTotal, _, body) = extractQueryResult row
  --                 r = contentRangeH 0 (queryTotal-1) (Just queryTotal)
  --                 s = case () of _ | queryTotal == 0 -> status404
  --                                  | echoRequested -> status200
  --                                  | otherwise -> status204
  --             return $ responseLBS s [contentTypeH, r]
  --               $ if echoRequested then fromMaybe "[]" body else ""
  --           "DELETE"  -> do
  --             let q = createWriteStatement selectQuery mutateQuery False False [] isCsv
  --             row <- H.maybeEx q
  --             let (_, queryTotal, _, _) = extractQueryResult row
  --             return $ if queryTotal == 0
  --               then notFound
  --               else responseLBS status204 [("Content-Range", "*/"<> cs (show queryTotal))] ""
  --           _         -> return notFound

  -- where

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
formatRelationError e = formatGeneralError
  "could not find foreign keys between these entities" e

formatParserError :: ParseError -> Text
formatParserError e = formatGeneralError message details
  where
     message = show (errorPos e)
     details = strip $ replace "\n" " " $ cs
       $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)

formatGeneralError :: Text -> Text -> Text
formatGeneralError message details = cs $ encode $ object [
  "message" .= message,
  "details" .= details]

parseRequestBody :: Bool -> RequestBody -> Either Text ([Text],[[Value]])
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

buildSelectApiRequest :: Text -> Schema -> TableName  -> [(String, String)] ->  [Relation] -> [(String, Maybe String)] -> Either Text ApiRequest
buildSelectApiRequest method schema rootTableName allFilters allRels qParams =
  augumentRequestWithJoin schema rels =<< first formatParserError (foldr addFilter <$> (addOrder <$> apiRequest <*> ord) <*> flts)
  where
    selStr = selectStr qParams
    orderS = orderStr qParams
    rels = case method of
      "POST"  -> fakeSourceRelations ++ allRels
      "PATCH" -> fakeSourceRelations ++ allRels
      _       -> allRels
      where fakeSourceRelations = mapMaybe (toSourceRelation rootTableName) allRels -- see comment in toSourceRelation
    sel = if method == "DELETE"
      then "*" -- we are not returning the records so no need to consider nested items
      else selStr
    rootName = if method == "GET"
      then rootTableName
      else sourceSubqueryName
    filters = if method == "GET"
      then allFilters
      else filter (( '.' `elem` ) . fst) allFilters -- there can be no filters on the root table whre we are doing insert/update
    apiRequest = parse (pRequestSelect rootName) ("failed to parse select parameter <<"++sel++">>") sel
    addOrder (Node (q,i) f) o = Node (q{order=o}, i) f
    flts = mapM pRequestFilter filters
    ord = traverse (parse pOrder ("failed to parse order parameter <<"++fromMaybe "" orderS++">>")) orderS

buildMutateApiRequest :: Text -> Bool -> TableName -> RequestBody -> [(String, String)] -> Either Text (ApiRequest, Bool)
buildMutateApiRequest method isCsv rootTableName reqBody allFilters =
  (,) <$> mutateApiRequest <*> pure isSingleRecord
  where
    mutateApiRequest = case method of
      "POST"   -> Node <$> ((,) <$> (Insert rootTableName <$> flds <*> vals)    <*> pure (rootTableName, Nothing)) <*> pure []
      "PATCH"  -> Node <$> ((,) <$> (Update rootTableName <$> setWith <*> cond) <*> pure (rootTableName, Nothing)) <*> pure []
      "DELETE" -> Node <$> ((,) <$> (Delete [rootTableName] <$> cond) <*> pure (rootTableName, Nothing)) <*> pure []
      _        -> Left "Unsupported HTTP verb"
    parseField f = parse pField ("failed to parse field <<"++f++">>") f
    parsedBody = parseRequestBody isCsv reqBody
    isSingleRecord = either (const False) ((==1) . length . snd ) parsedBody
    flds =  join $ first formatParserError . mapM (parseField . cs) <$> (fst <$> parsedBody)
    vals = snd <$> parsedBody
    mutateFilters = filter (not . ( '.' `elem` ) . fst) allFilters -- update/delete filters can be only on the root table
    cond = first formatParserError $ map snd <$> mapM pRequestFilter mutateFilters
    setWith = if isSingleRecord
          then M.fromList <$> (zip <$> flds <*> (head <$> vals))
          else Left "Expecting a sigle CSV line with header or a JSON object"

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

-- in a relation where one of the tables mathces "TableName"
-- replace the name to that table with pg_source
-- this "fake" relations is needed so that in a mutate query
-- we can look a the "returning *" part which is wrapped with a "with"
-- as just another table that has relations with other tables
toSourceRelation :: TableName -> Relation -> Maybe Relation
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

createSelectQuery :: [Relation] -> QualifiedIdentifier -> SqlQuery
createSelectQuery rels qi =
  requestToQuery schema <$> selectApiRequest
  undefined

parseRequest :: Schema -> [Relation] -> TableName -> Request -> RequestBody -> Either Text (SqlQuery, Maybe (SqlQuery, Bool))
parseRequest schema allRels rootTableName httpRequest reqBody =
  if method == "GET"
  then (,Nothing) <$> selectQuery
  else (,) <$> selectQuery <*> (  Just <$> mutatePart  )
  where
    mutatePart = (,) <$> mutateQuery <*> isSingleRecord
    hdrs = requestHeaders httpRequest
    lookupHeader = flip lookup hdrs
    isCsv = lookupHeader "Content-Type" == Just csvMT
    method = requestMethod httpRequest
    qParams = queryParams httpRequest
    allFilters = whereFilters qParams
    selectApiRequest = buildSelectApiRequest (cs method) schema rootTableName allFilters allRels qParams
    mutateTuple = buildMutateApiRequest (cs method) isCsv rootTableName reqBody allFilters
    mutateApiRequest = fst <$> mutateTuple
    isSingleRecord = snd <$> mutateTuple
    selectQuery = requestToQuery schema <$> selectApiRequest
    mutateQuery = requestToQuery schema <$> mutateApiRequest

createReadStatement :: SqlQuery -> Maybe NonnegRange -> Bool -> Bool -> Bool -> B.Stmt P.Postgres
createReadStatement selectQuery range isSingle countTable asCsv =
  B.Stmt (
    wrapQuery selectQuery [
      if countTable then countAllF else countNoneF,
      countF,
      "null", -- location header can not be calucalted
      if asCsv
        then asCsvF
        else if isSingle then asJsonSingleF else asJsonF
    ] selectStarF (if isNothing range && isSingle then Just $ singletonRange 0 else range)
  ) V.empty True

createWriteStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> [Text] -> Bool -> B.Stmt P.Postgres
createWriteStatement selectQuery mutateQuery isSingle echoRequested pKeys asCsv =
  B.Stmt (
    wrapQuery mutateQuery [
      countNoneF, -- when updateing it does not make sense
      countF,
      if isSingle then locationF pKeys else "null",
      if echoRequested
      then
        if asCsv
        then asCsvF
        else if isSingle then asJsonSingleF else asJsonF
      else "null"

    ] selectQuery Nothing
  ) V.empty True

extractQueryResult :: Maybe (Maybe Int, Int, Maybe BL.ByteString, Maybe BL.ByteString)
                         -> (Maybe Int, Int, Maybe BL.ByteString, Maybe BL.ByteString)
extractQueryResult = fromMaybe (Just 0, 0, Just "", Just "")
