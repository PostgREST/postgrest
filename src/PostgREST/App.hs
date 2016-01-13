{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
--module PostgREST.App where
module PostgREST.App (
  app
) where

import           Control.Applicative
import           Control.Arrow             ((***))
import           Control.Monad             (join)
import           Data.Bifunctor            (first)
import qualified Data.ByteString.Lazy      as BL
import           Data.Functor.Identity
import           Data.List                 (find, sortBy, delete)
import           Data.Maybe                (fromMaybe, fromJust, mapMaybe)
import           Data.Ord                  (comparing)
import           Data.Ranged.Ranges        (emptyRange)
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text, replace, strip)
import           Data.Tree

import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec (parse)

import           Network.HTTP.Base         (urlEncodeVars)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI    (parseSimpleQuery)
import           Network.Wai

import           Data.Aeson
import           Data.Aeson.Types (emptyArray)
import           Data.Monoid
import qualified Data.Vector               as V
import qualified Hasql.Connection          as H

import           PostgREST.Config          (AppConfig (..))
import           PostgREST.Parsers
import           PostgREST.DbStructure
import           PostgREST.RangeQuery
import           PostgREST.ApiRequest   (ApiRequest(..), ContentType(..)
                                            , Action(..), Target(..)
                                            , PreferRepresentation (..)
                                            , userApiRequest)
import           PostgREST.Types
import           PostgREST.Auth            (tokenJWT)
import           PostgREST.Error           (errResponse)

import           PostgREST.QueryBuilder ( asJson
                                        , callProc
                                        , addJoinConditions
                                        , sourceCTEName
                                        , requestToQuery
                                        , requestToCountQuery
                                        , addRelations
                                        , createReadStatement
                                        , createWriteStatement
                                        )

import           Prelude

app :: DbStructure -> AppConfig -> RequestBody -> Request -> H.Tx P.Postgres s Response
app dbStructure conf reqBody req =
  let
      -- TODO: blow up for Left values (there is a middleware that checks the headers)
      contentType = either (const ApplicationJSON) id (iAccepts apiRequest)
      contentTypeH = (hContentType, cs $ show contentType) in

  case (iAction apiRequest, iTarget apiRequest, iPayload apiRequest) of

    (ActionRead, TargetIdent qi, Nothing) ->
      case readSqlParts of
        Left e -> return $ responseLBS status400 [jsonH] $ cs e
        Right (q, cq) -> do
          let range = restrictRange (configMaxRows conf) $ iRange apiRequest
              singular = iPreferSingular apiRequest
              stm = createReadStatement q cq range singular
                    (iPreferCount apiRequest) (contentType == TextCSV)
          if range == emptyRange
          then return $ errResponse status416 "HTTP Range error"
          else do
            row <- H.maybeEx stm
            let (tableTotal, queryTotal, _ , body) = extractQueryResult row
            if singular
            then return $ if queryTotal <= 0
              then responseLBS status404 [] ""
              else responseLBS status200 [contentTypeH] (fromMaybe "{}" body)
            else do
              let frm = rangeOffset range
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

    (ActionCreate, TargetIdent qi@(QualifiedIdentifier _ table),
     Just payload@(PayloadJSON (UniformObjects rows))) ->
      case mutateSqlParts of
        Left e -> return $ responseLBS status400 [jsonH] $ cs e
        Right (sq,mq) -> do
          let isSingle = (==1) $ V.length rows
          let pKeys = map pkName $ filter (filterPk schema table) allPrKeys -- would it be ok to move primary key detection in the query itself?
          let stm = createWriteStatement qi sq mq isSingle (iPreferRepresentation apiRequest) pKeys (contentType == TextCSV) payload
          row <- H.maybeEx stm
          let (_, _, location, body) = extractQueryResult row
          return $ responseLBS status201
            [
              contentTypeH,
              (hLocation, "/" <> cs table <> "?" <> cs (fromMaybe "" location))
            ]
            $ if iPreferRepresentation apiRequest == Full then fromMaybe "[]" body else ""

    (ActionUpdate, TargetIdent qi, Just payload@(PayloadJSON _)) ->
      case mutateSqlParts of
        Left e -> return $ responseLBS status400 [jsonH] $ cs e
        Right (sq,mq) -> do
          let stm = createWriteStatement qi sq mq False (iPreferRepresentation apiRequest) [] (contentType == TextCSV) payload
          row <- H.maybeEx stm
          let (_, queryTotal, _, body) = extractQueryResult row
              r = contentRangeH 0 (queryTotal-1) (Just queryTotal)
              s = case () of _ | queryTotal == 0 -> status404
                               | iPreferRepresentation apiRequest == Full -> status200
                               | otherwise -> status204
          return $ responseLBS s [contentTypeH, r]
            $ if iPreferRepresentation apiRequest == Full then fromMaybe "[]" body else ""

    (ActionDelete, TargetIdent qi, Nothing) ->
      case mutateSqlParts of
        Left e -> return $ responseLBS status400 [jsonH] $ cs e
        Right (sq,mq) -> do
          let fakeload = PayloadJSON $ UniformObjects V.empty
          let stm = createWriteStatement qi sq mq False (iPreferRepresentation apiRequest) [] (contentType == TextCSV) fakeload
          row <- H.maybeEx stm
          let (_, queryTotal, _, _) = extractQueryResult row
          return $ if queryTotal == 0
            then notFound
            else responseLBS status204 [("Content-Range", "*/"<> cs (show queryTotal))] ""

    (ActionInfo, TargetIdent (QualifiedIdentifier tSchema tTable), Nothing) -> do
      let cols = filter (filterCol tSchema tTable) $ dbColumns dbStructure
          pkeys = map pkName $ filter (filterPk tSchema tTable) allPrKeys
          body = encode (TableOptions cols pkeys)
          filterCol :: Schema -> TableName -> Column -> Bool
          filterCol sc tb (Column{colTable=Table{tableSchema=s, tableName=t}}) = s==sc && t==tb
          filterCol _ _ _ =  False
      return $ responseLBS status200 [jsonH, allOrigins] $ cs body

    (ActionInvoke, TargetIdent qi,
     Just (PayloadJSON (UniformObjects payload))) -> do
      exists <- doesProcExist qi
      if exists
        then do
          let p = V.head payload
              call = B.Stmt "select " V.empty True <>
                asJson (callProc qi p)
              jwtSecret = configJwtSecret conf

          bodyJson :: Maybe (Identity Value) <- H.maybeEx call
          returnJWT <- doesProcReturnJWT qi
          return $ responseLBS status200 [jsonH]
                 (let body = fromMaybe emptyArray $ runIdentity <$> bodyJson in
                    if returnJWT
                    then "{\"token\":\"" <> cs (tokenJWT jwtSecret body) <> "\"}"
                    else cs $ encode body)
        else return notFound

    (ActionRead, TargetRoot, Nothing) -> do
      body <- encode <$> accessibleTables (cs schema)
      return $ responseLBS status200 [jsonH] $ cs body

    (ActionUnknown _, _, _) -> return notFound

    (_, TargetUnknown _, _) -> return notFound

    (_, _, Just (PayloadParseError e)) ->
      return $ responseLBS status400 [jsonH] $
        cs (formatGeneralError "Cannot parse request payload" (cs e))

    (_, _, _) -> return notFound

 where
  notFound = responseLBS status404 [] ""
  filterPk sc table pk = sc == (tableSchema . pkTable) pk && table == (tableName . pkTable) pk
  allPrKeys = dbPrimaryKeys dbStructure
  allOrigins = ("Access-Control-Allow-Origin", "*") :: Header
  schema = cs $ configSchema conf
  apiRequest = userApiRequest schema req reqBody
  readDbRequest = DbRead <$> buildReadRequest (dbRelations dbStructure) apiRequest
  mutateDbRequest = DbMutate <$> buildMutateRequest apiRequest
  selectQuery = requestToQuery schema <$> readDbRequest
  countQuery = requestToCountQuery schema <$> readDbRequest
  mutateQuery = requestToQuery schema <$> mutateDbRequest
  readSqlParts = (,) <$> selectQuery <*> countQuery
  mutateSqlParts = (,) <$> selectQuery <*> mutateQuery

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

jsonH :: Header
jsonH = (hContentType, "application/json")

formatRelationError :: Text -> Text
formatRelationError = formatGeneralError
  "could not find foreign keys between these entities"

formatParserError :: ParseError -> Text
formatParserError e = formatGeneralError message details
  where
     message = cs $ show (errorPos e)
     details = strip $ replace "\n" " " $ cs
       $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)

formatGeneralError :: Text -> Text -> Text
formatGeneralError message details = cs $ encode $ object [
  "message" .= message,
  "details" .= details]

augumentRequestWithJoin :: Schema ->  [Relation] ->  ReadRequest -> Either Text ReadRequest
augumentRequestWithJoin schema allRels request =
  (first formatRelationError . addRelations schema allRels Nothing) request
  >>= addJoinConditions schema

buildReadRequest :: [Relation] -> ApiRequest -> Either Text ReadRequest
buildReadRequest allRels apiRequest  =
  augumentRequestWithJoin schema rels =<< first formatParserError (foldr addFilter <$> (addOrder <$> readRequest <*> ord) <*> flts)
  where
    selStr = iSelect apiRequest
    orderS = iOrder apiRequest
    action = iAction apiRequest
    target = iTarget apiRequest
    (schema, rootTableName) = fromJust $ -- Make it safe
      case target of
        (TargetIdent (QualifiedIdentifier s t) ) -> Just (s, t)
        _ -> Nothing

    rootName = if action == ActionRead
      then rootTableName
      else sourceCTEName
    filters = if action == ActionRead
      then iFilters apiRequest
      else filter (( '.' `elem` ) . fst) $ iFilters apiRequest -- there can be no filters on the root table whre we are doing insert/update
    rels = case action of
      ActionCreate -> fakeSourceRelations ++ allRels
      ActionUpdate -> fakeSourceRelations ++ allRels
      _       -> allRels
      where fakeSourceRelations = mapMaybe (toSourceRelation rootTableName) allRels -- see comment in toSourceRelation
    readRequest = parse (pRequestSelect rootName) ("failed to parse select parameter <<"++selStr++">>") selStr
    addOrder (Node (q,i) f) o = Node (q{order=o}, i) f
    flts = mapM pRequestFilter filters
    ord = traverse (parse pOrder ("failed to parse order parameter <<"++fromMaybe "" orderS++">>")) orderS

buildMutateRequest :: ApiRequest -> Either Text MutateRequest
buildMutateRequest apiRequest =
  mutateApiRequest
  where
    action = iAction apiRequest
    target = iTarget apiRequest
    payload = fromJust $ iPayload apiRequest
    rootTableName = -- TODO: Make it safe
      case target of
        (TargetIdent (QualifiedIdentifier _ t) ) -> t
        _ -> undefined
    mutateApiRequest = case action of
      ActionCreate -> Insert rootTableName <$> pure payload
      ActionUpdate -> Update rootTableName <$> pure payload <*> cond
      ActionDelete -> Delete rootTableName <$> cond
      _        -> Left "Unsupported HTTP verb"
    mutateFilters = filter (not . ( '.' `elem` ) . fst) $ iFilters apiRequest -- update/delete filters can be only on the root table
    cond = first formatParserError $ map snd <$> mapM pRequestFilter mutateFilters

addFilter :: (Path, Filter) -> ReadRequest -> ReadRequest
addFilter ([], flt) (Node (q@(Select {flt_=flts}), i) forest) = Node (q {flt_=flt:flts}, i) forest
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
  | mt == tableName t = Just $ r {relTable=t {tableName=sourceCTEName}}
  | mt == tableName ft = Just $ r {relFTable=t {tableName=sourceCTEName}}
  | Just mt == (tableName <$> rt) = Just $ r {relLTable=(\tbl -> tbl {tableName=sourceCTEName}) <$> rt}
  | otherwise = Nothing

data TableOptions = TableOptions {
  tblOptcolumns :: [Column]
, tblOptpkey    :: [Text]
}

instance ToJSON TableOptions where
  toJSON t = object [
      "columns" .= tblOptcolumns t
    , "pkey"   .= tblOptpkey t ]


extractQueryResult :: Maybe (Maybe Int, Int, Maybe BL.ByteString, Maybe BL.ByteString)
                         -> (Maybe Int, Int, Maybe BL.ByteString, Maybe BL.ByteString)
extractQueryResult = fromMaybe (Just 0, 0, Just "", Just "")
