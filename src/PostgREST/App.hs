{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
--module PostgREST.App where
module PostgREST.App (
  postgrest
) where

import           Control.Applicative
import qualified Data.ByteString.Char8   as BS
import           Data.IORef                (IORef, readIORef)
import           Data.List                 (delete, lookup)
import           Data.Maybe                (fromJust)
import           Data.Ranged.Ranges        (emptyRange)
import           Data.Text                 (replace, strip, isInfixOf, dropWhile, drop, intercalate)
import           Data.Tree

import qualified Hasql.Pool                as P
import qualified Hasql.Transaction         as HT

import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec (parse)

import qualified Text.InterpolatedString.Perl6 as P6 (q)

import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI (renderSimpleQuery)
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger (logStdout)

import           Data.Aeson
import           Data.Aeson.Types (emptyArray)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import qualified Data.Vector               as V
import qualified Hasql.Transaction         as H

import qualified Data.HashMap.Strict       as M

import           PostgREST.ApiRequest   (ApiRequest(..), ContentType(..)
                                            , Action(..), Target(..)
                                            , PreferRepresentation (..)
                                            , userApiRequest, mutuallyAgreeable
                                            , ctToHeader
                                            , userApiRequest
                                            , toHeader)
import           PostgREST.Auth            (tokenJWT, jwtClaims, containsRole)
import           PostgREST.Config          (AppConfig (..))
import           PostgREST.DbStructure
import           PostgREST.Error           (errResponse, pgErrResponse)
import           PostgREST.Parsers
import           PostgREST.RangeQuery      (NonnegRange, allRange, rangeOffset, restrictRange)
import           PostgREST.Middleware
import           PostgREST.QueryBuilder ( callProc
                                        , addJoinConditions
                                        , sourceCTEName
                                        , requestToQuery
                                        , requestToCountQuery
                                        , addRelations
                                        , createReadStatement
                                        , createWriteStatement
                                        , ResultsWithCount
                                        )
import           PostgREST.Types
import           PostgREST.OpenAPI

import           Data.Foldable (foldr1)
import           Data.Function (id)
import           Protolude                hiding (dropWhile, drop, intercalate, Proxy)

postgrest :: AppConfig -> IORef DbStructure -> P.Pool -> Application
postgrest conf refDbStructure pool =
  let middle = (if configQuiet conf then id else logStdout) . defaultMiddle in

  middle $ \ req respond -> do
    time <- getPOSIXTime
    body <- strictRequestBody req
    dbStructure <- readIORef refDbStructure

    let schema = toS $ configSchema conf
        apiRequest = userApiRequest schema req body
        eClaims = jwtClaims (configJwtSecret conf) (iJWT apiRequest) time
        authed = containsRole eClaims
        handleReq = runWithClaims conf eClaims (app dbStructure conf) apiRequest
        txMode = transactionMode $ iAction apiRequest

    resp <- either (pgErrResponse authed) id <$> P.use pool
      (HT.run handleReq HT.ReadCommitted txMode)
    respond resp

transactionMode :: Action -> H.Mode
transactionMode ActionRead = HT.Read
transactionMode ActionInfo = HT.Read
transactionMode _ = HT.Write

app :: DbStructure -> AppConfig -> ApiRequest -> H.Transaction Response
app dbStructure conf apiRequest =
  case (iAction apiRequest, iTarget apiRequest, iPayload apiRequest) of

    (ActionRead, TargetIdent qi, Nothing) ->
      serves [CTApplicationJSON, CTTextCSV] (iAccepts apiRequest) $ \contentType ->
      case readSqlParts of
        Left e -> return $ responseLBS status400 [jsonH] $ toS e
        Right (q, cq) -> do
          let singular = iPreferSingular apiRequest
              stm = createReadStatement q cq singular
                    shouldCount (contentType == CTTextCSV)
          respondToRange $ do
            row <- H.query () stm
            let (tableTotal, queryTotal, _ , body) = row
            if singular
            then return $ if queryTotal <= 0
              then responseLBS status404 [] ""
              else responseLBS status200 [ctToHeader contentType] (toS body)
            else do
              let (status, contentRange) = rangeHeader queryTotal tableTotal
                  canonical = iCanonicalQS apiRequest
              return $ responseLBS status
                [ctToHeader contentType, contentRange,
                  ("Content-Location",
                    "/" <> toS (qiName qi) <>
                      if BS.null canonical then "" else "?" <> toS canonical
                  )
                ] (toS body)

    (ActionCreate, TargetIdent qi@(QualifiedIdentifier _ table),
     Just payload@(PayloadJSON uniform@(UniformObjects rows))) ->
      serves [CTApplicationJSON, CTTextCSV] (iAccepts apiRequest) $ \contentType ->
      case mutateSqlParts of
        Left e -> return $ responseLBS status400 [jsonH] $ toS e
        Right (sq,mq) -> do
          let isSingle = (==1) $ V.length rows
          when (not isSingle && iPreferSingular apiRequest) $
            HT.sql [P6.q| DO $$
                       BEGIN RAISE EXCEPTION cardinality_violation
                       USING MESSAGE =
                         'plurality=singular specified, but more than one object would be inserted';
                       END $$;
                     |]
          let pKeys = map pkName $ filter (filterPk schema table) allPrKeys -- would it be ok to move primary key detection in the query itself?
          let stm = createWriteStatement qi sq mq isSingle (iPreferRepresentation apiRequest) pKeys (contentType == CTTextCSV) payload
          row <- H.query uniform stm
          let (_, _, fs, body) = extractQueryResult row
              header =
                if null fs then []
                else [(hLocation, "/" <> toS table <> renderLocationFields fs)]

          return $ if iPreferRepresentation apiRequest == Full
            then responseLBS status201 (ctToHeader contentType : header) (toS body)
            else responseLBS status201 header ""

    (ActionUpdate, TargetIdent qi, Just payload@(PayloadJSON uniform)) ->
      serves [CTApplicationJSON, CTTextCSV] (iAccepts apiRequest) $ \contentType ->
      case mutateSqlParts of
        Left e -> return $ responseLBS status400 [jsonH] $ toS e
        Right (sq,mq) -> do
          let singular = iPreferSingular apiRequest
              stm = createWriteStatement qi sq mq singular (iPreferRepresentation apiRequest) [] (contentType == CTTextCSV) payload
          row <- H.query uniform stm
          let (_, queryTotal, _, body) = extractQueryResult row
          when (singular && queryTotal > 1) $
            HT.sql [P6.q| DO $$
                       BEGIN RAISE EXCEPTION cardinality_violation
                       USING MESSAGE =
                         'plurality=singular specified, but more than one object would be updated';
                       END $$;
                     |]
          let r = contentRangeH 0 (toInteger $ queryTotal-1)
                    (toInteger <$> if shouldCount then Just queryTotal else Nothing)
              s = case () of _ | queryTotal == 0 -> status404
                               | iPreferRepresentation apiRequest == Full -> status200
                               | otherwise -> status204
          return $ if iPreferRepresentation apiRequest == Full
            then responseLBS s [ctToHeader contentType, r] (toS body)
            else responseLBS s [r] ""

    (ActionDelete, TargetIdent qi, Nothing) ->
      serves [CTApplicationJSON, CTTextCSV] (iAccepts apiRequest) $ \contentType ->
      case mutateSqlParts of
        Left e -> return $ responseLBS status400 [jsonH] $ toS e
        Right (sq,mq) -> do
          let emptyUniform = UniformObjects V.empty
              fakeload = PayloadJSON emptyUniform
              stm = createWriteStatement qi sq mq False (iPreferRepresentation apiRequest) [] (contentType == CTTextCSV) fakeload
          row <- H.query emptyUniform stm
          let (_, queryTotal, _, body) = extractQueryResult row
              r = contentRangeH 1 0 $
                    toInteger <$> if shouldCount then Just queryTotal else Nothing
          return $ if queryTotal == 0
            then notFound
            else if iPreferRepresentation apiRequest == Full
              then responseLBS status200 [ctToHeader contentType, r] (toS body)
              else responseLBS status204 [r] ""

    (ActionInfo, TargetIdent (QualifiedIdentifier tSchema tTable), Nothing) ->
      let mTable = find (\t -> tableName t == tTable && tableSchema t == tSchema) (dbTables dbStructure) in
      case mTable of
        Nothing -> return notFound
        Just table ->
          let acceptH = (hAllow, if tableInsertable table then "GET,POST,PATCH,DELETE" else "GET") in
          return $ responseLBS status200 [allOrigins, acceptH] ""

    (ActionInvoke, TargetProc qi,
     Just (PayloadJSON (UniformObjects payload))) -> do
        let p = V.head payload
            singular = iPreferSingular apiRequest
            jwtSecret = configJwtSecret conf
            returnType = lookup (qiName qi) $ dbProcs dbStructure
            returnsJWT = fromMaybe False $ isInfixOf "jwt_claims" <$> returnType
        serves [CTApplicationJSON] (iAccepts apiRequest) $ \_ -> case readSqlParts of
          Left e -> return $ responseLBS status400 [jsonH] $ toS e
          Right (q,cq) -> respondToRange $ do
            row <- H.query () (callProc qi p q cq topLevelRange shouldCount singular)
            let (tableTotal, queryTotal, body) = fromMaybe (Just 0, 0, emptyArray) row
                (status, contentRange) = rangeHeader queryTotal tableTotal
              in
              return $ responseLBS status [jsonH, contentRange]
                      (if returnsJWT
                      then "{\"token\":\"" <> toS (tokenJWT jwtSecret body) <> "\"}"
                      else toS $ encode body)

    (ActionRead, TargetRoot, Nothing) -> do
      let host = configHost conf
          port = toInteger $ configPort conf
          proxy = pickProxy $ toS <$> configProxyUri conf
          uri Nothing = ("http", host, port, "/")
          uri (Just Proxy { proxyScheme = s, proxyHost = h, proxyPort = p, proxyPath = b }) = (s, h, p, b)
          uri' = uri proxy
          encodeApi ti = encodeOpenAPI ti uri'
      serves [CTOpenAPI] (iAccepts apiRequest) $ \_ -> do
        body <- encodeApi . toTableInfo <$> H.query schema accessibleTables
        return $ responseLBS status200 [openapiH] $ toS body

    (ActionInappropriate, _, _) -> return $ responseLBS status405 [] ""

    (_, _, Just (PayloadParseError e)) ->
      return $ responseLBS status400 [jsonH] $
        toS (formatGeneralError "Cannot parse request payload" (toS e))

    (_, TargetUnknown _, _) -> return notFound

    (_, _, _) -> return notFound

 where
  toTableInfo :: [Table] -> [(Table, [Column], [Text])]
  toTableInfo = map (\t ->
    let tSchema = tableSchema t
        tTable = tableName t
        cols = filter (filterCol tSchema tTable) $ dbColumns dbStructure
        pkeys = map pkName $ filter (filterPk tSchema tTable) allPrKeys
     in
        (t, cols, pkeys))
  notFound = responseLBS status404 [] ""
  filterPk sc table pk = sc == (tableSchema . pkTable) pk && table == (tableName . pkTable) pk
  filterCol :: Schema -> TableName -> Column -> Bool
  filterCol sc tb Column{colTable=Table{tableSchema=s, tableName=t}} = s==sc && t==tb
  filterCol _ _ _ =  False
  allPrKeys = dbPrimaryKeys dbStructure
  allOrigins = ("Access-Control-Allow-Origin", "*") :: Header
  jsonH = ctToHeader CTApplicationJSON
  openapiH = ctToHeader CTOpenAPI
  schema = toS $ configSchema conf
  shouldCount = iPreferCount apiRequest
  topLevelRange = fromMaybe allRange $ M.lookup "limit" $ iRange apiRequest
  readDbRequest = DbRead <$> buildReadRequest (configMaxRows conf) (dbRelations dbStructure) (dbProcs dbStructure) apiRequest
  mutateDbRequest = DbMutate <$> buildMutateRequest apiRequest
  selectQuery = requestToQuery schema False <$> readDbRequest
  countQuery = requestToCountQuery schema <$> readDbRequest
  mutateQuery = requestToQuery schema False <$> mutateDbRequest
  readSqlParts = (,) <$> selectQuery <*> countQuery
  mutateSqlParts = (,) <$> selectQuery <*> mutateQuery
  respondToRange response = if topLevelRange == emptyRange
                            then return $ errResponse status416 "HTTP Range error"
                            else response
  rangeHeader queryTotal tableTotal = let lower = rangeOffset topLevelRange
                                          upper = lower + toInteger queryTotal - 1
                                          contentRange = contentRangeH lower upper (toInteger <$> tableTotal)
                                          status = rangeStatus lower upper (toInteger <$> tableTotal)
                                      in (status, contentRange)


serves :: Monad m => [ContentType] -> [ContentType] ->
  (ContentType -> m Response) -> m Response
serves sProduces cAccepts resp =
  case mutuallyAgreeable sProduces cAccepts of
    Nothing -> do
      let failed = intercalate ", " $ map (toS . toHeader) cAccepts
      return $ errResponse status415 $
        "None of these Content-Types are available: " <> failed
    Just ct -> resp ct

splitKeyValue :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitKeyValue kv = (k, BS.tail v)
  where (k, v) = BS.break (== '=') kv

renderLocationFields :: [BS.ByteString] -> BS.ByteString
renderLocationFields fields =
  renderSimpleQuery True $ map splitKeyValue fields

rangeStatus :: Integer -> Integer -> Maybe Integer -> Status
rangeStatus _ _ Nothing = status200
rangeStatus lower upper (Just total)
  | lower > total            = status416
  | (1 + upper - lower) < total = status206
  | otherwise               = status200

contentRangeH :: Integer -> Integer -> Maybe Integer -> Header
contentRangeH lower upper total =
    ("Content-Range", headerValue)
    where
      headerValue   = rangeString <> "/" <> totalString
      rangeString
        | totalNotZero && fromInRange = show lower <> "-" <> show upper
        | otherwise = "*"
      totalString   = fromMaybe "*" (show <$> total)
      totalNotZero  = fromMaybe True ((/=) 0 <$> total)
      fromInRange   = lower <= upper

formatRelationError :: Text -> Text
formatRelationError = formatGeneralError
  "could not find foreign keys between these entities"

formatParserError :: ParseError -> Text
formatParserError e = formatGeneralError message details
  where
     message = show $ errorPos e
     details = strip $ replace "\n" " " $ toS
       $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)

formatGeneralError :: Text -> Text -> Text
formatGeneralError message details = toS $ encode $ object [
  "message" .= message,
  "details" .= details]

augumentRequestWithJoin :: Schema ->  [Relation] ->  ReadRequest -> Either Text ReadRequest
augumentRequestWithJoin schema allRels request =
  (first formatRelationError . addRelations schema allRels Nothing) request
  >>= addJoinConditions schema

addFiltersOrdersRanges :: ApiRequest -> Either ParseError (ReadRequest -> ReadRequest)
addFiltersOrdersRanges apiRequest = foldr1 (liftA2 (.)) [
    flip (foldr addFilter) <$> filters,
    flip (foldr addOrder) <$> orders,
    flip (foldr addRange) <$> ranges
  ]
  {-
  The esence of what is going on above is that we are composing tree functions
  of type (ReadRequest->ReadRequest) that are in (Either ParseError a) context
  -}
  where
    filters :: Either ParseError [(Path, Filter)]
    filters = mapM pRequestFilter flts
      where
        action = iAction apiRequest
        flts
          | action == ActionRead = iFilters apiRequest
          | action == ActionInvoke = iFilters apiRequest
          | otherwise = filter (( "." `isInfixOf` ) . fst) $ iFilters apiRequest -- there can be no filters on the root table whre we are doing insert/update
    orders :: Either ParseError [(Path, [OrderTerm])]
    orders = mapM pRequestOrder $ iOrder apiRequest
    ranges :: Either ParseError [(Path, NonnegRange)]
    ranges = mapM pRequestRange $ M.toList $ iRange apiRequest

treeRestrictRange :: Maybe Integer -> ReadRequest -> Either Text ReadRequest
treeRestrictRange maxRows_ request = pure $ nodeRestrictRange maxRows_ `fmap` request
  where
    nodeRestrictRange :: Maybe Integer -> ReadNode -> ReadNode
    nodeRestrictRange m (q@Select {range_=r}, i) = (q{range_=restrictRange m r }, i)

buildReadRequest :: Maybe Integer -> [Relation] -> [(Text, Text)] -> ApiRequest -> Either Text ReadRequest
buildReadRequest maxRows allRels allProcs apiRequest  =
  treeRestrictRange maxRows =<<
  augumentRequestWithJoin schema relations =<<
  first formatParserError readRequest
  where
    (schema, rootTableName) = fromJust $ -- Make it safe
      let target = iTarget apiRequest in
      case target of
        (TargetIdent (QualifiedIdentifier s t) ) -> Just (s, t)
        (TargetProc  (QualifiedIdentifier s p) ) -> Just (s, t)
          where
            returnType = fromMaybe "" $ lookup p allProcs
            -- we are looking for results looking like "SETOF schema.tablename" and want to extract tablename
            t = if "SETOF " `isInfixOf` returnType
              then drop 1 $ dropWhile (/= '.') returnType
              else p

        _ -> Nothing

    action :: Action
    action = iAction apiRequest

    readRequest :: Either ParseError ReadRequest
    readRequest = addFiltersOrdersRanges apiRequest <*>
      parse (pRequestSelect rootName) ("failed to parse select parameter <<" <> toS selStr <> ">>") (toS selStr)
      where
        selStr = iSelect apiRequest
        rootName = if action == ActionRead
          then rootTableName
          else sourceCTEName

    relations :: [Relation]
    relations = case action of
      ActionCreate -> fakeSourceRelations ++ allRels
      ActionUpdate -> fakeSourceRelations ++ allRels
      ActionDelete -> fakeSourceRelations ++ allRels
      ActionInvoke -> fakeSourceRelations ++ allRels
      _       -> allRels
      where fakeSourceRelations = mapMaybe (toSourceRelation rootTableName) allRels -- see comment in toSourceRelation

buildMutateRequest :: ApiRequest -> Either Text MutateRequest
buildMutateRequest apiRequest = case action of
  ActionCreate -> Insert rootTableName <$> pure payload
  ActionUpdate -> Update rootTableName <$> pure payload <*> filters
  ActionDelete -> Delete rootTableName <$> filters
  _        -> Left "Unsupported HTTP verb"
  where
    action = iAction apiRequest
    payload = fromJust $ iPayload apiRequest
    rootTableName = -- TODO: Make it safe
      let target = iTarget apiRequest in
      case target of
        (TargetIdent (QualifiedIdentifier _ t) ) -> t
        _ -> undefined
    filters = first formatParserError $ map snd <$> mapM pRequestFilter mutateFilters
      where mutateFilters = filter (not . ( "." `isInfixOf` ) . fst) $ iFilters apiRequest -- update/delete filters can be only on the root table

addFilterToNode :: Filter -> ReadRequest -> ReadRequest
addFilterToNode flt (Node (q@Select {flt_=flts}, i) f) = Node (q {flt_=flt:flts}, i) f

addFilter :: (Path, Filter) -> ReadRequest -> ReadRequest
addFilter = addProperty addFilterToNode

addOrderToNode :: [OrderTerm] -> ReadRequest -> ReadRequest
addOrderToNode o (Node (q,i) f) = Node (q{order=Just o}, i) f

addOrder :: (Path, [OrderTerm]) -> ReadRequest -> ReadRequest
addOrder = addProperty addOrderToNode

addRangeToNode :: NonnegRange -> ReadRequest -> ReadRequest
addRangeToNode r (Node (q,i) f) = Node (q{range_=r}, i) f

addRange :: (Path, NonnegRange) -> ReadRequest -> ReadRequest
addRange = addProperty addRangeToNode

addProperty :: (a -> ReadRequest -> ReadRequest) -> (Path, a) -> ReadRequest -> ReadRequest
addProperty f ([], a) n = f a n
addProperty f (path, a) (Node rn forest) =
  case targetNode of
    Nothing -> Node rn forest -- the property is silenty dropped in the Request does not contain the required path
    Just tn -> Node rn (addProperty f (remainingPath, a) tn:restForest)
  where
    targetNodeName:remainingPath = path
    (targetNode,restForest) = splitForest targetNodeName forest
    splitForest :: NodeName -> Forest ReadNode -> (Maybe ReadRequest, Forest ReadNode)
    splitForest name forst =
      case maybeNode of
        Nothing -> (Nothing,forest)
        Just node -> (Just node, delete node forest)
      where
        maybeNode :: Maybe ReadRequest
        maybeNode = find fnd forst
          where
            fnd :: ReadRequest -> Bool
            fnd (Node (_,(n,_,_)) _) = n == name

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

extractQueryResult :: Maybe ResultsWithCount -> ResultsWithCount
extractQueryResult = fromMaybe (Nothing, 0, [], "")
