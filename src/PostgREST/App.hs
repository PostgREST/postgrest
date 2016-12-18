{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
--module PostgREST.App where
module PostgREST.App (
  postgrest
) where

import           Control.Applicative
import qualified Data.ByteString.Char8     as BS
import           Data.IORef                (IORef, readIORef)
import           Data.List                 (delete, lookup)
import           Data.Maybe                (fromJust)
import           Data.Text                 (replace, strip, isInfixOf, dropWhile, drop, intercalate)
import           Data.Time.Clock.POSIX     (POSIXTime)
import           Data.Tree
import           Data.Either.Combinators   (mapLeft)

import qualified Hasql.Pool                as P
import qualified Hasql.Transaction         as HT

import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec (parse)

import qualified Text.InterpolatedString.Perl6 as P6 (q)

import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.URI    (renderSimpleQuery)
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.JWT                   (binarySecret)

import           Data.Aeson
import           Data.Aeson.Types          (emptyArray)
import qualified Data.Vector               as V
import qualified Hasql.Transaction         as H

import qualified Data.HashMap.Strict       as M

import           PostgREST.ApiRequest   ( ApiRequest(..), ContentType(..)
                                        , Action(..), Target(..)
                                        , PreferRepresentation (..)
                                        , mutuallyAgreeable
                                        , toHeader
                                        , userApiRequest
                                        , toMime
                                        )
import           PostgREST.Auth            (jwtClaims, containsRole)
import           PostgREST.Config          (AppConfig (..))
import           PostgREST.DbStructure
import           PostgREST.Error           (errResponse, pgErrResponse, apiRequestErrResponse)
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
                                        , returningF
                                        )
import           PostgREST.Types
import           PostgREST.OpenAPI

import           Data.Foldable (foldr1)
import           Data.Function (id)
import           Protolude                hiding (dropWhile, drop, intercalate, Proxy)

postgrest :: AppConfig -> IORef DbStructure -> P.Pool -> IO POSIXTime ->
             Application
postgrest conf refDbStructure pool getTime =
  let middle = (if configQuiet conf then id else logStdout) . defaultMiddle in

  middle $ \ req respond -> do
    time <- getTime
    body <- strictRequestBody req
    dbStructure <- readIORef refDbStructure

    response <- case userApiRequest (configSchema conf) req body of
      Left err -> return $ apiRequestErrResponse err
      Right apiRequest -> do
        let jwtSecret = binarySecret <$> configJwtSecret conf
            eClaims = jwtClaims jwtSecret (iJWT apiRequest) time
            authed = containsRole eClaims
            handleReq = runWithClaims conf eClaims (app dbStructure conf) apiRequest
            txMode = transactionMode $ iAction apiRequest
        response <- P.use pool $ HT.run handleReq HT.ReadCommitted txMode
        return $ either (pgErrResponse authed) identity response
    respond response

transactionMode :: Action -> H.Mode
transactionMode ActionRead = HT.Read
transactionMode ActionInfo = HT.Read
transactionMode _ = HT.Write

app :: DbStructure -> AppConfig -> ApiRequest -> H.Transaction Response
app dbStructure conf apiRequest =
  case responseContentTypeOrError (iAccepts apiRequest) (iAction apiRequest) of
    Left errorResponse -> return errorResponse
    Right contentType ->
      case (iAction apiRequest, iTarget apiRequest, iPayload apiRequest) of

        (ActionRead, TargetIdent qi, Nothing) ->
          case readSqlParts of
            Left errorResponse -> return errorResponse
            Right (q, cq) -> do
              let singular = iPreferSingular apiRequest
                  stm = createReadStatement q cq singular shouldCount (contentType == CTTextCSV)
              row <- H.query () stm
              let (tableTotal, queryTotal, _ , body) = row
              if singular
              then return $ if queryTotal <= 0
                then notFound
                else responseLBS status200 [toHeader contentType] (toS body)
              else do
                let (status, contentRange) = rangeHeader queryTotal tableTotal
                    canonical = iCanonicalQS apiRequest
                    --TargetIdent qi = iTarget apiRequest
                return $ responseLBS status
                  [toHeader contentType, contentRange,
                    ("Content-Location",
                      "/" <> toS (qiName qi) <>
                        if BS.null canonical then "" else "?" <> toS canonical
                    )
                  ] (toS body)

        (ActionCreate, TargetIdent qi@(QualifiedIdentifier _ table), Just payload@(PayloadJSON rows)) ->
          case mutateSqlParts of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
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
              row <- H.query payload stm
              let (_, _, fs, body) = extractQueryResult row
                  headers = catMaybes [
                      if null fs
                        then Nothing
                        else Just (hLocation, "/" <> toS table <> renderLocationFields fs)
                    , if iPreferRepresentation apiRequest == Full
                        then Just $ toHeader contentType
                        else Nothing
                    , Just . contentRangeH 1 0 $
                        toInteger <$> if shouldCount then Just (V.length rows) else Nothing
                    ]

              return . responseLBS status201 headers $
                if iPreferRepresentation apiRequest == Full
                  then toS body else ""

        (ActionUpdate, TargetIdent qi, Just payload) ->
          case mutateSqlParts of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let singular = iPreferSingular apiRequest
                  stm = createWriteStatement qi sq mq singular (iPreferRepresentation apiRequest) [] (contentType == CTTextCSV) payload
              row <- H.query payload stm
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
                then responseLBS s [toHeader contentType, r] (toS body)
                else responseLBS s [r] ""

        (ActionDelete, TargetIdent qi, Nothing) ->
          case mutateSqlParts of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let emptyPayload = PayloadJSON V.empty
                  stm = createWriteStatement qi sq mq False (iPreferRepresentation apiRequest) [] (contentType == CTTextCSV) emptyPayload
              row <- H.query emptyPayload stm
              let (_, queryTotal, _, body) = extractQueryResult row
                  r = contentRangeH 1 0 $
                        toInteger <$> if shouldCount then Just queryTotal else Nothing
              return $ if queryTotal == 0
                then notFound
                else if iPreferRepresentation apiRequest == Full
                  then responseLBS status200 [toHeader contentType, r] (toS body)
                  else responseLBS status204 [r] ""

        (ActionInfo, TargetIdent (QualifiedIdentifier tSchema tTable), Nothing) ->
          let mTable = find (\t -> tableName t == tTable && tableSchema t == tSchema) (dbTables dbStructure) in
          case mTable of
            Nothing -> return notFound
            Just table ->
              let acceptH = (hAllow, if tableInsertable table then "GET,POST,PATCH,DELETE" else "GET") in
              return $ responseLBS status200 [allOrigins, acceptH] ""

        (ActionInvoke, TargetProc qi, Just (PayloadJSON payload)) ->
          case readSqlParts of
            Left errorResponse -> return errorResponse
            Right (q, cq) -> do
              let p = V.head payload
                  singular = iPreferSingular apiRequest
                  paramsAsSingleObject = iPreferSingleObjectParameter apiRequest
              row <- H.query () (callProc qi p q cq topLevelRange shouldCount singular paramsAsSingleObject)
              let (tableTotal, queryTotal, body) =
                    fromMaybe (Just 0, 0, emptyArray) row
                  (status, contentRange) = rangeHeader queryTotal tableTotal
              return $ responseLBS status [jsonH, contentRange] (toS . encode $ body)

        (ActionInspect, TargetRoot, Nothing) -> do
          let host = configHost conf
              port = toInteger $ configPort conf
              proxy = pickProxy $ toS <$> configProxyUri conf
              uri Nothing = ("http", host, port, "/")
              uri (Just Proxy { proxyScheme = s, proxyHost = h, proxyPort = p, proxyPath = b }) = (s, h, p, b)
              uri' = uri proxy
              encodeApi ti = encodeOpenAPI (map snd $ dbProcs dbStructure) ti uri'
          body <- encodeApi . toTableInfo <$> H.query schema accessibleTables
          return $ responseLBS status200 [toHeader CTOpenAPI] $ toS body

        _ -> return notFound

    where
      toTableInfo :: [Table] -> [(Table, [Column], [Text])]
      toTableInfo = map (\t ->
        let tSchema = tableSchema t
            tTable = tableName t
            cols = filter (filterCol tSchema tTable) $ dbColumns dbStructure
            pkeys = map pkName $ filter (filterPk tSchema tTable) allPrKeys
        in (t, cols, pkeys))
      notFound = responseLBS status404 [] ""
      filterPk sc table pk = sc == (tableSchema . pkTable) pk && table == (tableName . pkTable) pk
      filterCol :: Schema -> TableName -> Column -> Bool
      filterCol sc tb Column{colTable=Table{tableSchema=s, tableName=t}} = s==sc && t==tb
      filterCol _ _ _ =  False
      allPrKeys = dbPrimaryKeys dbStructure
      allOrigins = ("Access-Control-Allow-Origin", "*") :: Header
      jsonH = toHeader CTApplicationJSON
      shouldCount = iPreferCount apiRequest
      schema = toS $ configSchema conf
      topLevelRange = fromMaybe allRange $ M.lookup "limit" $ iRange apiRequest
      rangeHeader queryTotal tableTotal =
        let lower = rangeOffset topLevelRange
            upper = lower + toInteger queryTotal - 1
            contentRange = contentRangeH lower upper (toInteger <$> tableTotal)
            status = rangeStatus lower upper (toInteger <$> tableTotal)
        in (status, contentRange)

      mapSnd f (a, b) = (a, f b)
      readDbRequest = DbRead <$> readRequest (configMaxRows conf) (dbRelations dbStructure) (map (mapSnd pdReturnType) $ dbProcs dbStructure) apiRequest
      mutateDbRequest = DbMutate <$> mutateRequest apiRequest
      returningSql = returningF (iTarget apiRequest) (iPreferRepresentation apiRequest) <$> readDbRequest
      selectQuery = requestToQuery schema False "" <$> readDbRequest
      mutateQuery = requestToQuery schema False <$> returningSql <*> mutateDbRequest
      countQuery = requestToCountQuery schema <$> readDbRequest
      readSqlParts = (,) <$> selectQuery <*> countQuery
      mutateSqlParts = (,) <$> selectQuery <*> mutateQuery

responseContentTypeOrError :: [ContentType] -> Action -> Either Response ContentType
responseContentTypeOrError accepts action = serves contentTypesForRequest accepts
  where
    contentTypesForRequest =
      case action of
        ActionRead -> [CTApplicationJSON, CTTextCSV]
        ActionCreate -> [CTApplicationJSON, CTTextCSV]
        ActionUpdate -> [CTApplicationJSON, CTTextCSV]
        ActionDelete -> [CTApplicationJSON, CTTextCSV]
        ActionInvoke -> [CTApplicationJSON]
        ActionInspect -> [CTOpenAPI]
        ActionInfo -> [CTTextCSV]
    serves sProduces cAccepts =
      case mutuallyAgreeable sProduces cAccepts of
        Nothing -> do
          let failed = intercalate ", " $ map (toS . toMime) cAccepts
          Left $ errResponse status415 $
            "None of these Content-Types are available: " <> failed
        Just ct -> Right ct


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

formatParserError :: ParseError -> Text
formatParserError e = formatGeneralError message details
  where
     message = show $ errorPos e
     details = strip $ replace "\n" " " $ toS
       $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)

formatGeneralError :: Text -> Text -> Text
formatGeneralError message details = message <> ", " <> details

augumentRequestWithJoin :: Schema ->  [Relation] ->  ReadRequest -> Either Text ReadRequest
augumentRequestWithJoin schema allRels request =
  (first formatRelationError . addRelations schema allRels Nothing) request
  >>= addJoinConditions schema
  where
    formatRelationError = formatGeneralError
      "could not find foreign keys between these entities"

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

readRequest :: Maybe Integer -> [Relation] -> [(Text, Text)] -> ApiRequest -> Either Response ReadRequest
readRequest maxRows allRels allProcs apiRequest  =
  mapLeft (errResponse status400) $
  treeRestrictRange maxRows =<<
  augumentRequestWithJoin schema relations =<<
  first formatParserError parseReadRequest
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

    parseReadRequest :: Either ParseError ReadRequest
    parseReadRequest = addFiltersOrdersRanges apiRequest <*>
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

mutateRequest :: ApiRequest -> Either Response MutateRequest
mutateRequest apiRequest = mapLeft (errResponse status400) $
  case action of
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
