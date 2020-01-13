{-|
Module      : PostgREST.App
Description : PostgREST main application

This module is in charge of mapping HTTP requests to PostgreSQL queries.
Some of its functionality includes:

- Mapping HTTP request methods to proper SQL statements. For example, a GET request is translated to executing a SELECT query in a read-only TRANSACTION.
- Producing HTTP Headers according to RFCs.
- Content Negotiation
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgREST.App (
  postgrest
, useConnOrPool
) where

import qualified Data.ByteString.Char8      as BS
import qualified Data.HashMap.Strict        as M
import qualified Data.List                  as L (union)
import qualified Data.Set                   as S
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction          as H
import qualified Hasql.Transaction          as HT
import qualified Hasql.Transaction.Sessions as HT

import Data.Function                        (id)
import Data.IORef                           (IORef, readIORef)
import Data.Time.Clock                      (UTCTime)
import Network.HTTP.Types.URI               (renderSimpleQuery)
import Network.Wai.Middleware.RequestLogger (logStdout)

import Control.Applicative
import Data.Maybe
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai

import Hasql.Connection           (Connection)
import qualified Hasql.Session as Hsq
import PostgREST.ApiRequest       (Action (..), ApiRequest (..),
                                   ContentType (..),
                                   InvokeMethod (..), Target (..),
                                   mutuallyAgreeable, userApiRequest)
import PostgREST.Auth             (containsRole, jwtClaims,
                                   parseSecret)
import PostgREST.Config           (AppConfig (..))
import PostgREST.DbRequestBuilder (mutateRequest, readRequest)
import PostgREST.DbStructure
import PostgREST.Error            (PgError (..), SimpleError (..),
                                   errorResponseFor, singularityError)
import PostgREST.Middleware
import PostgREST.OpenAPI
import PostgREST.Parsers          (pRequestColumns)
import PostgREST.QueryBuilder     (limitedQuery, mutateRequestToQuery,
                                   readRequestToCountQuery,
                                   readRequestToQuery,
                                   requestToCallProcQuery)
import PostgREST.RangeQuery       (allRange, contentRangeH,
                                   rangeStatusHeader)
import PostgREST.Statements       (callProcStatement,
                                   createExplainStatement,
                                   createReadStatement,
                                   createWriteStatement)
import PostgREST.Types
import Protolude                  hiding (Proxy, intercalate)

useConnOrPool :: Either Connection P.Pool -> Hsq.Session a -> IO (Either P.UsageError a)
useConnOrPool poolOrConn = either (\c -> fmap (first P.SessionError) . flip Hsq.run c) P.use poolOrConn

postgrest :: AppConfig -> IORef (Maybe DbStructure) -> Either Connection P.Pool -> IO UTCTime -> IO () -> Application
postgrest conf refDbStructure poolOrConn getTime worker =
  let middle = (if configQuiet conf then id else logStdout) . defaultMiddle
      jwtSecret = parseSecret <$> configJwtSecret conf in
  middle $ \ req respond -> do
    time <- getTime
    body <- strictRequestBody req
    maybeDbStructure <- readIORef refDbStructure
    case maybeDbStructure of
      Nothing -> respond . errorResponseFor $ ConnectionLostError
      Just dbStructure -> do
        response <- do
          -- Need to parse ?columns early because findProc needs it to solve overloaded functions
          let apiReq = userApiRequest (configSchema conf) (configRootSpec conf) req body
              apiReqCols = (,) <$> apiReq <*> (pRequestColumns =<< iColumns <$> apiReq)
          case apiReqCols of
            Left err -> return . errorResponseFor $ err
            Right (apiRequest, maybeCols) -> do
              eClaims <- jwtClaims jwtSecret (configJwtAudience conf) (toS $ iJWT apiRequest) time (rightToMaybe $ configRoleClaimKey conf)
              let authed = containsRole eClaims
                  cols = case (iPayload apiRequest, maybeCols) of
                    (Just ProcessedJSON{pjKeys}, _) -> pjKeys
                    (Just RawJSON{}, Just cls)      -> cls
                    _                               -> S.empty
                  proc = case iTarget apiRequest of
                    TargetProc qi _ -> findProc qi cols (iPreferParameters apiRequest == Just SingleObject) $ dbProcs dbStructure
                    _ -> Nothing
                  handleReq = runWithClaims conf eClaims (app dbStructure proc cols conf) apiRequest
                  txMode = transactionMode proc (iAction apiRequest)
              response <- useConnOrPool poolOrConn $ HT.transaction HT.ReadCommitted txMode handleReq
              return $ either (errorResponseFor . PgError authed) identity response
        when (responseStatus response == status503) worker
        respond response

transactionMode :: Maybe ProcDescription -> Action -> HT.Mode
transactionMode proc action =
  case action of
    ActionRead _         -> HT.Read
    ActionInfo           -> HT.Read
    ActionInspect _      -> HT.Read
    ActionInvoke InvGet  -> HT.Read
    ActionInvoke InvHead -> HT.Read
    ActionInvoke InvPost ->
      let v = maybe Volatile pdVolatility proc in
      if v == Stable || v == Immutable
         then HT.Read
         else HT.Write
    _ -> HT.Write

app :: DbStructure -> Maybe ProcDescription -> S.Set FieldName -> AppConfig -> ApiRequest -> H.Transaction Response
app dbStructure proc cols conf apiRequest =
  let rawContentTypes = (decodeContentType <$> configRawMediaTypes conf) `L.union` [ CTOctetStream, CTTextPlain ] in
  case responseContentTypeOrError (iAccepts apiRequest) rawContentTypes (iAction apiRequest) (iTarget apiRequest) of
    Left errorResponse -> return errorResponse
    Right contentType ->
      case (iAction apiRequest, iTarget apiRequest, iPayload apiRequest) of

        (ActionRead headersOnly, TargetIdent (QualifiedIdentifier tSchema tName), Nothing) ->
          case readSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (q, cq, bField) -> do
              let cQuery = if estimatedCount
                             then limitedQuery cq ((+ 1) <$> maxRows) -- LIMIT maxRows + 1 so we can determine below that maxRows was surpassed
                             else cq
                  stm = createReadStatement q cQuery (contentType == CTSingularJSON) shouldCount
                        (contentType == CTTextCSV) bField pgVer
                  explStm = createExplainStatement cq
              row <- H.statement () stm
              let (tableTotal, queryTotal, _ , body, gucHeaders) = row
              case gucHeaders of
                Left _ -> return . errorResponseFor $ GucHeadersError
                Right hs -> do
                  total <- if | plannedCount   -> H.statement () explStm
                              | estimatedCount -> if tableTotal > (fromIntegral <$> maxRows)
                                                    then do estTotal <- H.statement () explStm
                                                            pure $ if estTotal > tableTotal then estTotal else tableTotal
                                                    else pure tableTotal
                              | otherwise      -> pure tableTotal
                  let (status, contentRange) = rangeStatusHeader topLevelRange queryTotal total
                  return $
                    if contentType == CTSingularJSON && queryTotal /= 1
                      then errorResponseFor . singularityError $ queryTotal
                      else responseLBS status
                        ([toHeader contentType, contentRange, contentLocationH tName (iCanonicalQS apiRequest)] ++ (gucHToHeader <$> hs))
                        (if headersOnly then mempty else toS body)

        (ActionCreate, TargetIdent (QualifiedIdentifier tSchema tName), Just pJson) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let pkCols = tablePKCols dbStructure tSchema tName
                  stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) True
                    (contentType == CTTextCSV) (iPreferRepresentation apiRequest) pkCols pgVer
              row <- H.statement (toS $ pjRaw pJson) stm
              let (_, queryTotal, fs, body, gucHeaders) = row
              case gucHeaders of
                Left _ -> return . errorResponseFor $ GucHeadersError
                Right hdrs -> do
                  let headers = catMaybes [
                          if null fs
                            then Nothing
                            else Just $ locationH tName fs
                        , if iPreferRepresentation apiRequest == Full
                            then Just $ toHeader contentType
                            else Nothing
                        , Just $ contentRangeH 1 0 $
                            if shouldCount then Just queryTotal else Nothing
                        , if null pkCols && isNothing (iOnConflict apiRequest)
                            then Nothing
                            else (\x -> ("Preference-Applied", show x)) <$> iPreferResolution apiRequest
                        ] ++ (gucHToHeader <$> hdrs)
                  if contentType == CTSingularJSON && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return . responseLBS status201 headers $ if iPreferRepresentation apiRequest == Full then toS body else mempty

        (ActionUpdate, TargetIdent (QualifiedIdentifier tSchema tName), Just pJson) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False (contentType == CTTextCSV)
                    (iPreferRepresentation apiRequest) [] pgVer
              row <- H.statement (toS $ pjRaw pJson) stm
              let (_, queryTotal, _, body, gucHeaders) = row
              case gucHeaders of
                Left _ -> return . errorResponseFor $ GucHeadersError
                Right hdrs -> do
                  let
                    updateIsNoOp = S.null cols
                    status | queryTotal == 0 && not updateIsNoOp      = status404
                           | iPreferRepresentation apiRequest == Full = status200
                           | otherwise                                = status204
                    contentRangeHeader = contentRangeH 0 (queryTotal - 1) $ if shouldCount then Just queryTotal else Nothing
                    headers = [contentRangeHeader] ++
                              [if iPreferRepresentation apiRequest == Full then toHeader contentType else mempty] ++
                              (gucHToHeader <$> hdrs)
                  if contentType == CTSingularJSON && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return . responseLBS status headers $ if iPreferRepresentation apiRequest == Full then toS body else mempty

        (ActionSingleUpsert, TargetIdent (QualifiedIdentifier tSchema tName), Just ProcessedJSON{pjRaw, pjType, pjKeys}) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let isSingle = case pjType of
                               PJArray len -> len == 1
                               PJObject    -> True
                  colNames = colName <$> tableCols dbStructure tSchema tName
              if topLevelRange /= allRange
                then return . errorResponseFor $ PutRangeNotAllowedError
              else if not isSingle
                then return . errorResponseFor $ PutSingletonError
              else if S.fromList colNames /= pjKeys
                then return . errorResponseFor $ PutPayloadIncompleteError
              else do
                row <- H.statement (toS pjRaw) $
                       createWriteStatement sq mq (contentType == CTSingularJSON) False
                                            (contentType == CTTextCSV) (iPreferRepresentation apiRequest) [] pgVer
                let (_, queryTotal, _, body, gucHeaders) = row
                case gucHeaders of
                  Left _ -> return . errorResponseFor $ GucHeadersError
                  Right hdrs -> do
                    let headers = toHeader contentType : (gucHToHeader <$> hdrs)
                        status = if iPreferRepresentation apiRequest == Full then status200 else status204
                    -- Makes sure the querystring pk matches the payload pk
                    -- e.g. PUT /items?id=eq.1 { "id" : 1, .. } is accepted, PUT /items?id=eq.14 { "id" : 2, .. } is rejected
                    -- If this condition is not satisfied then nothing is inserted, check the WHERE for INSERT in QueryBuilder.hs to see how it's done
                    if queryTotal /= 1
                      then do
                        HT.condemn
                        return . errorResponseFor $ PutMatchingPkError
                      else
                        return . responseLBS status headers $ if iPreferRepresentation apiRequest == Full then toS body else mempty

        (ActionDelete, TargetIdent (QualifiedIdentifier tSchema tName), Nothing) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False
                    (contentType == CTTextCSV)
                    (iPreferRepresentation apiRequest) [] pgVer
              row <- H.statement mempty stm
              let (_, queryTotal, _, body, gucHeaders) = row
              case gucHeaders of
                Left _ -> return . errorResponseFor $ GucHeadersError
                Right hdrs -> do
                  let
                    status = if iPreferRepresentation apiRequest == Full then status200 else status204
                    contentRangeHeader = contentRangeH 1 0 $ if shouldCount then Just queryTotal else Nothing
                    headers = [contentRangeHeader] ++
                              [if iPreferRepresentation apiRequest == Full then toHeader contentType else mempty] ++
                              (gucHToHeader <$> hdrs)
                  if contentType == CTSingularJSON
                     && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return . responseLBS status headers $ if iPreferRepresentation apiRequest == Full then toS body else mempty

        (ActionInfo, TargetIdent (QualifiedIdentifier tSchema tTable), Nothing) ->
          let mTable = find (\t -> tableName t == tTable && tableSchema t == tSchema) (dbTables dbStructure) in
          case mTable of
            Nothing -> return notFound
            Just table ->
              let allowH = (hAllow, if tableInsertable table then "GET,POST,PATCH,DELETE" else "GET")
                  allOrigins = ("Access-Control-Allow-Origin", "*") :: Header in
              return $ responseLBS status200 [allOrigins, allowH] mempty

        (ActionInvoke invMethod, TargetProc qi@(QualifiedIdentifier tSchema pName) _, Just pJson) ->
          let tName = fromMaybe pName $ procTableName =<< proc in
          case readSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (q, cq, bField) -> do
              let
                preferParams = iPreferParameters apiRequest
                pq = requestToCallProcQuery qi (specifiedProcArgs cols proc) returnsScalar preferParams
                stm = callProcStatement returnsScalar pq q cq shouldCount (contentType == CTSingularJSON)
                        (contentType == CTTextCSV) (contentType `elem` rawContentTypes) (preferParams == Just MultipleObjects)
                        bField pgVer
              row <- H.statement (toS $ pjRaw pJson) stm
              let (tableTotal, queryTotal, body, gucHeaders) = row
                  (status, contentRange) = rangeStatusHeader topLevelRange queryTotal tableTotal
              case gucHeaders of
                Left _ -> return . errorResponseFor $ GucHeadersError
                Right hs ->
                  if contentType == CTSingularJSON && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return $ responseLBS status ([toHeader contentType, contentRange] ++ (gucHToHeader <$> hs))
                      (if invMethod == InvHead then mempty else toS body)

        (ActionInspect headersOnly, TargetDefaultSpec tSchema, Nothing) -> do
          let host = configHost conf
              port = toInteger $ configPort conf
              proxy = pickProxy $ toS <$> configProxyUri conf
              uri Nothing = ("http", host, port, "/")
              uri (Just Proxy { proxyScheme = s, proxyHost = h, proxyPort = p, proxyPath = b }) = (s, h, p, b)
              uri' = uri proxy
              toTableInfo :: [Table] -> [(Table, [Column], [Text])]
              toTableInfo = map (\t -> let (s, tn) = (tableSchema t, tableName t) in (t, tableCols dbStructure s tn, tablePKCols dbStructure s tn))
              encodeApi ti sd procs = encodeOpenAPI (concat $ M.elems procs) (toTableInfo ti) uri' sd $ dbPrimaryKeys dbStructure

          body <- encodeApi <$>
            H.statement tSchema accessibleTables <*>
            H.statement tSchema schemaDescription <*>
            H.statement tSchema accessibleProcs
          return $ responseLBS status200 [toHeader CTOpenAPI] (if headersOnly then mempty else toS body)

        _ -> return notFound

      where
        notFound = responseLBS status404 [] ""
        maxRows = configMaxRows conf
        exactCount = iPreferCount apiRequest == Just ExactCount
        estimatedCount = iPreferCount apiRequest == Just EstimatedCount
        plannedCount = iPreferCount apiRequest == Just PlannedCount
        shouldCount = exactCount || estimatedCount
        topLevelRange = iTopLevelRange apiRequest
        returnsScalar = maybe False procReturnsScalar proc
        pgVer = pgVersion dbStructure

        readSqlParts s t =
          let
            readReq = readRequest s t maxRows (dbRelations dbStructure) apiRequest
          in
          (,,) <$>
          (readRequestToQuery <$> readReq) <*>
          (readRequestToCountQuery <$> readReq) <*>
          (binaryField contentType rawContentTypes returnsScalar =<< readReq)

        mutateSqlParts s t =
          let
            readReq = readRequest s t maxRows (dbRelations dbStructure) apiRequest
            mutReq = mutateRequest s t apiRequest cols (tablePKCols dbStructure s t) =<< readReq
          in
          (,) <$>
          (readRequestToQuery <$> readReq) <*>
          (mutateRequestToQuery <$> mutReq)

responseContentTypeOrError :: [ContentType] -> [ContentType] -> Action -> Target -> Either Response ContentType
responseContentTypeOrError accepts rawContentTypes action target = serves contentTypesForRequest accepts
  where
    contentTypesForRequest = case action of
      ActionRead _       ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
                             ++ rawContentTypes
      ActionCreate       ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
      ActionUpdate       ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
      ActionDelete       ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
      ActionInvoke _     ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
                             ++ rawContentTypes
                             ++ [CTOpenAPI | tpIsRootSpec target]
      ActionInspect _    ->  [CTOpenAPI, CTApplicationJSON]
      ActionInfo         ->  [CTTextCSV]
      ActionSingleUpsert ->  [CTApplicationJSON, CTSingularJSON, CTTextCSV]
    serves sProduces cAccepts =
      case mutuallyAgreeable sProduces cAccepts of
        Nothing -> Left . errorResponseFor . ContentTypeError . map toMime $ cAccepts
        Just ct -> Right ct

{-
  | If raw(binary) output is requested, check that ContentType is one of the admitted rawContentTypes and that
  | `?select=...` contains only one field other than `*`
-}
binaryField :: ContentType -> [ContentType] -> Bool -> ReadRequest -> Either Response (Maybe FieldName)
binaryField ct rawContentTypes isScalarProc readReq
  | isScalarProc = Right Nothing
  | ct `elem` rawContentTypes =
      let fieldName = headMay fldNames in
      if length fldNames == 1 && fieldName /= Just "*"
        then Right fieldName
        else Left . errorResponseFor $ BinaryFieldError ct
  | otherwise = Right Nothing
  where
    fldNames = fstFieldNames readReq

locationH :: TableName -> [BS.ByteString] -> Header
locationH tName fields =
  let
    locationFields = renderSimpleQuery True $ map splitKeyValue fields
  in
    (hLocation, "/" <> toS tName <> locationFields)
  where
    splitKeyValue :: BS.ByteString -> (BS.ByteString, BS.ByteString)
    splitKeyValue kv =
      let (k, v) = BS.break (== '=') kv
      in (k, BS.tail v)

contentLocationH :: TableName -> ByteString -> Header
contentLocationH tName qString =
  ("Content-Location", "/" <> toS tName <> if BS.null qString then mempty else "?" <> toS qString)
