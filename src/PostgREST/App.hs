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
) where

import qualified Data.ByteString.Char8      as BS
import qualified Data.HashMap.Strict        as M
import qualified Data.List                  as L (union)
import qualified Data.Set                   as S
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction          as H
import qualified Hasql.Transaction          as HT
import qualified Hasql.Transaction.Sessions as HT

import Data.IORef             (IORef, readIORef)
import Data.Time.Clock        (UTCTime)
import Network.HTTP.Types.URI (renderSimpleQuery)

import Control.Applicative
import Data.Maybe
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai

import PostgREST.ApiRequest       (Action (..), ApiRequest (..),
                                   InvokeMethod (..), Target (..),
                                   mutuallyAgreeable, userApiRequest)
import PostgREST.Auth             (attemptJwtClaims, containsRole,
                                   jwtClaims)
import PostgREST.Config           (AppConfig (..))
import PostgREST.DbRequestBuilder (mutateRequest, readRequest,
                                   returningCols)
import PostgREST.DbStructure
import PostgREST.Error            (PgError (..), SimpleError (..),
                                   errorResponseFor, singularityError)
import PostgREST.Middleware
import PostgREST.OpenAPI
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
import Protolude                  hiding (Proxy, intercalate, toS)
import Protolude.Conv             (toS)

postgrest :: LogLevel -> IORef AppConfig -> IORef (Maybe DbStructure) -> P.Pool -> IO UTCTime -> IO () -> Application
postgrest logLev refConf refDbStructure pool getTime connWorker =
  pgrstMiddleware logLev $ \ req respond -> do
    time <- getTime
    body <- strictRequestBody req
    maybeDbStructure <- readIORef refDbStructure
    conf <- readIORef refConf
    case maybeDbStructure of
      Nothing -> respond . errorResponseFor $ ConnectionLostError
      Just dbStructure -> do
        response <- do
          let apiReq = userApiRequest (configDbSchemas conf) (configDbRootSpec conf) dbStructure req body
          case apiReq of
            Left err -> return . errorResponseFor $ err
            Right apiRequest -> do
              -- The jwt must be checked before touching the db.
              attempt <- attemptJwtClaims (configJWKS conf) (configJwtAudience conf) (toS $ iJWT apiRequest) time (rightToMaybe $ configJwtRoleClaimKey conf)
              case jwtClaims attempt of
                Left errJwt -> return . errorResponseFor $ errJwt
                Right claims -> do
                  let
                    authed = containsRole claims
                    shouldCommit   = configDbTxAllowOverride conf && iPreferTransaction apiRequest == Just Commit
                    shouldRollback = configDbTxAllowOverride conf && iPreferTransaction apiRequest == Just Rollback
                    preferenceApplied
                      | shouldCommit    = addHeadersIfNotIncluded [(hPreferenceApplied, BS.pack (show Commit))]
                      | shouldRollback  = addHeadersIfNotIncluded [(hPreferenceApplied, BS.pack (show Rollback))]
                      | otherwise       = identity
                    handleReq = do
                      when (shouldRollback || (configDbTxRollbackAll conf && not shouldCommit)) HT.condemn
                      mapResponseHeaders preferenceApplied <$> runPgLocals conf claims (app dbStructure conf) apiRequest
                  dbResp <- P.use pool $ HT.transaction HT.ReadCommitted (txMode apiRequest) handleReq
                  return $ either (errorResponseFor . PgError authed) identity dbResp
        -- Launch the connWorker when the connection is down. The postgrest function can respond successfully(with a stale schema cache) before the connWorker is done.
        when (responseStatus response == status503) connWorker
        respond response

txMode :: ApiRequest -> HT.Mode
txMode apiRequest =
  case (iAction apiRequest, iTarget apiRequest) of
    (ActionRead _        , _) -> HT.Read
    (ActionInfo          , _) -> HT.Read
    (ActionInspect _     , _) -> HT.Read
    (ActionInvoke InvGet , _) -> HT.Read
    (ActionInvoke InvHead, _) -> HT.Read
    (ActionInvoke InvPost, TargetProc ProcDescription{pdVolatility=Stable} _)    -> HT.Read
    (ActionInvoke InvPost, TargetProc ProcDescription{pdVolatility=Immutable} _) -> HT.Read
    _ -> HT.Write

app :: DbStructure -> AppConfig -> ApiRequest -> H.Transaction Response
app dbStructure conf apiRequest =
  let rawContentTypes = (decodeContentType <$> configRawMediaTypes conf) `L.union` [ CTOctetStream, CTTextPlain ] in
  case responseContentTypeOrError (iAccepts apiRequest) rawContentTypes (iAction apiRequest) (iTarget apiRequest) of
    Left errorResponse -> return errorResponse
    Right contentType ->
      case (iAction apiRequest, iTarget apiRequest) of

        (ActionRead headersOnly, TargetIdent (QualifiedIdentifier tSchema tName)) ->
          case readSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (q, cq, bField, _) -> do
              let cQuery = if estimatedCount
                             then limitedQuery cq ((+ 1) <$> maxRows) -- LIMIT maxRows + 1 so we can determine below that maxRows was surpassed
                             else cq
                  stm = createReadStatement q cQuery (contentType == CTSingularJSON) shouldCount
                        (contentType == CTTextCSV) bField pgVer prepared
                  explStm = createExplainStatement cq prepared
              row <- H.statement mempty stm
              let (tableTotal, queryTotal, _ , body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  total <- if | plannedCount   -> H.statement () explStm
                              | estimatedCount -> if tableTotal > (fromIntegral <$> maxRows)
                                                    then do estTotal <- H.statement () explStm
                                                            pure $ if estTotal > tableTotal then estTotal else tableTotal
                                                    else pure tableTotal
                              | otherwise      -> pure tableTotal
                  let (rangeStatus, contentRange) = rangeStatusHeader topLevelRange queryTotal total
                      status = fromMaybe rangeStatus gstatus
                      headers = addHeadersIfNotIncluded (catMaybes [
                                  Just $ toHeader contentType, Just contentRange,
                                  Just $ contentLocationH tName (iCanonicalQS apiRequest), profileH])
                                (unwrapGucHeader <$> ghdrs)
                      rBody = if headersOnly then mempty else toS body
                  return $
                    if contentType == CTSingularJSON && queryTotal /= 1
                      then errorResponseFor . singularityError $ queryTotal
                      else responseLBS status headers rBody

        (ActionCreate, TargetIdent (QualifiedIdentifier tSchema tName)) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let pkCols = tablePKCols dbStructure tSchema tName
                  stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) True
                    (contentType == CTTextCSV) (iPreferRepresentation apiRequest) pkCols pgVer prepared
              row <- H.statement mempty stm
              let (_, queryTotal, fields, body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  let
                    (ctHeaders, rBody) = if iPreferRepresentation apiRequest == Full
                                          then ([Just $ toHeader contentType, profileH], toS body)
                                          else ([], mempty)
                    status = fromMaybe status201 gstatus
                    headers = addHeadersIfNotIncluded (catMaybes ([
                          if null fields
                            then Nothing
                            else Just $ locationH tName fields
                        , Just $ contentRangeH 1 0 $ if shouldCount then Just queryTotal else Nothing
                        , if null pkCols && isNothing (iOnConflict apiRequest)
                            then Nothing
                            else (\x -> ("Preference-Applied", BS.pack (show x))) <$> iPreferResolution apiRequest
                        ] ++ ctHeaders)) (unwrapGucHeader <$> ghdrs)
                  if contentType == CTSingularJSON && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return $ responseLBS status headers rBody

        (ActionUpdate, TargetIdent (QualifiedIdentifier tSchema tName)) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              row <- H.statement mempty $
                     createWriteStatement sq mq
                       (contentType == CTSingularJSON) False (contentType == CTTextCSV)
                       (iPreferRepresentation apiRequest) [] pgVer prepared
              let (_, queryTotal, _, body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  let
                    updateIsNoOp = S.null (iColumns apiRequest)
                    defStatus | queryTotal == 0 && not updateIsNoOp      = status404
                              | iPreferRepresentation apiRequest == Full = status200
                              | otherwise                                = status204
                    status = fromMaybe defStatus gstatus
                    contentRangeHeader = contentRangeH 0 (queryTotal - 1) $ if shouldCount then Just queryTotal else Nothing
                    (ctHeaders, rBody) = if iPreferRepresentation apiRequest == Full
                                          then ([Just $ toHeader contentType, profileH], toS body)
                                          else ([], mempty)
                    headers = addHeadersIfNotIncluded (catMaybes ctHeaders ++ [contentRangeHeader]) (unwrapGucHeader <$> ghdrs)
                  if contentType == CTSingularJSON && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return $ responseLBS status headers rBody

        (ActionSingleUpsert, TargetIdent (QualifiedIdentifier tSchema tName)) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) ->
              if topLevelRange /= allRange
                then return . errorResponseFor $ PutRangeNotAllowedError
              else do
                row <- H.statement mempty $
                       createWriteStatement sq mq (contentType == CTSingularJSON) False
                                            (contentType == CTTextCSV) (iPreferRepresentation apiRequest) [] pgVer prepared
                let (_, queryTotal, _, body, gucHeaders, gucStatus) = row
                    gucs =  (,) <$> gucHeaders <*> gucStatus
                case gucs of
                  Left err -> return $ errorResponseFor err
                  Right (ghdrs, gstatus) -> do
                    let headers = addHeadersIfNotIncluded (catMaybes [Just $ toHeader contentType, profileH]) (unwrapGucHeader <$> ghdrs)
                        (defStatus, rBody) = if iPreferRepresentation apiRequest == Full then (status200, toS body) else (status204, mempty)
                        status = fromMaybe defStatus gstatus
                    -- Makes sure the querystring pk matches the payload pk
                    -- e.g. PUT /items?id=eq.1 { "id" : 1, .. } is accepted, PUT /items?id=eq.14 { "id" : 2, .. } is rejected
                    -- If this condition is not satisfied then nothing is inserted, check the WHERE for INSERT in QueryBuilder.hs to see how it's done
                    if queryTotal /= 1
                      then do
                        HT.condemn
                        return . errorResponseFor $ PutMatchingPkError
                      else
                        return $ responseLBS status headers rBody

        (ActionDelete, TargetIdent (QualifiedIdentifier tSchema tName)) ->
          case mutateSqlParts tSchema tName of
            Left errorResponse -> return errorResponse
            Right (sq, mq) -> do
              let stm = createWriteStatement sq mq
                    (contentType == CTSingularJSON) False
                    (contentType == CTTextCSV)
                    (iPreferRepresentation apiRequest) [] pgVer prepared
              row <- H.statement mempty stm
              let (_, queryTotal, _, body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  let
                    defStatus = if iPreferRepresentation apiRequest == Full then status200 else status204
                    status = fromMaybe defStatus gstatus
                    contentRangeHeader = contentRangeH 1 0 $ if shouldCount then Just queryTotal else Nothing
                    (ctHeaders, rBody) = if iPreferRepresentation apiRequest == Full
                                          then ([Just $ toHeader contentType, profileH], toS body)
                                          else ([], mempty)
                    headers = addHeadersIfNotIncluded (catMaybes ctHeaders ++ [contentRangeHeader]) (unwrapGucHeader <$> ghdrs)
                  if contentType == CTSingularJSON
                     && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return $ responseLBS status headers rBody

        (ActionInfo, TargetIdent (QualifiedIdentifier tSchema tTable)) ->
          let mTable = find (\t -> tableName t == tTable && tableSchema t == tSchema) (dbTables dbStructure) in
          case mTable of
            Nothing -> return notFound
            Just table ->
              let allowH = (hAllow, if tableInsertable table then "GET,POST,PATCH,DELETE" else "GET")
                  allOrigins = ("Access-Control-Allow-Origin", "*") :: Header in
              return $ responseLBS status200 [allOrigins, allowH] mempty

        (ActionInvoke invMethod, TargetProc proc@ProcDescription{pdSchema, pdName} _) ->
          let tName = fromMaybe pdName $ procTableName proc in
          case readSqlParts pdSchema tName of
            Left errorResponse -> return errorResponse
            Right (q, cq, bField, returning) -> do
              let
                preferParams = iPreferParameters apiRequest
                pq = requestToCallProcQuery (QualifiedIdentifier pdSchema pdName) (specifiedProcArgs (iColumns apiRequest) proc)
                       (iPayload apiRequest) returnsScalar preferParams returning
                stm = callProcStatement returnsScalar pq q cq shouldCount (contentType == CTSingularJSON)
                        (contentType == CTTextCSV) (contentType `elem` rawContentTypes) (preferParams == Just MultipleObjects)
                        bField pgVer prepared
              row <- H.statement mempty stm
              let (tableTotal, queryTotal, body, gucHeaders, gucStatus) = row
                  gucs =  (,) <$> gucHeaders <*> gucStatus
              case gucs of
                Left err -> return $ errorResponseFor err
                Right (ghdrs, gstatus) -> do
                  let (rangeStatus, contentRange) = rangeStatusHeader topLevelRange queryTotal tableTotal
                      status = fromMaybe rangeStatus gstatus
                      headers = addHeadersIfNotIncluded
                        (catMaybes [Just $ toHeader contentType, Just contentRange, profileH])
                        (unwrapGucHeader <$> ghdrs)
                      rBody = if invMethod == InvHead then mempty else toS body
                  if contentType == CTSingularJSON && queryTotal /= 1
                    then do
                      HT.condemn
                      return . errorResponseFor . singularityError $ queryTotal
                    else
                      return $ responseLBS status headers rBody

        (ActionInspect headersOnly, TargetDefaultSpec tSchema) -> do
          let host = configServerHost conf
              port = toInteger $ configServerPort conf
              proxy = pickProxy $ toS <$> configOpenApiServerProxyUri conf
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
          return $ responseLBS status200 (catMaybes [Just $ toHeader CTOpenAPI, profileH]) (if headersOnly then mempty else toS body)

        _ -> return notFound

      where
        notFound = responseLBS status404 [] ""
        maxRows = configDbMaxRows conf
        prepared = configDbPreparedStatements conf
        exactCount = iPreferCount apiRequest == Just ExactCount
        estimatedCount = iPreferCount apiRequest == Just EstimatedCount
        plannedCount = iPreferCount apiRequest == Just PlannedCount
        shouldCount = exactCount || estimatedCount
        topLevelRange = iTopLevelRange apiRequest
        returnsScalar =
          case iTarget apiRequest of
            TargetProc proc _ -> procReturnsScalar proc
            _                 -> False
        pgVer = pgVersion dbStructure
        profileH = contentProfileH <$> iProfile apiRequest

        readSqlParts s t =
          let
            readReq = readRequest s t maxRows (dbRelations dbStructure) apiRequest
            returnings :: ReadRequest -> Either Response [FieldName]
            returnings rr = Right (returningCols rr [])
          in
          (,,,) <$>
          (readRequestToQuery <$> readReq) <*>
          (readRequestToCountQuery <$> readReq) <*>
          (binaryField contentType rawContentTypes returnsScalar =<< readReq) <*>
          (returnings =<< readReq)

        mutateSqlParts s t =
          let
            readReq = readRequest s t maxRows (dbRelations dbStructure) apiRequest
            mutReq = mutateRequest s t apiRequest (tablePKCols dbStructure s t) =<< readReq
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
    locationFields = renderSimpleQuery True $ splitKeyValue <$> fields
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

contentProfileH :: Schema -> Header
contentProfileH schema =
   ("Content-Profile", toS schema)
