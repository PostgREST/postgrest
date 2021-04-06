{-|
Module      : PostgREST.App
Description : PostgREST main application

This module is in charge of mapping HTTP requests to PostgreSQL queries.
Some of its functionality includes:

- Mapping HTTP request methods to proper SQL statements. For example, a GET request is translated to executing a SELECT query in a read-only TRANSACTION.
- Producing HTTP Headers according to RFCs.
- Content Negotiation
-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.App (postgrest) where

import Control.Monad.Except    (liftEither)
import Data.Either.Combinators (mapLeft)
import Data.IORef              (IORef, readIORef)
import Data.List               (union)
import Data.Time.Clock         (UTCTime)

import qualified Data.ByteString.Char8           as BS8
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Set                        as Set
import qualified Hasql.DynamicStatements.Snippet as SQL
import qualified Hasql.Pool                      as SQL
import qualified Hasql.Transaction               as SQL
import qualified Hasql.Transaction.Sessions      as SQL
import qualified Network.HTTP.Types.Header       as HTTP
import qualified Network.HTTP.Types.Status       as HTTP
import qualified Network.HTTP.Types.URI          as HTTP
import qualified Network.Wai                     as Wai

import qualified PostgREST.ApiRequest       as ApiRequest
import qualified PostgREST.Auth             as Auth
import qualified PostgREST.DbRequestBuilder as ReqBuilder
import qualified PostgREST.DbStructure      as DbStructure
import qualified PostgREST.Error            as Error
import qualified PostgREST.Middleware       as Middleware
import qualified PostgREST.OpenAPI          as OpenAPI
import qualified PostgREST.QueryBuilder     as QueryBuilder
import qualified PostgREST.RangeQuery       as RangeQuery
import qualified PostgREST.Statements       as Statements

import PostgREST.ApiRequest       (Action (..), ApiRequest (..),
                                   InvokeMethod (..), Target (..))
import PostgREST.Config           (AppConfig (..), LogLevel (..))
import PostgREST.ContentTypes     (ContentType (..),
                                   decodeContentType, toHeader,
                                   toMime)
import PostgREST.DbStructureTypes (DbStructure (..), FieldName,
                                   ProcDescription (..),
                                   ProcVolatility (..),
                                   QualifiedIdentifier (..), Schema,
                                   Table (..), procReturnsScalar,
                                   procReturnsSingle, procTableName,
                                   specifiedProcArgs, tablePKCols)
import PostgREST.Error            (Error)
import PostgREST.Headers          (GucHeader, addHeadersIfNotIncluded,
                                   unwrapGucHeader)
import PostgREST.Preferences      (PreferCount (..),
                                   PreferParameters (..),
                                   PreferRepresentation (..))
import PostgREST.Queries          (ReadRequest, fstFieldNames)

import Protolude      hiding (Handler, toS)
import Protolude.Conv (toS)


data RequestContext = RequestContext
  { ctxConfig      :: AppConfig
  , ctxDbStructure :: DbStructure
  , ctxApiRequest  :: ApiRequest
  , ctxContentType :: ContentType
  }

type Handler = ExceptT Error

type DbHandler = Handler SQL.Transaction


-- | PostgREST application
postgrest
  :: LogLevel
  -> IORef AppConfig
  -> IORef (Maybe DbStructure)
  -> SQL.Pool
  -> IO UTCTime
  -> IO () -- ^ Lauch connection worker in a separate thread
  -> Wai.Application
postgrest logLev refConf refDbStructure pool getTime connWorker =
  Middleware.pgrstMiddleware logLev $
    \req respond -> do
      time <- getTime
      conf <- readIORef refConf
      maybeDbStructure <- readIORef refDbStructure

      let
        eitherResponse :: IO (Either Error Wai.Response)
        eitherResponse =
          runExceptT $ postgrestResponse conf maybeDbStructure pool time req

      response <- either Error.errorResponseFor identity <$> eitherResponse

      -- Launch the connWorker when the connection is down.  The postgrest
      -- function can respond successfully (with a stale schema cache) before
      -- the connWorker is done.
      when (Wai.responseStatus response == HTTP.status503) connWorker

      respond response

postgrestResponse
  :: AppConfig
  -> Maybe DbStructure
  -> SQL.Pool
  -> UTCTime
  -> Wai.Request
  -> Handler IO Wai.Response
postgrestResponse conf@AppConfig{..} maybeDbStructure pool time req = do
  body <- lift $ Wai.strictRequestBody req

  dbStructure <-
    case maybeDbStructure of
      Just dbStructure ->
        return dbStructure
      Nothing ->
        throwError Error.ConnectionLostError

  apiRequest@ApiRequest{..} <-
    liftEither . mapLeft Error.ApiRequestError $
      ApiRequest.userApiRequest configDbSchemas configDbRootSpec dbStructure req body

  -- The JWT must be checked before touching the db
  jwtClaims <- Auth.jwtClaims conf (toS iJWT) time

  contentType <-
    case ApiRequest.mutuallyAgreeable (requestContentTypes conf apiRequest) iAccepts of
      Just ct ->
        return ct
      Nothing ->
        throwError . Error.ContentTypeError $ map toMime iAccepts

  let
    handleReq apiReq =
      handleRequest $ RequestContext conf dbStructure apiReq contentType

  runDbHandler pool (txMode apiRequest) jwtClaims .
    Middleware.optionalRollback conf apiRequest $
      Middleware.runPgLocals conf jwtClaims handleReq apiRequest

runDbHandler :: SQL.Pool -> SQL.Mode -> Auth.JWTClaims -> DbHandler a -> Handler IO a
runDbHandler pool mode jwtClaims handler = do
  dbResp <-
    lift . SQL.use pool . SQL.transaction SQL.ReadCommitted mode $ runExceptT handler

  resp <-
    liftEither . mapLeft Error.PgErr $
      mapLeft (Error.PgError $ Auth.containsRole jwtClaims) dbResp

  liftEither resp

handleRequest :: RequestContext -> DbHandler Wai.Response
handleRequest context@(RequestContext _ _ ApiRequest{..} _) =
  case (iAction, iTarget) of
    (ActionRead headersOnly, TargetIdent identifier) ->
      handleRead headersOnly identifier context
    (ActionCreate, TargetIdent identifier) ->
      handleCreate identifier context
    (ActionUpdate, TargetIdent identifier) ->
      handleUpdate identifier context
    (ActionSingleUpsert, TargetIdent identifier) ->
      handleSingleUpsert identifier context
    (ActionDelete, TargetIdent identifier) ->
      handleDelete identifier context
    (ActionInfo, TargetIdent identifier) ->
      handleInfo identifier context
    (ActionInvoke invMethod, TargetProc proc _) ->
      handleInvoke invMethod proc context
    (ActionInspect headersOnly, TargetDefaultSpec tSchema) ->
      handleOpenApi headersOnly tSchema context
    _ ->
      throwError Error.NotFound

handleRead :: Bool -> QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleRead headersOnly identifier context@RequestContext{..} = do
  req <- readRequest identifier context
  bField <- binaryField context req

  let
    ApiRequest{..} = ctxApiRequest
    AppConfig{..} = ctxConfig
    countQuery = QueryBuilder.readRequestToCountQuery req

  (tableTotal, queryTotal, _ , body, gucHeaders, gucStatus) <-
    lift . SQL.statement mempty $
      Statements.createReadStatement
        (QueryBuilder.readRequestToQuery req)
        (if iPreferCount == Just EstimatedCount then
           -- LIMIT maxRows + 1 so we can determine below that maxRows was surpassed
           QueryBuilder.limitedQuery countQuery ((+ 1) <$> configDbMaxRows)
         else
           countQuery
        )
        (ctxContentType == CTSingularJSON)
        (shouldCount iPreferCount)
        (ctxContentType == CTTextCSV)
        bField
        (pgVersion ctxDbStructure)
        configDbPreparedStatements

  total <- readTotal ctxConfig ctxApiRequest tableTotal countQuery
  response <- liftEither $ gucResponse <$> gucStatus <*> gucHeaders

  let
    (status, contentRange) = RangeQuery.rangeStatusHeader iTopLevelRange queryTotal total
    headers =
      [ contentRange
      , ( "Content-Location"
        , "/"
            <> toS (qiName identifier)
            <> if BS8.null iCanonicalQS then mempty else "?" <> toS iCanonicalQS
        )
      ]
      ++ contentTypeHeaders context

  failNotSingular ctxContentType queryTotal . response status headers $
    if headersOnly then mempty else toS body

readTotal :: AppConfig -> ApiRequest -> Maybe Int64 -> SQL.Snippet -> DbHandler (Maybe Int64)
readTotal AppConfig{..} ApiRequest{..} tableTotal countQuery =
  case iPreferCount of
    Just PlannedCount ->
      explain
    Just EstimatedCount ->
      if tableTotal > (fromIntegral <$> configDbMaxRows) then
        max tableTotal <$> explain
      else
        return tableTotal
    _ ->
      return tableTotal
  where
    explain =
      lift . SQL.statement mempty . Statements.createExplainStatement countQuery $
        configDbPreparedStatements

handleCreate :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleCreate identifier@QualifiedIdentifier{..} context@RequestContext{..} = do
  let
    ApiRequest{..} = ctxApiRequest
    pkCols = tablePKCols ctxDbStructure qiSchema qiName

  WriteQueryResult{..} <- writeQuery identifier True pkCols context

  let
    response = gucResponse resGucStatus resGucHeaders
    headers =
      catMaybes
        [ if null resFields then
            Nothing
          else
            Just
              ( HTTP.hLocation
              , "/"
                  <> toS qiName
                  <> HTTP.renderSimpleQuery True (splitKeyValue <$> resFields)
              )
        , Just . RangeQuery.contentRangeH 1 0 $
            if shouldCount iPreferCount then Just resQueryTotal else Nothing
        , if null pkCols && isNothing iOnConflict then
            Nothing
          else
            (\x -> ("Preference-Applied", BS8.pack $ show x)) <$> iPreferResolution
        ]

  failNotSingular ctxContentType resQueryTotal $
    if iPreferRepresentation == Full then
      response HTTP.status201 (headers ++ contentTypeHeaders context) (toS resBody)
    else
      response HTTP.status201 headers mempty

handleUpdate :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleUpdate identifier context@(RequestContext _ _ ApiRequest{..} contentType) = do
  WriteQueryResult{..} <- writeQuery identifier False mempty context

  let
    response = gucResponse resGucStatus resGucHeaders
    fullRepr = iPreferRepresentation == Full
    updateIsNoOp = Set.null iColumns
    status
      | resQueryTotal == 0 && not updateIsNoOp = HTTP.status404
      | fullRepr = HTTP.status200
      | otherwise = HTTP.status204
    contentRangeHeader =
      RangeQuery.contentRangeH 0 (resQueryTotal - 1) $
        if shouldCount iPreferCount then Just resQueryTotal else Nothing

  failNotSingular contentType resQueryTotal $
    if fullRepr then
      response status (contentTypeHeaders context ++ [contentRangeHeader]) (toS resBody)
    else
      response status [contentRangeHeader] mempty

handleSingleUpsert :: QualifiedIdentifier -> RequestContext-> DbHandler Wai.Response
handleSingleUpsert identifier context@(RequestContext _ _ ApiRequest{..} _) = do
  when (iTopLevelRange /= RangeQuery.allRange) $
    throwError Error.PutRangeNotAllowedError

  WriteQueryResult{..} <- writeQuery identifier False mempty context

  let response = gucResponse resGucStatus resGucHeaders

  -- Makes sure the querystring pk matches the payload pk
  -- e.g. PUT /items?id=eq.1 { "id" : 1, .. } is accepted,
  -- PUT /items?id=eq.14 { "id" : 2, .. } is rejected.
  -- If this condition is not satisfied then nothing is inserted,
  -- check the WHERE for INSERT in QueryBuilder.hs to see how it's done
  when (resQueryTotal /= 1) $ do
    lift SQL.condemn
    throwError Error.PutMatchingPkError

  return $
    if iPreferRepresentation == Full then
      response HTTP.status200 (contentTypeHeaders context) (toS resBody)
    else
      response HTTP.status204 (contentTypeHeaders context) mempty

handleDelete :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleDelete identifier context@(RequestContext _ _ ApiRequest{..} contentType) = do
  WriteQueryResult{..} <- writeQuery identifier False mempty context

  let
    response = gucResponse resGucStatus resGucHeaders
    contentRangeHeader =
      RangeQuery.contentRangeH 1 0 $
        if shouldCount iPreferCount then Just resQueryTotal else Nothing

  failNotSingular contentType resQueryTotal $
    if iPreferRepresentation == Full then
      response HTTP.status200
        (contentTypeHeaders context ++ [contentRangeHeader])
        (toS resBody)
    else
      response HTTP.status204 [contentRangeHeader] mempty

handleInfo :: Monad m => QualifiedIdentifier -> RequestContext -> Handler m Wai.Response
handleInfo identifier RequestContext{..} =
  case find tableMatches $ dbTables ctxDbStructure of
    Just table ->
      return $ Wai.responseLBS HTTP.status200 [allOrigins, allowH table] mempty
    Nothing ->
      throwError Error.NotFound
  where
    allOrigins = ("Access-Control-Allow-Origin", "*")
    allowH table =
      ( HTTP.hAllow
      , if tableInsertable table then "GET,POST,PATCH,DELETE" else "GET"
      )
    tableMatches table =
      tableName table == qiName identifier
      && tableSchema table == qiSchema identifier

handleInvoke :: InvokeMethod -> ProcDescription -> RequestContext -> DbHandler Wai.Response
handleInvoke invMethod proc context@RequestContext{..} = do
  let
    ApiRequest{..} = ctxApiRequest

    identifier =
      QualifiedIdentifier
        (pdSchema proc)
        (fromMaybe (pdName proc) $ procTableName proc)

    returnsSingle (ApiRequest.TargetProc target _) = procReturnsSingle target
    returnsSingle _                                = False

  req <- readRequest identifier context
  bField <- binaryField context req

  (tableTotal, queryTotal, body, gucHeaders, gucStatus) <-
    lift . SQL.statement mempty $
      Statements.callProcStatement
        (returnsScalar iTarget)
        (returnsSingle iTarget)
        (QueryBuilder.requestToCallProcQuery
          (QualifiedIdentifier (pdSchema proc) (pdName proc))
          (specifiedProcArgs iColumns proc)
          iPayload
          (returnsScalar iTarget)
          iPreferParameters
          (ReqBuilder.returningCols req [])
        )
        (QueryBuilder.readRequestToQuery req)
        (QueryBuilder.readRequestToCountQuery req)
        (shouldCount iPreferCount)
        (ctxContentType == CTSingularJSON)
        (ctxContentType == CTTextCSV)
        (iPreferParameters == Just MultipleObjects)
        bField
        (pgVersion ctxDbStructure)
        (configDbPreparedStatements ctxConfig)

  response <- liftEither $ gucResponse <$> gucStatus <*> gucHeaders

  let
    (status, contentRange) =
      RangeQuery.rangeStatusHeader iTopLevelRange queryTotal tableTotal

  failNotSingular ctxContentType queryTotal $
    response status
      (contentTypeHeaders context ++ [contentRange])
      (if invMethod == InvHead then mempty else toS body)

handleOpenApi :: Bool -> Schema -> RequestContext -> DbHandler Wai.Response
handleOpenApi headersOnly tSchema (RequestContext conf@AppConfig{..} dbStructure apiRequest _) = do
  body <-
    lift $
      OpenAPI.encode conf dbStructure
        <$> SQL.statement tSchema (DbStructure.accessibleTables configDbPreparedStatements)
        <*> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements)
        <*> SQL.statement tSchema (DbStructure.accessibleProcs configDbPreparedStatements)

  return $
    Wai.responseLBS HTTP.status200
      (toHeader CTOpenAPI : maybeToList (profileHeader apiRequest))
      (if headersOnly then mempty else toS body)

txMode :: ApiRequest -> SQL.Mode
txMode ApiRequest{..} =
  case (iAction, iTarget) of
    (ActionRead _, _) ->
      SQL.Read
    (ActionInfo, _) ->
      SQL.Read
    (ActionInspect _, _) ->
      SQL.Read
    (ActionInvoke InvGet, _) ->
      SQL.Read
    (ActionInvoke InvHead, _) ->
      SQL.Read
    (ActionInvoke InvPost, TargetProc ProcDescription{pdVolatility=Stable} _) ->
      SQL.Read
    (ActionInvoke InvPost, TargetProc ProcDescription{pdVolatility=Immutable} _) ->
      SQL.Read
    _ ->
      SQL.Write

-- | Result from executing a write query on the database
data WriteQueryResult = WriteQueryResult
  { resQueryTotal :: Int64
  , resFields     :: [ByteString]
  , resBody       :: ByteString
  , resGucStatus  :: Maybe HTTP.Status
  , resGucHeaders :: [GucHeader]
  }

writeQuery :: QualifiedIdentifier -> Bool -> [Text] -> RequestContext -> DbHandler WriteQueryResult
writeQuery identifier@QualifiedIdentifier{..} isInsert pkCols context@RequestContext{..} = do
  readReq <- readRequest identifier context

  mutateReq <-
    liftEither $
      ReqBuilder.mutateRequest qiSchema qiName ctxApiRequest
        (tablePKCols ctxDbStructure qiSchema qiName)
        readReq

  (_, queryTotal, fields, body, gucHeaders, gucStatus) <-
    lift . SQL.statement mempty $
      Statements.createWriteStatement
        (QueryBuilder.readRequestToQuery readReq)
        (QueryBuilder.mutateRequestToQuery mutateReq)
        (ctxContentType == CTSingularJSON)
        isInsert
        (ctxContentType == CTTextCSV)
        (iPreferRepresentation ctxApiRequest)
        pkCols
        (pgVersion ctxDbStructure)
        (configDbPreparedStatements ctxConfig)

  liftEither $ WriteQueryResult queryTotal fields body <$> gucStatus <*> gucHeaders

-- | Response with headers and status overridden from GUCs.
gucResponse
  :: Maybe HTTP.Status
  -> [GucHeader]
  -> HTTP.Status
  -> [HTTP.Header]
  -> LBS.ByteString
  -> Wai.Response
gucResponse gucStatus gucHeaders status headers =
  Wai.responseLBS (fromMaybe status gucStatus) $
    addHeadersIfNotIncluded headers (map unwrapGucHeader gucHeaders)

-- |
-- Fail a response if a single JSON object was requested and not exactly one
-- was found.
failNotSingular :: ContentType -> Int64 -> Wai.Response -> DbHandler Wai.Response
failNotSingular contentType queryTotal response =
  if contentType == CTSingularJSON && queryTotal /= 1 then
    do
      lift SQL.condemn
      throwError $ Error.singularityError queryTotal
  else
    return response

shouldCount :: Maybe PreferCount -> Bool
shouldCount preferCount =
  preferCount == Just ExactCount || preferCount == Just EstimatedCount

returnsScalar :: ApiRequest.Target -> Bool
returnsScalar (TargetProc proc _) = procReturnsScalar proc
returnsScalar _                   = False

readRequest :: Monad m => QualifiedIdentifier -> RequestContext -> Handler m ReadRequest
readRequest QualifiedIdentifier{..} (RequestContext AppConfig{..} dbStructure apiRequest _) =
  liftEither $
    ReqBuilder.readRequest qiSchema qiName configDbMaxRows
      (dbRelations dbStructure)
      apiRequest

contentTypeHeaders :: RequestContext -> [HTTP.Header]
contentTypeHeaders RequestContext{..} =
  toHeader ctxContentType : maybeToList (profileHeader ctxApiRequest)

requestContentTypes :: AppConfig -> ApiRequest -> [ContentType]
requestContentTypes conf ApiRequest{..} =
  case iAction of
    ActionRead _    -> defaultContentTypes ++ rawContentTypes conf
    ActionInvoke _  -> invokeContentTypes
    ActionInspect _ -> [CTOpenAPI, CTApplicationJSON]
    ActionInfo      -> [CTTextCSV]
    _               -> defaultContentTypes
  where
    invokeContentTypes =
      defaultContentTypes
        ++ rawContentTypes conf
        ++ [CTOpenAPI | ApiRequest.tpIsRootSpec iTarget]
    defaultContentTypes =
      [CTApplicationJSON, CTSingularJSON, CTTextCSV]

-- |
-- If raw(binary) output is requested, check that ContentType is one of the admitted
-- rawContentTypes and that`?select=...` contains only one field other than `*`
binaryField :: Monad m => RequestContext -> ReadRequest -> Handler m (Maybe FieldName)
binaryField RequestContext{..} readReq
  | returnsScalar (iTarget ctxApiRequest) && ctxContentType `elem` rawContentTypes ctxConfig =
      return $ Just "pgrst_scalar"
  | ctxContentType `elem` rawContentTypes ctxConfig =
      let
        fldNames = fstFieldNames readReq
        fieldName = headMay fldNames
      in
      if length fldNames == 1 && fieldName /= Just "*" then
        return fieldName
      else
        throwError $ Error.BinaryFieldError ctxContentType
  | otherwise =
      return Nothing

rawContentTypes :: AppConfig -> [ContentType]
rawContentTypes AppConfig{..} =
  (decodeContentType <$> configRawMediaTypes) `union` [CTOctetStream, CTTextPlain]

profileHeader :: ApiRequest -> Maybe HTTP.Header
profileHeader ApiRequest{..} =
  (,) "Content-Profile" <$> (toS <$> iProfile)

splitKeyValue :: ByteString -> (ByteString, ByteString)
splitKeyValue kv =
  (k, BS8.tail v)
  where
    (k, v) = BS8.break (== '=') kv
