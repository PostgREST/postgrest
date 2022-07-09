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
module PostgREST.App
  ( SignalHandlerInstaller
  , SocketRunner
  , postgrest
  , run
  ) where

import Control.Monad.Except     (liftEither)
import Data.Either.Combinators  (mapLeft)
import Data.List                (union)
import Data.Maybe               (fromJust)
import Data.String              (IsString (..))
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort,
                                 setServerName)
import System.Posix.Types       (FileMode)

import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.HashMap.Strict             as HM
import qualified Data.Set                        as S
import qualified Hasql.DynamicStatements.Snippet as SQL (Snippet)
import qualified Hasql.Pool                      as SQL
import qualified Hasql.Transaction               as SQL
import qualified Hasql.Transaction.Sessions      as SQL
import qualified Network.HTTP.Types.Header       as HTTP
import qualified Network.HTTP.Types.Status       as HTTP
import qualified Network.HTTP.Types.URI          as HTTP
import qualified Network.Wai                     as Wai
import qualified Network.Wai.Handler.Warp        as Warp

import qualified PostgREST.Admin                    as Admin
import qualified PostgREST.AppState                 as AppState
import qualified PostgREST.Auth                     as Auth
import qualified PostgREST.Cors                     as Cors
import qualified PostgREST.DbStructure              as DbStructure
import qualified PostgREST.Error                    as Error
import qualified PostgREST.Logger                   as Logger
import qualified PostgREST.Middleware               as Middleware
import qualified PostgREST.OpenAPI                  as OpenAPI
import qualified PostgREST.Query.QueryBuilder       as QueryBuilder
import qualified PostgREST.Query.Statements         as Statements
import qualified PostgREST.RangeQuery               as RangeQuery
import qualified PostgREST.Request.ApiRequest       as ApiRequest
import qualified PostgREST.Request.DbRequestBuilder as ReqBuilder
import qualified PostgREST.Request.Types            as ApiRequestTypes

import PostgREST.AppState                (AppState)
import PostgREST.Auth                    (AuthResult (..))
import PostgREST.Config                  (AppConfig (..),
                                          LogLevel (..),
                                          OpenAPIMode (..))
import PostgREST.Config.PgVersion        (PgVersion (..))
import PostgREST.DbStructure             (DbStructure (..))
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)
import PostgREST.DbStructure.Proc        (ProcDescription (..),
                                          ProcVolatility (..))
import PostgREST.DbStructure.Table       (Table (..))
import PostgREST.Error                   (Error)
import PostgREST.GucHeader               (GucHeader,
                                          addHeadersIfNotIncluded,
                                          unwrapGucHeader)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Request.ApiRequest      (Action (..),
                                          ApiRequest (..),
                                          InvokeMethod (..),
                                          Mutation (..), Target (..))
import PostgREST.Request.Preferences     (PreferCount (..),
                                          PreferParameters (..),
                                          PreferRepresentation (..),
                                          toAppliedHeader)
import PostgREST.Request.QueryParams     (QueryParams (..))
import PostgREST.Request.ReadQuery       (ReadRequest, fstFieldNames)
import PostgREST.Version                 (prettyVersion)
import PostgREST.Workers                 (connectionWorker, listener)

import qualified PostgREST.DbStructure.Proc as Proc
import qualified PostgREST.MediaType        as MediaType

import Protolude hiding (Handler)

data RequestContext = RequestContext
  { ctxConfig      :: AppConfig
  , ctxDbStructure :: DbStructure
  , ctxApiRequest  :: ApiRequest
  , ctxPgVersion   :: PgVersion
  }

type Handler = ExceptT Error

type DbHandler = Handler SQL.Transaction

type SignalHandlerInstaller = AppState -> IO()

type SocketRunner = Warp.Settings -> Wai.Application -> FileMode -> FilePath -> IO()


run :: SignalHandlerInstaller -> Maybe SocketRunner -> AppState -> IO ()
run installHandlers maybeRunWithSocket appState = do
  conf@AppConfig{..} <- AppState.getConfig appState
  connectionWorker appState -- Loads the initial DbStructure
  installHandlers appState
  -- reload schema cache + config on NOTIFY
  when configDbChannelEnabled $ listener appState

  let app = postgrest configLogLevel appState (connectionWorker appState)
      adminApp = Admin.postgrestAdmin appState conf

  whenJust configAdminServerPort $ \adminPort -> do
    AppState.logWithZTime appState $ "Admin server listening on port " <> show adminPort
    void . forkIO $ Warp.runSettings (serverSettings conf & setPort adminPort) adminApp

  case configServerUnixSocket of
    Just socket ->
      -- run the postgrest application with user defined socket. Only for UNIX systems
      case maybeRunWithSocket of
        Just runWithSocket -> do
          AppState.logWithZTime appState $ "Listening on unix socket " <> show socket
          runWithSocket (serverSettings conf) app configServerUnixSocketMode socket
        Nothing ->
          panic "Cannot run with unix socket on non-unix plattforms."
    Nothing ->
      do
        AppState.logWithZTime appState $ "Listening on port " <> show configServerPort
        Warp.runSettings (serverSettings conf) app
  where
    whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
    whenJust mg f = maybe (pure ()) f mg

serverSettings :: AppConfig -> Warp.Settings
serverSettings AppConfig{..} =
  defaultSettings
    & setHost (fromString $ toS configServerHost)
    & setPort configServerPort
    & setServerName ("postgrest/" <> prettyVersion)

-- | PostgREST application
postgrest :: LogLevel -> AppState.AppState -> IO () -> Wai.Application
postgrest logLevel appState connWorker =
  Cors.middleware .
  Auth.middleware appState .
  Logger.middleware logLevel $
    -- fromJust can be used, because the auth middleware will **always** add
    -- some AuthResult to the vault.
    \req respond -> case fromJust $ Auth.getResult req of
      Left err -> respond $ Error.errorResponseFor err
      Right authResult -> do
        conf <- AppState.getConfig appState
        maybeDbStructure <- AppState.getDbStructure appState
        pgVer <- AppState.getPgVersion appState
        jsonDbS <- AppState.getJsonDbS appState

        let
          eitherResponse :: IO (Either Error Wai.Response)
          eitherResponse =
            runExceptT $ postgrestResponse conf maybeDbStructure jsonDbS pgVer (AppState.getPool appState) authResult req

        response <- either Error.errorResponseFor identity <$> eitherResponse
        -- Launch the connWorker when the connection is down.  The postgrest
        -- function can respond successfully (with a stale schema cache) before
        -- the connWorker is done.
        let isPGAway = Wai.responseStatus response == HTTP.status503
        when isPGAway connWorker
        resp <- addRetryHint isPGAway appState response
        respond resp

addRetryHint :: Bool -> AppState -> Wai.Response -> IO Wai.Response
addRetryHint shouldAdd appState response = do
  delay <- AppState.getRetryNextIn appState
  let h = ("Retry-After", BS.pack $ show delay)
  return $ Wai.mapResponseHeaders (\hs -> if shouldAdd then h:hs else hs) response

postgrestResponse
  :: AppConfig
  -> Maybe DbStructure
  -> ByteString
  -> PgVersion
  -> SQL.Pool
  -> AuthResult
  -> Wai.Request
  -> Handler IO Wai.Response
postgrestResponse conf@AppConfig{..} maybeDbStructure jsonDbS pgVer pool AuthResult{..} req = do
  body <- lift $ Wai.strictRequestBody req

  dbStructure <-
    case maybeDbStructure of
      Just dbStructure ->
        return dbStructure
      Nothing ->
        throwError Error.NoSchemaCacheError

  apiRequest <-
    liftEither . mapLeft Error.ApiRequestError $
      ApiRequest.userApiRequest conf dbStructure req body

  let handleReq apiReq = handleRequest $ RequestContext conf dbStructure apiReq pgVer

  runDbHandler pool (txMode apiRequest) (Just authRole /= configDbAnonRole) configDbPreparedStatements .
    Middleware.optionalRollback conf apiRequest $
      Middleware.runPgLocals conf authClaims authRole handleReq apiRequest jsonDbS pgVer

runDbHandler :: SQL.Pool -> SQL.Mode -> Bool -> Bool -> DbHandler a -> Handler IO a
runDbHandler pool mode authenticated prepared handler = do
  dbResp <-
    let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction in
    lift . SQL.use pool . transaction SQL.ReadCommitted mode $ runExceptT handler

  resp <-
    liftEither . mapLeft Error.PgErr $
      mapLeft (Error.PgError authenticated) dbResp

  liftEither resp

handleRequest :: RequestContext -> DbHandler Wai.Response
handleRequest context@(RequestContext _ _ ApiRequest{..} _) =
  case (iAction, iTarget) of
    (ActionRead headersOnly, TargetIdent identifier) ->
      handleRead headersOnly identifier context
    (ActionMutate MutationCreate, TargetIdent identifier) ->
      handleCreate identifier context
    (ActionMutate MutationUpdate, TargetIdent identifier) ->
      handleUpdate identifier context
    (ActionMutate MutationSingleUpsert, TargetIdent identifier) ->
      handleSingleUpsert identifier context
    (ActionMutate MutationDelete, TargetIdent identifier) ->
      handleDelete identifier context
    (ActionInfo, TargetIdent identifier) ->
      handleInfo identifier context
    (ActionInvoke invMethod, TargetProc proc _) ->
      handleInvoke invMethod proc context
    (ActionInspect headersOnly, TargetDefaultSpec tSchema) ->
      handleOpenApi headersOnly tSchema context
    _ ->
      -- This is unreachable as the ApiRequest.hs rejects it before
      -- TODO Refactor the Action/Target types to remove this line
      throwError $ Error.ApiRequestError ApiRequestTypes.NotFound

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
        (shouldCount iPreferCount)
        iAcceptMediaType
        bField
        configDbPreparedStatements

  total <- readTotal ctxConfig ctxApiRequest tableTotal countQuery
  response <- liftEither $ gucResponse <$> gucStatus <*> gucHeaders

  let
    (status, contentRange) = RangeQuery.rangeStatusHeader iTopLevelRange queryTotal total
    headers =
      [ contentRange
      , ( "Content-Location"
        , "/"
            <> toUtf8 (qiName identifier)
            <> if BS.null (qsCanonical iQueryParams) then mempty else "?" <> qsCanonical iQueryParams
        )
      ]
      ++ contentTypeHeaders context

  failNotSingular iAcceptMediaType queryTotal . response status headers $
    if headersOnly then mempty else LBS.fromStrict body

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
    pkCols = if iPreferRepresentation /= None || isJust iPreferResolution
      then maybe mempty tablePKCols $ HM.lookup identifier $ dbTables ctxDbStructure
      else mempty

  WriteQueryResult{..} <- writeQuery MutationCreate identifier True pkCols context

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
                  <> toUtf8 qiName
                  <> HTTP.renderSimpleQuery True (splitKeyValue <$> resFields)
              )
        , Just . RangeQuery.contentRangeH 1 0 $
            if shouldCount iPreferCount then Just resQueryTotal else Nothing
        , if null pkCols && isNothing (qsOnConflict iQueryParams) then
            Nothing
          else
            toAppliedHeader <$> iPreferResolution
        ]

  failNotSingular iAcceptMediaType resQueryTotal $
    if iPreferRepresentation == Full then
      response HTTP.status201 (headers ++ contentTypeHeaders context) (LBS.fromStrict resBody)
    else
      response HTTP.status201 headers mempty

handleUpdate :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleUpdate identifier context@RequestContext{..} = do
  let
    ApiRequest{..} = ctxApiRequest
    pkCols = maybe mempty tablePKCols $ HM.lookup identifier $ dbTables ctxDbStructure

  WriteQueryResult{..} <- writeQuery MutationUpdate identifier False pkCols context

  let
    response = gucResponse resGucStatus resGucHeaders
    fullRepr = iPreferRepresentation == Full
    updateIsNoOp = S.null iColumns
    status
      | resQueryTotal == 0 && not updateIsNoOp = HTTP.status404
      | fullRepr = HTTP.status200
      | otherwise = HTTP.status204
    contentRangeHeader =
      RangeQuery.contentRangeH 0 (resQueryTotal - 1) $
        if shouldCount iPreferCount then Just resQueryTotal else Nothing

  failChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resQueryTotal =<<
    failNotSingular iAcceptMediaType resQueryTotal (
      if fullRepr then
        response status (contentTypeHeaders context ++ [contentRangeHeader]) (LBS.fromStrict resBody)
      else
        response status [contentRangeHeader] mempty)

handleSingleUpsert :: QualifiedIdentifier -> RequestContext-> DbHandler Wai.Response
handleSingleUpsert identifier context@(RequestContext _ ctxDbStructure ApiRequest{..} _) = do
  let pkCols = maybe mempty tablePKCols $ HM.lookup identifier $ dbTables ctxDbStructure

  WriteQueryResult{..} <- writeQuery MutationSingleUpsert identifier False pkCols context

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
      response HTTP.status200 (contentTypeHeaders context) (LBS.fromStrict resBody)
    else
      response HTTP.status204 [] mempty

handleDelete :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleDelete identifier context@(RequestContext _ _ ApiRequest{..} _) = do
  WriteQueryResult{..} <- writeQuery MutationDelete identifier False mempty context

  let
    response = gucResponse resGucStatus resGucHeaders
    contentRangeHeader =
      RangeQuery.contentRangeH 1 0 $
        if shouldCount iPreferCount then Just resQueryTotal else Nothing

  failChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resQueryTotal =<<
    failNotSingular iAcceptMediaType resQueryTotal (
      if iPreferRepresentation == Full then
        response HTTP.status200
          (contentTypeHeaders context ++ [contentRangeHeader])
          (LBS.fromStrict resBody)
      else
        response HTTP.status204 [contentRangeHeader] mempty)

handleInfo :: Monad m => QualifiedIdentifier -> RequestContext -> Handler m Wai.Response
handleInfo identifier RequestContext{..} =
  case tbl of
    Just table ->
      return $ Wai.responseLBS HTTP.status200 [allOrigins, allowH table] mempty
    Nothing ->
      -- TODO is this right? When no tbl is found on the schema cache we disallow OPTIONS?
      throwError $ Error.ApiRequestError ApiRequestTypes.NotFound
  where
    tbl = HM.lookup identifier (dbTables ctxDbStructure)
    allOrigins = ("Access-Control-Allow-Origin", "*")
    allowH table =
      ( HTTP.hAllow
      , BS.intercalate "," $
          ["OPTIONS,GET,HEAD"]
          ++ ["POST" | tableInsertable table]
          ++ ["PUT" | tableInsertable table && tableUpdatable table && hasPK]
          ++ ["PATCH" | tableUpdatable table]
          ++ ["DELETE" | tableDeletable table]
      )
    hasPK =
      not $ null $ maybe mempty tablePKCols tbl

handleInvoke :: InvokeMethod -> ProcDescription -> RequestContext -> DbHandler Wai.Response
handleInvoke invMethod proc context@RequestContext{..} = do
  let
    ApiRequest{..} = ctxApiRequest

    identifier =
      QualifiedIdentifier
        (pdSchema proc)
        (fromMaybe (pdName proc) $ Proc.procTableName proc)

  req <- readRequest identifier context
  bField <- binaryField context req

  let callReq = ReqBuilder.callRequest proc ctxApiRequest req

  (tableTotal, queryTotal, body, gucHeaders, gucStatus) <-
    lift . SQL.statement mempty $
      Statements.callProcStatement
        (Proc.procReturnsScalar proc)
        (Proc.procReturnsSingle proc)
        (QueryBuilder.requestToCallProcQuery callReq)
        (QueryBuilder.readRequestToQuery req)
        (QueryBuilder.readRequestToCountQuery req)
        (shouldCount iPreferCount)
        iAcceptMediaType
        (iPreferParameters == Just MultipleObjects)
        bField
        (configDbPreparedStatements ctxConfig)

  response <- liftEither $ gucResponse <$> gucStatus <*> gucHeaders

  let
    (status, contentRange) =
      RangeQuery.rangeStatusHeader iTopLevelRange queryTotal tableTotal

  failNotSingular iAcceptMediaType queryTotal $
    if Proc.procReturnsVoid proc then
      response HTTP.status204 [contentRange] mempty
    else
      response status
        (contentTypeHeaders context ++ [contentRange])
        (if invMethod == InvHead then mempty else LBS.fromStrict body)

handleOpenApi :: Bool -> Schema -> RequestContext -> DbHandler Wai.Response
handleOpenApi headersOnly tSchema (RequestContext conf@AppConfig{..} dbStructure apiRequest ctxPgVersion) = do
  body <-
    lift $ case configOpenApiMode of
      OAFollowPriv ->
        OpenAPI.encode conf dbStructure
           <$> SQL.statement [tSchema] (DbStructure.accessibleTables ctxPgVersion configDbPreparedStatements)
           <*> SQL.statement tSchema (DbStructure.accessibleProcs ctxPgVersion configDbPreparedStatements)
           <*> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements)
      OAIgnorePriv ->
        OpenAPI.encode conf dbStructure
              (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ DbStructure.dbTables dbStructure)
              (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ DbStructure.dbProcs dbStructure)
          <$> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements)
      OADisabled ->
        pure mempty

  return $
    Wai.responseLBS HTTP.status200
      (MediaType.toContentType MTOpenAPI : maybeToList (profileHeader apiRequest))
      (if headersOnly then mempty else body)

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

writeQuery :: Mutation -> QualifiedIdentifier -> Bool -> [Text] -> RequestContext -> DbHandler WriteQueryResult
writeQuery mutation identifier@QualifiedIdentifier{..} isInsert pkCols context@RequestContext{..} = do
  readReq <- readRequest identifier context

  mutateReq <-
    liftEither $
      ReqBuilder.mutateRequest mutation qiSchema qiName ctxApiRequest
        pkCols
        readReq

  (_, queryTotal, fields, body, gucHeaders, gucStatus) <-
    lift . SQL.statement mempty $
      Statements.createWriteStatement
        (QueryBuilder.readRequestToQuery readReq)
        (QueryBuilder.mutateRequestToQuery mutateReq)
        isInsert
        (iAcceptMediaType ctxApiRequest)
        (iPreferRepresentation ctxApiRequest)
        pkCols
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
failNotSingular :: MediaType -> Int64 -> Wai.Response -> DbHandler Wai.Response
failNotSingular mediaType queryTotal response =
  if mediaType == MTSingularJSON && queryTotal /= 1 then
    do
      lift SQL.condemn
      throwError $ Error.singularityError queryTotal
  else
    return response

failChangesOffLimits :: Maybe Integer -> Int64 -> Wai.Response -> DbHandler Wai.Response
failChangesOffLimits (Just maxChanges) queryTotal response =
  if queryTotal > fromIntegral maxChanges
  then do
      lift SQL.condemn
      throwError $ Error.OffLimitsChangesError queryTotal maxChanges
  else
    return response
failChangesOffLimits _ _ response = return response

shouldCount :: Maybe PreferCount -> Bool
shouldCount preferCount =
  preferCount == Just ExactCount || preferCount == Just EstimatedCount

returnsScalar :: ApiRequest.Target -> Bool
returnsScalar (TargetProc proc _) = Proc.procReturnsScalar proc
returnsScalar _                   = False

readRequest :: Monad m => QualifiedIdentifier -> RequestContext -> Handler m ReadRequest
readRequest QualifiedIdentifier{..} (RequestContext AppConfig{..} dbStructure apiRequest _) =
  liftEither $
    ReqBuilder.readRequest qiSchema qiName configDbMaxRows
      (dbRelationships dbStructure)
      apiRequest

contentTypeHeaders :: RequestContext -> [HTTP.Header]
contentTypeHeaders RequestContext{..} =
  MediaType.toContentType (iAcceptMediaType ctxApiRequest) : maybeToList (profileHeader ctxApiRequest)

-- | If raw(binary) output is requested, check that MediaType is one of the
-- admitted rawMediaTypes and that`?select=...` contains only one field other
-- than `*`
binaryField :: Monad m => RequestContext -> ReadRequest -> Handler m (Maybe FieldName)
binaryField RequestContext{..} readReq
  | returnsScalar (iTarget ctxApiRequest) && isRawMediaType =
      return $ Just "pgrst_scalar"
  | isRawMediaType =
      let
        fldNames = fstFieldNames readReq
        fieldName = headMay fldNames
      in
      if length fldNames == 1 && fieldName /= Just "*" then
        return fieldName
      else
        throwError $ Error.BinaryFieldError (iAcceptMediaType ctxApiRequest)
  | otherwise =
      return Nothing
  where
    isRawMediaType = iAcceptMediaType ctxApiRequest `elem` configRawMediaTypes ctxConfig `union` [MTOctetStream, MTTextPlain, MTTextXML]

profileHeader :: ApiRequest -> Maybe HTTP.Header
profileHeader ApiRequest{..} =
  (,) "Content-Profile" <$> (toUtf8 <$> iProfile)

splitKeyValue :: ByteString -> (ByteString, ByteString)
splitKeyValue kv =
  (k, BS.tail v)
  where
    (k, v) = BS.break (== '=') kv
