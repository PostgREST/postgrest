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
import Data.String              (IsString (..))
import Data.Time.Clock          (UTCTime)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort,
                                 setServerName)
import System.Posix.Types       (FileMode)

import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.HashMap.Strict             as M
import qualified Data.Set                        as S
import qualified Hasql.DynamicStatements.Snippet as SQL
import qualified Hasql.Pool                      as SQL
import qualified Hasql.Transaction               as SQL
import qualified Hasql.Transaction.Sessions      as SQL
import qualified Network.HTTP.Types.Header       as HTTP
import qualified Network.HTTP.Types.Status       as HTTP
import qualified Network.HTTP.Types.URI          as HTTP
import qualified Network.Wai                     as Wai
import qualified Network.Wai.Handler.Warp        as Warp

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

import PostgREST.AppState                (AppState)
import PostgREST.Config                  (AppConfig (..),
                                          LogLevel (..),
                                          OpenAPIMode (..))
import PostgREST.Config.PgVersion        (PgVersion (..))
import PostgREST.ContentType             (ContentType (..))
import PostgREST.DbStructure             (DbStructure (..),
                                          tablePKCols)
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
import PostgREST.Request.ApiRequest      (Action (..),
                                          ApiRequest (..),
                                          InvokeMethod (..),
                                          Target (..))
import PostgREST.Request.Preferences     (PreferCount (..),
                                          PreferParameters (..),
                                          PreferRepresentation (..),
                                          toAppliedHeader)
import PostgREST.Request.Types           (ReadRequest, fstFieldNames)
import PostgREST.Version                 (prettyVersion)
import PostgREST.Workers                 (connectionWorker, listener)

import qualified PostgREST.ContentType      as ContentType
import qualified PostgREST.DbStructure.Proc as Proc

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

  case configServerUnixSocket of
    Just socket ->
      -- run the postgrest application with user defined socket. Only for UNIX systems
      case maybeRunWithSocket of
        Just runWithSocket -> do
          AppState.logWithZTime appState $ "Listening on unix socket " <> show socket
          runWithSocket (serverSettings conf) app configServerUnixSocketMode socket
        Nothing ->
          panic "Cannot run with socket on non-unix plattforms."
    Nothing ->
      do
        AppState.logWithZTime appState $ "Listening on port " <> show configServerPort
        Warp.runSettings (serverSettings conf) app

serverSettings :: AppConfig -> Warp.Settings
serverSettings AppConfig{..} =
  defaultSettings
    & setHost (fromString $ toS configServerHost)
    & setPort configServerPort
    & setServerName ("postgrest/" <> prettyVersion)

-- | PostgREST application
postgrest :: LogLevel -> AppState.AppState -> IO () -> Wai.Application
postgrest logLevel appState connWorker =
  Logger.middleware logLevel .
  Cors.middleware $
    \req respond -> do
      time <- AppState.getTime appState
      conf <- AppState.getConfig appState
      maybeDbStructure <- AppState.getDbStructure appState
      pgVer <- AppState.getPgVersion appState
      jsonDbS <- AppState.getJsonDbS appState

      let
        eitherResponse :: IO (Either Error Wai.Response)
        eitherResponse =
          runExceptT $ postgrestResponse conf maybeDbStructure jsonDbS pgVer (AppState.getPool appState) time req

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
  -> UTCTime
  -> Wai.Request
  -> Handler IO Wai.Response
postgrestResponse conf maybeDbStructure jsonDbS pgVer pool time req = do
  body <- lift $ Wai.strictRequestBody req

  dbStructure <-
    case maybeDbStructure of
      Just dbStructure ->
        return dbStructure
      Nothing ->
        throwError Error.ConnectionLostError

  apiRequest@ApiRequest{..} <-
    liftEither . mapLeft Error.ApiRequestError $
      ApiRequest.userApiRequest conf dbStructure req body

  -- The JWT must be checked before touching the db
  jwtClaims <- Auth.jwtClaims conf (toUtf8Lazy iJWT) time

  let
    handleReq apiReq =
      handleRequest $ RequestContext conf dbStructure apiReq pgVer

  runDbHandler pool (txMode apiRequest) jwtClaims (configDbPreparedStatements conf) .
    Middleware.optionalRollback conf apiRequest $
      Middleware.runPgLocals conf jwtClaims handleReq apiRequest jsonDbS pgVer

runDbHandler :: SQL.Pool -> SQL.Mode -> Auth.JWTClaims -> Bool -> DbHandler a -> Handler IO a
runDbHandler pool mode jwtClaims prepared handler = do
  dbResp <-
    let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction in
    lift . SQL.use pool . transaction SQL.ReadCommitted mode $ runExceptT handler

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
        (iAcceptContentType == CTSingularJSON)
        (shouldCount iPreferCount)
        (iAcceptContentType == CTTextCSV)
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
            <> if BS.null iCanonicalQS then mempty else "?" <> iCanonicalQS
        )
      ]
      ++ contentTypeHeaders context

  failNotSingular iAcceptContentType queryTotal . response status headers $
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
                  <> toUtf8 qiName
                  <> HTTP.renderSimpleQuery True (splitKeyValue <$> resFields)
              )
        , Just . RangeQuery.contentRangeH 1 0 $
            if shouldCount iPreferCount then Just resQueryTotal else Nothing
        , if null pkCols && isNothing iOnConflict then
            Nothing
          else
            toAppliedHeader <$> iPreferResolution
        ]

  failNotSingular iAcceptContentType resQueryTotal $
    if iPreferRepresentation == Full then
      response HTTP.status201 (headers ++ contentTypeHeaders context) (LBS.fromStrict resBody)
    else
      response HTTP.status201 headers mempty

handleUpdate :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleUpdate identifier context@(RequestContext _ _ ApiRequest{..} _) = do
  WriteQueryResult{..} <- writeQuery identifier False mempty context

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

  failNotSingular iAcceptContentType resQueryTotal $
    if fullRepr then
      response status (contentTypeHeaders context ++ [contentRangeHeader]) (LBS.fromStrict resBody)
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
      response HTTP.status200 (contentTypeHeaders context) (LBS.fromStrict resBody)
    else
      response HTTP.status204 (contentTypeHeaders context) mempty

handleDelete :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleDelete identifier context@(RequestContext _ _ ApiRequest{..} _) = do
  WriteQueryResult{..} <- writeQuery identifier False mempty context

  let
    response = gucResponse resGucStatus resGucHeaders
    contentRangeHeader =
      RangeQuery.contentRangeH 1 0 $
        if shouldCount iPreferCount then Just resQueryTotal else Nothing

  failNotSingular iAcceptContentType resQueryTotal $
    if iPreferRepresentation == Full then
      response HTTP.status200
        (contentTypeHeaders context ++ [contentRangeHeader])
        (LBS.fromStrict resBody)
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
      , BS.intercalate "," $
          ["OPTIONS,GET,HEAD"]
          ++ ["POST" | tableInsertable table]
          ++ ["PUT" | tableInsertable table && tableUpdatable table && hasPK]
          ++ ["PATCH" | tableUpdatable table]
          ++ ["DELETE" | tableDeletable table]
      )
    tableMatches table =
      tableName table == qiName identifier
      && tableSchema table == qiSchema identifier
    hasPK =
      not $ null $ tablePKCols ctxDbStructure (qiSchema identifier) (qiName identifier)

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
        (iAcceptContentType == CTSingularJSON)
        (iAcceptContentType == CTTextCSV)
        (iPreferParameters == Just MultipleObjects)
        bField
        (configDbPreparedStatements ctxConfig)

  response <- liftEither $ gucResponse <$> gucStatus <*> gucHeaders

  let
    (status, contentRange) =
      RangeQuery.rangeStatusHeader iTopLevelRange queryTotal tableTotal

  failNotSingular iAcceptContentType queryTotal $
    response status
      (contentTypeHeaders context ++ [contentRange])
      (if invMethod == InvHead then mempty else LBS.fromStrict body)

handleOpenApi :: Bool -> Schema -> RequestContext -> DbHandler Wai.Response
handleOpenApi headersOnly tSchema (RequestContext conf@AppConfig{..} dbStructure apiRequest ctxPgVersion) = do
  body <-
    lift $ case configOpenApiMode of
      OAFollowPriv ->
        OpenAPI.encode conf dbStructure
           <$> SQL.statement tSchema (DbStructure.accessibleTables ctxPgVersion configDbPreparedStatements)
           <*> SQL.statement tSchema (DbStructure.accessibleProcs configDbPreparedStatements)
           <*> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements)
      OAIgnorePriv ->
        OpenAPI.encode conf dbStructure
              (filter (\x -> tableSchema x == tSchema) $ DbStructure.dbTables dbStructure)
              (M.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ DbStructure.dbProcs dbStructure)
          <$> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements)
      OADisabled ->
        pure mempty

  return $
    Wai.responseLBS HTTP.status200
      (ContentType.toHeader CTOpenAPI : maybeToList (profileHeader apiRequest))
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
        (iAcceptContentType ctxApiRequest == CTSingularJSON)
        isInsert
        (iAcceptContentType ctxApiRequest == CTTextCSV)
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
  ContentType.toHeader (iAcceptContentType ctxApiRequest) : maybeToList (profileHeader ctxApiRequest)

-- | If raw(binary) output is requested, check that ContentType is one of the
-- admitted rawContentTypes and that`?select=...` contains only one field other
-- than `*`
binaryField :: Monad m => RequestContext -> ReadRequest -> Handler m (Maybe FieldName)
binaryField RequestContext{..} readReq
  | returnsScalar (iTarget ctxApiRequest) && iAcceptContentType ctxApiRequest `elem` rawContentTypes ctxConfig =
      return $ Just "pgrst_scalar"
  | iAcceptContentType ctxApiRequest `elem` rawContentTypes ctxConfig =
      let
        fldNames = fstFieldNames readReq
        fieldName = headMay fldNames
      in
      if length fldNames == 1 && fieldName /= Just "*" then
        return fieldName
      else
        throwError $ Error.BinaryFieldError (iAcceptContentType ctxApiRequest)
  | otherwise =
      return Nothing

rawContentTypes :: AppConfig -> [ContentType]
rawContentTypes AppConfig{..} =
  (ContentType.decodeContentType <$> configRawMediaTypes) `union` [CTOctetStream, CTTextPlain]

profileHeader :: ApiRequest -> Maybe HTTP.Header
profileHeader ApiRequest{..} =
  (,) "Content-Profile" <$> (toUtf8 <$> iProfile)

splitKeyValue :: ByteString -> (ByteString, ByteString)
splitKeyValue kv =
  (k, BS.tail v)
  where
    (k, v) = BS.break (== '=') kv
