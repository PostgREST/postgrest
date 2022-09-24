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

import qualified Data.HashMap.Strict             as HM
import qualified Hasql.DynamicStatements.Snippet as SQL (Snippet)
import qualified Hasql.Transaction               as SQL
import qualified Hasql.Transaction.Sessions      as SQL
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
import qualified PostgREST.Query.QueryBuilder       as QueryBuilder
import qualified PostgREST.Query.Statements         as Statements
import qualified PostgREST.RangeQuery               as RangeQuery
import qualified PostgREST.Request.ApiRequest       as ApiRequest
import qualified PostgREST.Request.DbRequestBuilder as ReqBuilder
import qualified PostgREST.Request.Types            as ApiRequestTypes
import qualified PostgREST.Response                 as Response

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
import PostgREST.MediaType               (MTPlanAttrs (..),
                                          MediaType (..))
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.Request.ApiRequest      (Action (..),
                                          ApiRequest (..),
                                          InvokeMethod (..),
                                          Mutation (..), Target (..))
import PostgREST.Request.Preferences     (PreferCount (..),
                                          PreferParameters (..),
                                          PreferRepresentation (..),
                                          shouldCount)
import PostgREST.Request.ReadQuery       (ReadRequest, fstFieldNames)
import PostgREST.Version                 (prettyVersion)
import PostgREST.Workers                 (connectionWorker, listener)

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
          panic "Cannot run with unix socket on non-unix platforms."
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
            runExceptT $ postgrestResponse appState conf maybeDbStructure jsonDbS pgVer authResult req

        response <- either Error.errorResponseFor identity <$> eitherResponse
        -- Launch the connWorker when the connection is down.  The postgrest
        -- function can respond successfully (with a stale schema cache) before
        -- the connWorker is done.
        when (Response.isServiceUnavailable response) connWorker
        resp <- do
          delay <- AppState.getRetryNextIn appState
          return $ Response.addRetryHint delay response
        respond resp

postgrestResponse
  :: AppState.AppState
  -> AppConfig
  -> Maybe DbStructure
  -> ByteString
  -> PgVersion
  -> AuthResult
  -> Wai.Request
  -> Handler IO Wai.Response
postgrestResponse appState conf@AppConfig{..} maybeDbStructure jsonDbS pgVer AuthResult{..} req = do
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

  let ctx apiReq = RequestContext conf dbStructure apiReq pgVer

  if iAction apiRequest == ActionInfo then
    pure $ Response.infoResponse (iTarget apiRequest) dbStructure
  else
    runDbHandler appState (txMode apiRequest) (Just authRole /= configDbAnonRole) configDbPreparedStatements .
      Middleware.optionalRollback conf apiRequest $
        Middleware.runPgLocals conf authClaims authRole (handleRequest . ctx) apiRequest jsonDbS pgVer

runDbHandler :: AppState.AppState -> SQL.Mode -> Bool -> Bool -> DbHandler b -> Handler IO b
runDbHandler appState mode authenticated prepared handler = do
  dbResp <-
    let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction in
    lift . AppState.usePool appState . transaction SQL.ReadCommitted mode $ runExceptT handler

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

  resultSet <-
     lift . SQL.statement mempty $
      Statements.prepareRead
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

  failNotSingular iAcceptMediaType resultSet
  total <- readTotal ctxConfig ctxApiRequest resultSet countQuery

  pure $ Response.readResponse headersOnly identifier ctxApiRequest total resultSet

readTotal :: AppConfig -> ApiRequest -> ResultSet -> SQL.Snippet -> DbHandler (Maybe Int64)
readTotal _ _ RSPlan{} _ = pure Nothing
readTotal AppConfig{..} ApiRequest{..} RSStandard{rsTableTotal=tableTotal} countQuery =
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
      lift . SQL.statement mempty . Statements.preparePlanRows countQuery $
        configDbPreparedStatements

handleCreate :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleCreate identifier context@RequestContext{..} = do
  let
    ApiRequest{..} = ctxApiRequest
    pkCols = if iPreferRepresentation /= None || isJust iPreferResolution
      then maybe mempty tablePKCols $ HM.lookup identifier $ dbTables ctxDbStructure
      else mempty

  resultSet <- writeQuery MutationCreate identifier True pkCols context

  failNotSingular iAcceptMediaType resultSet

  pure $ Response.createResponse identifier pkCols ctxApiRequest resultSet

handleUpdate :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleUpdate identifier context@(RequestContext _ _ ctxApiRequest@ApiRequest{..} _) = do
  resultSet <- writeQuery MutationUpdate identifier False mempty context
  failNotSingular iAcceptMediaType resultSet
  failsChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resultSet

  pure $ Response.updateResponse ctxApiRequest resultSet

handleSingleUpsert :: QualifiedIdentifier -> RequestContext-> DbHandler Wai.Response
handleSingleUpsert identifier context@(RequestContext _ ctxDbStructure ctxApiRequest _) = do
  let pkCols = maybe mempty tablePKCols $ HM.lookup identifier $ dbTables ctxDbStructure
  resultSet <- writeQuery MutationSingleUpsert identifier False pkCols context
  failPut resultSet
  pure $ Response.singleUpsertResponse ctxApiRequest resultSet

-- Makes sure the querystring pk matches the payload pk
-- e.g. PUT /items?id=eq.1 { "id" : 1, .. } is accepted,
-- PUT /items?id=eq.14 { "id" : 2, .. } is rejected.
-- If this condition is not satisfied then nothing is inserted,
-- check the WHERE for INSERT in QueryBuilder.hs to see how it's done
failPut :: ResultSet -> DbHandler ()
failPut RSPlan{} = pure ()
failPut RSStandard{rsQueryTotal=queryTotal} =
  when (queryTotal /= 1) $ do
    lift SQL.condemn
    throwError Error.PutMatchingPkError

handleDelete :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleDelete identifier context@(RequestContext _ _ ctxApiRequest@ApiRequest{..} _) = do
  resultSet <- writeQuery MutationDelete identifier False mempty context
  failNotSingular iAcceptMediaType resultSet
  failsChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resultSet

  pure $ Response.deleteResponse ctxApiRequest resultSet

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

  resultSet <-
    lift . SQL.statement mempty $
      Statements.prepareCall
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

  failNotSingular iAcceptMediaType resultSet
  pure $ Response.invokeResponse invMethod proc ctxApiRequest resultSet

handleOpenApi :: Bool -> Schema -> RequestContext -> DbHandler Wai.Response
handleOpenApi headersOnly tSchema (RequestContext conf@AppConfig{..} dbStructure apiRequest ctxPgVersion) = do
  body <-
    lift $ case configOpenApiMode of
      OAFollowPriv ->
        Just <$> ((,,)
           <$> SQL.statement [tSchema] (DbStructure.accessibleTables ctxPgVersion configDbPreparedStatements)
           <*> SQL.statement tSchema (DbStructure.accessibleProcs ctxPgVersion configDbPreparedStatements)
           <*> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements))
      OAIgnorePriv ->
        Just <$> ((,,)
              (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ DbStructure.dbTables dbStructure)
              (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ DbStructure.dbProcs dbStructure)
          <$> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements))
      OADisabled ->
        pure Nothing

  pure $ Response.openApiResponse headersOnly body conf dbStructure $ iProfile apiRequest

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

writeQuery :: Mutation -> QualifiedIdentifier -> Bool -> [Text] -> RequestContext -> DbHandler ResultSet
writeQuery mutation identifier@QualifiedIdentifier{..} isInsert pkCols context@RequestContext{..} = do
  readReq <- readRequest identifier context

  mutateReq <-
    liftEither $
      ReqBuilder.mutateRequest mutation qiSchema qiName ctxApiRequest
        pkCols
        readReq

  lift . SQL.statement mempty $
    Statements.prepareWrite
      (QueryBuilder.readRequestToQuery readReq)
      (QueryBuilder.mutateRequestToQuery mutateReq)
      isInsert
      (iAcceptMediaType ctxApiRequest)
      (iPreferRepresentation ctxApiRequest)
      pkCols
      (configDbPreparedStatements ctxConfig)

-- |
-- Fail a response if a single JSON object was requested and not exactly one
-- was found.
failNotSingular :: MediaType -> ResultSet -> DbHandler ()
failNotSingular _ RSPlan{} = pure ()
failNotSingular mediaType RSStandard{rsQueryTotal=queryTotal} =
  when (mediaType == MTSingularJSON && queryTotal /= 1) $ do
    lift SQL.condemn
    throwError $ Error.singularityError queryTotal

failsChangesOffLimits :: Maybe Integer -> ResultSet -> DbHandler ()
failsChangesOffLimits _ RSPlan{} = pure ()
failsChangesOffLimits Nothing _  = pure ()
failsChangesOffLimits (Just maxChanges) RSStandard{rsQueryTotal=queryTotal} =
  when (queryTotal > fromIntegral maxChanges) $ do
    lift SQL.condemn
    throwError $ Error.OffLimitsChangesError queryTotal maxChanges

returnsScalar :: ApiRequest.Target -> Bool
returnsScalar (TargetProc proc _) = Proc.procReturnsScalar proc
returnsScalar _                   = False

readRequest :: Monad m => QualifiedIdentifier -> RequestContext -> Handler m ReadRequest
readRequest QualifiedIdentifier{..} (RequestContext AppConfig{..} dbStructure apiRequest _) =
  liftEither $
    ReqBuilder.readRequest qiSchema qiName configDbMaxRows
      (dbRelationships dbStructure)
      apiRequest

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
        throwError $ Error.BinaryFieldError mediaType
  | otherwise =
      return Nothing
  where
    mediaType = iAcceptMediaType ctxApiRequest
    isRawMediaType = mediaType `elem` configRawMediaTypes ctxConfig `union` [MTOctetStream, MTTextPlain, MTTextXML] || isRawPlan mediaType
    isRawPlan mt = case mt of
      MTPlan (MTPlanAttrs (Just MTOctetStream) _ _) -> True
      MTPlan (MTPlanAttrs (Just MTTextPlain) _ _)   -> True
      MTPlan (MTPlanAttrs (Just MTTextXML) _ _)     -> True
      _                                             -> False
