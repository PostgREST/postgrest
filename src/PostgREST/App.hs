{-|
Module      : PostgREST.App
Description : PostgREST main application

This module is in charge of mapping HTTP requests to PostgreSQL queries.
Some of its functionality includes:

- Mapping HTTP request methods to proper SQL statements. For example, a GET request is translated to executing a SELECT query in a read-only TRANSACTION.
- Producing HTTP Headers according to RFCs.
- Content Negotiation
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.App
  ( SignalHandlerInstaller
  , SocketRunner
  , postgrest
  , run
  ) where


import Control.Monad.Except     (liftEither)
import Data.Either.Combinators  (mapLeft, whenLeft)
import Data.Maybe               (fromJust)
import Data.String              (IsString (..))
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort,
                                 setServerName)
import System.Posix.Types       (FileMode)

import qualified Hasql.Pool                 as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp

import qualified PostgREST.ApiRequest       as ApiRequest
import qualified PostgREST.ApiRequest.Types as ApiRequestTypes
import qualified PostgREST.AppState         as AppState
import qualified PostgREST.Auth             as Auth
import qualified PostgREST.Cors             as Cors
import qualified PostgREST.Error            as Error
import qualified PostgREST.Logger           as Logger
import qualified PostgREST.Plan             as Plan
import qualified PostgREST.Query            as Query
import qualified PostgREST.Response         as Response
import qualified PostgREST.Workers          as Workers

import PostgREST.ApiRequest       (Action (..), ApiRequest (..),
                                   Mutation (..), Target (..))
import PostgREST.AppState         (AppState)
import PostgREST.Auth             (AuthResult (..))
import PostgREST.Config           (AppConfig (..))
import PostgREST.Config.PgVersion (PgVersion (..))
import PostgREST.Error            (Error)
import PostgREST.Query            (DbHandler)
import PostgREST.SchemaCache      (SchemaCache (..))
import PostgREST.Version          (prettyVersion)

import Protolude hiding (Handler)

type Handler = ExceptT Error

type SignalHandlerInstaller = AppState -> IO()

type SocketRunner = Warp.Settings -> Wai.Application -> FileMode -> FilePath -> IO()

run :: SignalHandlerInstaller -> Maybe SocketRunner -> AppState -> IO ()
run installHandlers maybeRunWithSocket appState = do
  conf@AppConfig{..} <- AppState.getConfig appState
  Workers.connectionWorker appState -- Loads the initial SchemaCache
  installHandlers appState
  -- reload schema cache + config on NOTIFY
  Workers.runListener conf appState

  Workers.runAdmin conf appState $ serverSettings conf

  let app = postgrest conf appState (Workers.connectionWorker appState)

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

serverSettings :: AppConfig -> Warp.Settings
serverSettings AppConfig{..} =
  defaultSettings
    & setHost (fromString $ toS configServerHost)
    & setPort configServerPort
    & setServerName ("postgrest/" <> prettyVersion)

-- | PostgREST application
postgrest :: AppConfig -> AppState.AppState -> IO () -> Wai.Application
postgrest conf appState connWorker =
  Response.traceHeaderMiddleware conf .
  Cors.middleware .
  Auth.middleware appState .
  Logger.middleware (configLogLevel conf) $
    -- fromJust can be used, because the auth middleware will **always** add
    -- some AuthResult to the vault.
    \req respond -> case fromJust $ Auth.getResult req of
      Left err -> respond $ Error.errorResponseFor err
      Right authResult -> do
        appConf <- AppState.getConfig appState -- the config must be read again because it can reload
        maybeSchemaCache <- AppState.getSchemaCache appState
        pgVer <- AppState.getPgVersion appState

        let
          eitherResponse :: IO (Either Error Wai.Response)
          eitherResponse =
            runExceptT $ postgrestResponse appState appConf maybeSchemaCache pgVer authResult req

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
  -> Maybe SchemaCache
  -> PgVersion
  -> AuthResult
  -> Wai.Request
  -> Handler IO Wai.Response
postgrestResponse appState conf@AppConfig{..} maybeSchemaCache pgVer authResult@AuthResult{..} req = do
  sCache <-
    case maybeSchemaCache of
      Just sCache ->
        return sCache
      Nothing ->
        throwError Error.NoSchemaCacheError

  body <- lift $ Wai.strictRequestBody req

  apiRequest <-
    liftEither . mapLeft Error.ApiRequestError $
      ApiRequest.userApiRequest conf sCache req body

  Response.optionalRollback conf apiRequest $
    handleRequest authResult conf appState (Just authRole /= configDbAnonRole) configDbPreparedStatements pgVer apiRequest sCache

runDbHandler :: AppState.AppState -> SQL.Mode -> Bool -> Bool -> DbHandler b -> Handler IO b
runDbHandler appState mode authenticated prepared handler = do
  dbResp <- lift $ do
    let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction
    res <- AppState.usePool appState . transaction SQL.ReadCommitted mode $ runExceptT handler
    whenLeft res (\case
      SQL.AcquisitionTimeoutUsageError -> AppState.debounceLogAcquisitionTimeout appState -- this can happen rapidly for many requests, so we debounce
      _ -> pure ())
    return res

  resp <-
    liftEither . mapLeft Error.PgErr $
      mapLeft (Error.PgError authenticated) dbResp

  liftEither resp

handleRequest :: AuthResult -> AppConfig -> AppState.AppState -> Bool -> Bool -> PgVersion -> ApiRequest -> SchemaCache -> Handler IO Wai.Response
handleRequest AuthResult{..} conf appState authenticated prepared pgVer apiReq@ApiRequest{..} sCache =
  case (iAction, iTarget) of
    (ActionRead headersOnly, TargetIdent identifier) -> do
      rPlan <- liftEither $ Plan.readPlan identifier conf sCache apiReq
      resultSet <- runQuery Plan.readPlanTxMode $ Query.readQuery rPlan conf apiReq
      return $ Response.readResponse headersOnly identifier apiReq resultSet

    (ActionMutate MutationCreate, TargetIdent identifier) -> do
      mrPlan <- liftEither $ Plan.mutateReadPlan MutationCreate apiReq identifier conf sCache
      resultSet <- runQuery (Plan.mrTxMode mrPlan) $ Query.createQuery mrPlan apiReq conf
      return $ Response.createResponse identifier mrPlan apiReq resultSet

    (ActionMutate MutationUpdate, TargetIdent identifier) -> do
      mrPlan <- liftEither $ Plan.mutateReadPlan MutationUpdate apiReq identifier conf sCache
      resultSet <- runQuery (Plan.mrTxMode mrPlan) $ Query.updateQuery mrPlan apiReq conf
      return $ Response.updateResponse apiReq resultSet

    (ActionMutate MutationSingleUpsert, TargetIdent identifier) -> do
      mrPlan <- liftEither $ Plan.mutateReadPlan MutationSingleUpsert apiReq identifier conf sCache
      resultSet <- runQuery (Plan.mrTxMode mrPlan) $ Query.singleUpsertQuery mrPlan apiReq conf
      return $ Response.singleUpsertResponse apiReq resultSet

    (ActionMutate MutationDelete, TargetIdent identifier) -> do
      mrPlan <- liftEither $ Plan.mutateReadPlan MutationDelete apiReq identifier conf sCache
      resultSet <- runQuery (Plan.mrTxMode mrPlan) $ Query.deleteQuery mrPlan apiReq conf
      return $ Response.deleteResponse apiReq resultSet

    (ActionInvoke invMethod, TargetProc proc _) -> do
      cPlan <- liftEither $ Plan.callReadPlan proc conf sCache apiReq invMethod
      resultSet <- runQuery (Plan.crTxMode cPlan) $ Query.invokeQuery proc cPlan apiReq conf
      return $ Response.invokeResponse invMethod proc apiReq resultSet

    (ActionInspect headersOnly, TargetDefaultSpec tSchema) -> do
      oaiResult <- runQuery Plan.inspectPlanTxMode $ Query.openApiQuery sCache pgVer conf tSchema
      return $ Response.openApiResponse headersOnly oaiResult conf sCache iSchema iNegotiatedByProfile

    (ActionInfo, TargetIdent identifier) ->
      return $ Response.infoIdentResponse identifier sCache

    (ActionInfo, TargetProc proc _) ->
      return $ Response.infoProcResponse proc

    (ActionInfo, TargetDefaultSpec _) ->
      return Response.infoRootResponse

    _ ->
      -- This is unreachable as the ApiRequest.hs rejects it before
      -- TODO Refactor the Action/Target types to remove this line
      throwError $ Error.ApiRequestError ApiRequestTypes.NotFound
  where
    runQuery mode query =
      runDbHandler appState mode authenticated prepared $ do
        Query.setPgLocals conf authClaims authRole apiReq pgVer
        Query.runPreReq conf
        query
