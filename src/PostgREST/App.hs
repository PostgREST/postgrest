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

import qualified Data.HashMap.Strict        as HM
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp

import qualified PostgREST.Admin                    as Admin
import qualified PostgREST.AppState                 as AppState
import qualified PostgREST.Auth                     as Auth
import qualified PostgREST.Cors                     as Cors
import qualified PostgREST.Error                    as Error
import qualified PostgREST.Logger                   as Logger
import qualified PostgREST.Middleware               as Middleware
import qualified PostgREST.Query                    as Query
import qualified PostgREST.Request.ApiRequest       as ApiRequest
import qualified PostgREST.Request.DbRequestBuilder as ReqBuilder
import qualified PostgREST.Request.MutateQuery      as MutateRequest
import qualified PostgREST.Request.Types            as ApiRequestTypes
import qualified PostgREST.Response                 as Response

import PostgREST.AppState                (AppState)
import PostgREST.Auth                    (AuthResult (..))
import PostgREST.Config                  (AppConfig (..),
                                          LogLevel (..))
import PostgREST.Config.PgVersion        (PgVersion (..))
import PostgREST.DbStructure             (DbStructure (..))
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)
import PostgREST.DbStructure.Proc        (ProcDescription (..))
import PostgREST.DbStructure.Table       (Table (..))
import PostgREST.Error                   (Error)
import PostgREST.MediaType               (MTPlanAttrs (..),
                                          MediaType (..))
import PostgREST.Query                   (DbHandler)
import PostgREST.Request.ApiRequest      (Action (..),
                                          ApiRequest (..),
                                          InvokeMethod (..),
                                          Mutation (..), Target (..))
import PostgREST.Request.Preferences     (PreferRepresentation (..))
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
  dbStructure <-
    case maybeDbStructure of
      Just dbStructure ->
        return dbStructure
      Nothing ->
        throwError Error.NoSchemaCacheError

  body <- lift $ Wai.strictRequestBody req

  apiRequest <-
    liftEither . mapLeft Error.ApiRequestError $
      ApiRequest.userApiRequest conf dbStructure req body

  let ctx apiReq = RequestContext conf dbStructure apiReq pgVer

  if iAction apiRequest == ActionInfo then
    pure $ Response.infoResponse (iTarget apiRequest) dbStructure
  else
    runDbHandler appState (Query.txMode apiRequest) (Just authRole /= configDbAnonRole) configDbPreparedStatements .
      Middleware.optionalRollback conf apiRequest $ do
        Query.setPgLocals conf authClaims authRole apiRequest jsonDbS pgVer
        handleRequest (ctx apiRequest)

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
  req <- liftEither $ readRequest identifier context
  bField <- binaryField context req

  (resultSet, total) <- Query.readQuery req ctxConfig ctxApiRequest bField

  pure $ Response.readResponse headersOnly identifier ctxApiRequest total resultSet

handleCreate :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleCreate identifier context@RequestContext{..} = do
  let
    ApiRequest{..} = ctxApiRequest
    pkCols = if iPreferRepresentation /= None || isJust iPreferResolution
      then maybe mempty tablePKCols $ HM.lookup identifier $ dbTables ctxDbStructure
      else mempty

  (mutateReq, readReq) <- liftEither $ writeRequest MutationCreate identifier context pkCols

  resultSet <- Query.createQuery mutateReq readReq pkCols ctxApiRequest ctxConfig

  pure $ Response.createResponse identifier pkCols ctxApiRequest resultSet

handleUpdate :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleUpdate identifier context@(RequestContext ctxConfig _ ctxApiRequest _) = do
  (mutateReq, readReq) <- liftEither $ writeRequest MutationUpdate identifier context mempty
  resultSet <- Query.updateQuery mutateReq readReq ctxApiRequest ctxConfig
  pure $ Response.updateResponse ctxApiRequest resultSet

handleSingleUpsert :: QualifiedIdentifier -> RequestContext-> DbHandler Wai.Response
handleSingleUpsert identifier context@(RequestContext ctxConfig ctxDbStructure ctxApiRequest _) = do
  let pkCols = maybe mempty tablePKCols $ HM.lookup identifier $ dbTables ctxDbStructure
  (mutateReq, readReq) <- liftEither $ writeRequest MutationSingleUpsert identifier context pkCols
  resultSet <- Query.singleUpsertQuery mutateReq readReq ctxApiRequest ctxConfig
  pure $ Response.singleUpsertResponse ctxApiRequest resultSet

handleDelete :: QualifiedIdentifier -> RequestContext -> DbHandler Wai.Response
handleDelete identifier context@(RequestContext ctxConfig _ ctxApiRequest _) = do
  (mutateReq, readReq) <- liftEither $ writeRequest MutationDelete identifier context mempty
  resultSet <- Query.deleteQuery mutateReq readReq ctxApiRequest ctxConfig
  pure $ Response.deleteResponse ctxApiRequest resultSet

handleInvoke :: InvokeMethod -> ProcDescription -> RequestContext -> DbHandler Wai.Response
handleInvoke invMethod proc context@RequestContext{..} = do
  let
    identifier =
      QualifiedIdentifier
        (pdSchema proc)
        (fromMaybe (pdName proc) $ Proc.procTableName proc)

  readReq <- liftEither $ readRequest identifier context
  bField <- binaryField context readReq
  let callReq = ReqBuilder.callRequest proc ctxApiRequest readReq

  resultSet <- Query.invokeQuery proc callReq readReq ctxApiRequest bField ctxConfig

  pure $ Response.invokeResponse invMethod proc ctxApiRequest resultSet

handleOpenApi :: Bool -> Schema -> RequestContext -> DbHandler Wai.Response
handleOpenApi headersOnly tSchema (RequestContext conf dbStructure apiRequest pgVer) = do
  oaiResult <- Query.openApiQuery dbStructure pgVer conf tSchema
  pure $ Response.openApiResponse headersOnly oaiResult conf dbStructure $ iProfile apiRequest

writeRequest :: Mutation -> QualifiedIdentifier -> RequestContext -> [FieldName] -> Either Error (MutateRequest.MutateRequest, ReadRequest)
writeRequest mutation identifier@QualifiedIdentifier{..} context@RequestContext{..} pkCols = do
  readReq <- readRequest identifier context
  mutateReq <- ReqBuilder.mutateRequest mutation qiSchema qiName ctxApiRequest pkCols readReq
  pure (mutateReq, readReq)

returnsScalar :: ApiRequest.Target -> Bool
returnsScalar (TargetProc proc _) = Proc.procReturnsScalar proc
returnsScalar _                   = False

readRequest :: QualifiedIdentifier -> RequestContext -> Either Error ReadRequest
readRequest QualifiedIdentifier{..} (RequestContext AppConfig{..} dbStructure apiRequest _) =
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
