{-|
Module      : PostgREST.App
Description : PostgREST main application

This module is in charge of mapping HTTP requests to PostgreSQL queries.
Some of its functionality includes:

- Mapping HTTP request methods to proper SQL statements. For example, a GET request is translated to executing a SELECT query in a read-only TRANSACTION.
- Producing HTTP Headers according to RFCs.
- Content Negotiation
-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.App
  ( postgrest
  , run
  ) where


import Control.Monad.Except     (liftEither)
import Data.Either.Combinators  (mapLeft)
import Data.Maybe               (fromJust)
import Data.String              (IsString (..))
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort,
                                 setServerName)

import qualified Data.HashMap.Strict        as HM
import qualified Data.Text.Encoding         as T
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp

import qualified PostgREST.Admin      as Admin
import qualified PostgREST.ApiRequest as ApiRequest
import qualified PostgREST.AppState   as AppState
import qualified PostgREST.Auth       as Auth
import qualified PostgREST.Cors       as Cors
import qualified PostgREST.Error      as Error
import qualified PostgREST.Logger     as Logger
import qualified PostgREST.Plan       as Plan
import qualified PostgREST.Query      as Query
import qualified PostgREST.Response   as Response
import qualified PostgREST.Unix       as Unix (installSignalHandlers)

import PostgREST.ApiRequest           (Action (..), ApiRequest (..),
                                       DbAction (..))
import PostgREST.AppState             (AppState)
import PostgREST.Auth                 (AuthResult (..))
import PostgREST.Config               (AppConfig (..), LogLevel (..))
import PostgREST.Config.PgVersion     (PgVersion (..))
import PostgREST.Error                (Error)
import PostgREST.Observation          (Observation (..))
import PostgREST.Query                (DbHandler)
import PostgREST.Response.Performance (ServerTiming (..),
                                       serverTimingHeader)
import PostgREST.SchemaCache          (SchemaCache (..))
import PostgREST.SchemaCache.Routine  (Routine (..))
import PostgREST.Version              (docsVersion, prettyVersion)

import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L
import qualified Network.HTTP.Types    as HTTP
import qualified Network.Socket        as NS
import           Protolude             hiding (Handler)
import           System.TimeIt         (timeItT)

type Handler = ExceptT Error

run :: AppState -> (Observation -> IO ()) -> IO ()
run appState observer = do
  observer $ AppServerStartObs prettyVersion

  conf@AppConfig{..} <- AppState.getConfig appState
  AppState.connectionWorker appState -- Loads the initial SchemaCache
  Unix.installSignalHandlers (AppState.getMainThreadId appState) (AppState.connectionWorker appState) (AppState.reReadConfig False appState observer)
  -- reload schema cache + config on NOTIFY
  AppState.runListener conf appState observer

  Admin.runAdmin conf appState (serverSettings conf) observer

  let app = postgrest configLogLevel appState (AppState.connectionWorker appState) observer

  case configServerUnixSocket of
    Just path -> do
      observer $ AppServerUnixObs path
    Nothing   -> do
      port <- NS.socketPort $ AppState.getSocketREST appState
      observer $ AppServerPortObs port

  Warp.runSettingsSocket (serverSettings conf) (AppState.getSocketREST appState) app

serverSettings :: AppConfig -> Warp.Settings
serverSettings AppConfig{..} =
  defaultSettings
    & setHost (fromString $ toS configServerHost)
    & setPort configServerPort
    & setServerName ("postgrest/" <> prettyVersion)

-- | PostgREST application
postgrest :: LogLevel -> AppState.AppState -> IO () -> (Observation -> IO ()) -> Wai.Application
postgrest logLevel appState connWorker observer =
  traceHeaderMiddleware appState .
  Cors.middleware appState .
  Auth.middleware appState .
  Logger.middleware logLevel $
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
            runExceptT $ postgrestResponse appState appConf maybeSchemaCache pgVer authResult req observer

        response <- either Error.errorResponseFor identity <$> eitherResponse
        -- Launch the connWorker when the connection is down.  The postgrest
        -- function can respond successfully (with a stale schema cache) before
        -- the connWorker is done.
        when (isServiceUnavailable response) connWorker
        resp <- do
          delay <- AppState.getRetryNextIn appState
          return $ addRetryHint delay response
        respond resp

postgrestResponse
  :: AppState.AppState
  -> AppConfig
  -> Maybe SchemaCache
  -> PgVersion
  -> AuthResult
  -> Wai.Request
  -> (Observation -> IO ())
  -> Handler IO Wai.Response
postgrestResponse appState conf@AppConfig{..} maybeSchemaCache pgVer authResult@AuthResult{..} req observer = do
  sCache <-
    case maybeSchemaCache of
      Just sCache ->
        return sCache
      Nothing ->
        throwError Error.NoSchemaCacheError

  body <- lift $ Wai.strictRequestBody req

  (parseTime, apiRequest) <-
    calcTiming configServerTimingEnabled $
      liftEither . mapLeft Error.ApiRequestError $
        ApiRequest.userApiRequest conf req body sCache

  let jwtTime = if configServerTimingEnabled then Auth.getJwtDur req else Nothing
  handleRequest authResult conf appState (Just authRole /= configDbAnonRole) configDbPreparedStatements pgVer apiRequest sCache jwtTime parseTime observer

runDbHandler :: AppState.AppState -> AppConfig -> SQL.IsolationLevel -> SQL.Mode -> Bool -> Bool -> (Observation -> IO ()) -> DbHandler b -> Handler IO b
runDbHandler appState config isoLvl mode authenticated prepared observer handler = do
  dbResp <- lift $ do
    let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction
    AppState.usePool appState config (transaction isoLvl mode $ runExceptT handler) observer

  resp <-
    liftEither . mapLeft Error.PgErr $
      mapLeft (Error.PgError authenticated) dbResp

  liftEither resp

handleRequest :: AuthResult -> AppConfig -> AppState.AppState -> Bool -> Bool -> PgVersion -> ApiRequest -> SchemaCache ->
                Maybe Double -> Maybe Double -> (Observation -> IO ()) -> Handler IO Wai.Response
handleRequest AuthResult{..} conf appState authenticated prepared pgVer apiReq@ApiRequest{..} sCache jwtTime parseTime observer =
  case iAction of
    ActDb dbAct -> do
      (planTime', plan) <- withTiming $ liftEither $ Plan.actionPlan dbAct conf apiReq sCache
      (txTime', resultSet) <- withTiming $ runQuery (planIsoLvl plan) (planFunSettings plan) (Plan.pTxMode plan) $ Query.actionQuery plan conf apiReq pgVer
      (respTime', pgrst) <- withTiming $ liftEither $ Response.actionResponse plan (dbActQi dbAct) apiReq resultSet
      return $ pgrstResponse (ServerTiming jwtTime parseTime planTime' txTime' respTime') pgrst

    ActSchemaRead tSchema headersOnly -> do
      (planTime', iPlan) <- withTiming $ liftEither $ Plan.inspectPlan apiReq headersOnly tSchema
      (txTime', oaiResult) <- withTiming $ runQuery roleIsoLvl mempty (Plan.ipTxmode iPlan) $ Query.openApiQuery iPlan conf sCache pgVer
      (respTime', pgrst) <- withTiming $ liftEither $ Response.openApiResponse iPlan (T.decodeUtf8 prettyVersion, docsVersion) oaiResult conf sCache iSchema iNegotiatedByProfile
      return $ pgrstResponse (ServerTiming jwtTime parseTime planTime' txTime' respTime') pgrst

    ActRelationInfo identifier -> do
      (respTime', pgrst) <- withTiming $ liftEither $ Response.infoIdentResponse identifier sCache
      return $ pgrstResponse (ServerTiming jwtTime parseTime Nothing Nothing respTime') pgrst

    ActRoutineInfo identifier -> do
      (planTime', cPlan) <- withTiming $ liftEither $ Plan.callReadPlan identifier conf sCache apiReq $ ApiRequest.InvRead True
      (respTime', pgrst) <- withTiming $ liftEither $ Response.infoProcResponse (Plan.crProc cPlan)
      return $ pgrstResponse (ServerTiming jwtTime parseTime planTime' Nothing respTime') pgrst

    ActSchemaInfo -> do
      (respTime', pgrst) <- withTiming $ liftEither Response.infoRootResponse
      return $ pgrstResponse (ServerTiming jwtTime parseTime Nothing Nothing respTime') pgrst

  where
    roleSettings = fromMaybe mempty (HM.lookup authRole $ configRoleSettings conf)
    roleIsoLvl = HM.findWithDefault SQL.ReadCommitted authRole $ configRoleIsoLvl conf
    runQuery isoLvl funcSets mode query =
      runDbHandler appState conf isoLvl mode authenticated prepared observer $ do
        Query.setPgLocals conf authClaims authRole (HM.toList roleSettings) funcSets apiReq
        Query.runPreReq conf
        query

    planIsoLvl (Plan.CallReadPlan{crProc}) = fromMaybe roleIsoLvl $ pdIsoLvl crProc
    planIsoLvl _ = roleIsoLvl

    planFunSettings (Plan.CallReadPlan{crProc}) = pdFuncSettings crProc
    planFunSettings _ = mempty

    pgrstResponse :: ServerTiming -> Response.PgrstResponse -> Wai.Response
    pgrstResponse timing (Response.PgrstResponse st hdrs bod) = Wai.responseLBS st (hdrs ++ ([serverTimingHeader timing | configServerTimingEnabled conf])) bod

    withTiming = calcTiming $ configServerTimingEnabled conf

calcTiming :: Bool -> Handler IO a -> Handler IO (Maybe Double, a)
calcTiming timingEnabled f = if timingEnabled
    then do
      (t, r) <- timeItT f
      pure (Just t, r)
    else do
      r <- f
      pure (Nothing, r)

traceHeaderMiddleware :: AppState -> Wai.Middleware
traceHeaderMiddleware appState app req respond = do
  conf <- AppState.getConfig appState

  case configServerTraceHeader conf of
    Nothing -> app req respond
    Just hdr ->
      let hdrVal = L.lookup hdr $ Wai.requestHeaders req in
      app req (respond . Wai.mapResponseHeaders ([(hdr, fromMaybe mempty hdrVal)] ++))

addRetryHint :: Int -> Wai.Response -> Wai.Response
addRetryHint delay response = do
  let h = ("Retry-After", BS.pack $ show delay)
  Wai.mapResponseHeaders (\hs -> if isServiceUnavailable response then h:hs else hs) response

isServiceUnavailable :: Wai.Response -> Bool
isServiceUnavailable response = Wai.responseStatus response == HTTP.status503
