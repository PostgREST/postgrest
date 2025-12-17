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
  ( postgrest
  , run
  ) where


import Control.Monad.Except     (liftEither)
import Data.Either.Combinators  (mapLeft, whenLeft)
import Data.Maybe               (fromJust)
import Data.String              (IsString (..))
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort,
                                 setServerName)

import qualified Data.Text.Encoding       as T
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp

import qualified PostgREST.Admin      as Admin
import qualified PostgREST.ApiRequest as ApiRequest
import qualified PostgREST.AppState   as AppState
import qualified PostgREST.Auth       as Auth
import qualified PostgREST.Cors       as Cors
import qualified PostgREST.Error      as Error
import qualified PostgREST.Listener   as Listener
import qualified PostgREST.Logger     as Logger
import qualified PostgREST.MainTx     as MainTx
import qualified PostgREST.Plan       as Plan
import qualified PostgREST.Query      as Query
import qualified PostgREST.Response   as Response
import qualified PostgREST.Unix       as Unix (installSignalHandlers)

import PostgREST.ApiRequest           (ApiRequest (..))
import PostgREST.AppState             (AppState)
import PostgREST.Auth.Types           (AuthResult (..))
import PostgREST.Config               (AppConfig (..), LogLevel (..))
import PostgREST.Error                (Error)
import PostgREST.Network              (resolveSocketToAddress)
import PostgREST.Observation          (Observation (..))
import PostgREST.Response.Performance (ServerTiming (..),
                                       serverTimingHeader)
import PostgREST.SchemaCache          (SchemaCache (..))
import PostgREST.Version              (docsVersion, prettyVersion)

import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L
import qualified Network.HTTP.Types    as HTTP
import           Protolude             hiding (Handler)
import           System.TimeIt         (timeItT)

type Handler = ExceptT Error

run :: AppState -> IO ()
run appState = do
  let observer = AppState.getObserver appState
  conf@AppConfig{..} <- AppState.getConfig appState

  AppState.schemaCacheLoader appState -- Loads the initial SchemaCache
  Unix.installSignalHandlers (AppState.getMainThreadId appState) (AppState.schemaCacheLoader appState) (AppState.readInDbConfig False appState)

  Listener.runListener appState

  Admin.runAdmin appState (serverSettings conf)

  let app = postgrest configLogLevel appState (AppState.schemaCacheLoader appState)

  do
    address <- resolveSocketToAddress (AppState.getSocketREST appState)
    observer $ AppServerAddressObs address

  Warp.runSettingsSocket (serverSettings conf) (AppState.getSocketREST appState) app

serverSettings :: AppConfig -> Warp.Settings
serverSettings AppConfig{..} =
  defaultSettings
    & setHost (fromString $ toS configServerHost)
    & setPort configServerPort
    & setServerName ("postgrest/" <> prettyVersion)

-- | PostgREST application
postgrest :: LogLevel -> AppState.AppState -> IO () -> Wai.Application
postgrest logLevel appState connWorker =
  traceHeaderMiddleware appState .
  Cors.middleware appState .
  Auth.middleware appState .
  Logger.middleware logLevel Auth.getRole $
    -- fromJust can be used, because the auth middleware will **always** add
    -- some AuthResult to the vault.
    \req respond -> case fromJust $ Auth.getResult req of
      Left err -> respond $ Error.errorResponseFor err
      Right authResult -> do
        appConf <- AppState.getConfig appState -- the config must be read again because it can reload
        maybeSchemaCache <- AppState.getSchemaCache appState

        let
          eitherResponse :: IO (Either Error Wai.Response)
          eitherResponse =
            runExceptT $ postgrestResponse appState appConf maybeSchemaCache authResult req

        response <- either Error.errorResponseFor identity <$> eitherResponse
        -- Launch the connWorker when the connection is down. The postgrest
        -- function can respond successfully (with a stale schema cache) before
        -- the connWorker is done. However, when there's an empty schema cache
        -- postgrest responds with the error `PGRST002`; this means that the schema
        -- cache is still loading, so we don't launch the connWorker here because
        -- it would duplicate the loading process, e.g. https://github.com/PostgREST/postgrest/issues/3704
        -- TODO: this process may be unnecessary when the Listener is enabled. Revisit once https://github.com/PostgREST/postgrest/issues/1766 is done
        when (isServiceUnavailable response && isJust maybeSchemaCache) connWorker
        resp <- do
          delay <- AppState.getNextDelay appState
          return $ addRetryHint delay response
        respond resp

postgrestResponse
  :: AppState.AppState
  -> AppConfig
  -> Maybe SchemaCache
  -> AuthResult
  -> Wai.Request
  -> Handler IO Wai.Response
postgrestResponse appState conf@AppConfig{..} maybeSchemaCache authResult@AuthResult{..} req = do
  let observer = AppState.getObserver appState

  sCache <-
    case maybeSchemaCache of
      Just sCache ->
        return sCache
      Nothing -> do
        lift $ observer SchemaCacheEmptyObs
        throwError Error.NoSchemaCacheError

  body <- lift $ Wai.strictRequestBody req

  let jwtTime = if configServerTimingEnabled then Auth.getJwtDur req else Nothing
      timezones = dbTimezones sCache
      prefs = ApiRequest.userPreferences conf req timezones

  (parseTime, apiReq@ApiRequest{..}) <- withTiming $ liftEither . mapLeft Error.ApiRequestError $ ApiRequest.userApiRequest conf prefs req body
  (planTime, plan)                   <- withTiming $ liftEither $ Plan.actionPlan iAction conf apiReq sCache

  pgVer <- lift $ AppState.getPgVersion appState
  let mainQ = Query.mainQuery pgVer plan conf apiReq authResult configDbPreRequest
      tx = MainTx.mainTx mainQ conf authResult apiReq plan sCache
      obsQuery s = when configLogQuery $ observer $ QueryObs mainQ s

  (txTime, txResult) <- withTiming $ do
    case tx of
      MainTx.NoDbTx r -> pure r
      MainTx.DbTx{..} -> do
        dbRes <- lift $ AppState.usePool appState (dqTransaction dqIsoLevel dqTxMode $ runExceptT dqDbHandler)
        let eitherResp = join $ mapLeft (Error.PgErr . Error.PgError (Just authRole /= configDbAnonRole)) dbRes

        -- TODO: we use obsQuery twice, one here and one below because in case of an error with the usePool above, the request will finish here and return an error message.
        -- This is because of a combination of ExceptT + our Error module which has Wai.responseLBS.
        -- This needs refactoring so only the below obsQuery is used.
        lift $ whenLeft eitherResp $ obsQuery . Error.status
        liftEither eitherResp

  (respTime, resp) <- withTiming $ do
    let response = Response.actionResponse txResult apiReq (T.decodeUtf8 prettyVersion, docsVersion) conf sCache iSchema iNegotiatedByProfile
        status' = either Error.status Response.pgrstStatus response

    -- TODO: see above obsQuery, only this obsQuery should remain after refactoring (because the QueryObs depends on the status)
    lift $ obsQuery status'
    liftEither response

  return $ toWaiResponse (ServerTiming jwtTime parseTime planTime txTime respTime) resp

  where
    toWaiResponse :: ServerTiming -> Response.PgrstResponse -> Wai.Response
    toWaiResponse timing (Response.PgrstResponse st hdrs bod) = Wai.responseLBS st (hdrs ++ ([serverTimingHeader timing | configServerTimingEnabled])) bod

    withTiming :: Handler IO a -> Handler IO (Maybe Double, a)
    withTiming f = if configServerTimingEnabled
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
