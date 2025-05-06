{-|
Module      : PostgREST.App
Description : PostgREST main application

This module is in charge of mapping HTTP requests to PostgreSQL queries.
Some of its functionality includes:

- Mapping HTTP request methods to proper SQL statements. For example, a GET request is translated to executing a SELECT query in a read-only TRANSACTION.
- Producing HTTP Headers according to RFCs.
- Content Negotiation
-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module PostgREST.App
  ( postgrest
  , run
  ) where


import GHC.IO.Exception (IOErrorType (..))
import System.IO.Error  (ioeGetErrorType)

import Control.Monad.Except     (liftEither)
import Control.Monad.Extra      (whenJust)
import Data.Either.Combinators  (mapLeft, whenLeft)
import Data.String              (IsString (..))
import Network.Wai.Handler.Warp (defaultSettings, setHost,
                                 setOnException, setPort,
                                 setServerName)

import qualified Data.Text.Encoding       as T
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Header       as WaiHeader

import qualified PostgREST.Admin      as Admin
import qualified PostgREST.ApiRequest as ApiRequest
import qualified PostgREST.AppState   as AppState
import qualified PostgREST.Auth       as Auth
import qualified PostgREST.Cors       as Cors
import qualified PostgREST.Error      as Error
import qualified PostgREST.Listener   as Listener
import qualified PostgREST.MainTx     as MainTx
import qualified PostgREST.Plan       as Plan
import qualified PostgREST.Query      as Query
import qualified PostgREST.Response   as Response
import qualified PostgREST.Unix       as Unix (installSignalHandlers)

import PostgREST.ApiRequest           (ApiRequest (..))
import PostgREST.AppState             (AppState)
import PostgREST.Auth.Types           (AuthResult (..))
import PostgREST.Config               (AppConfig (..))
import PostgREST.Error                (Error)
import PostgREST.Network              (resolveSocketToAddress)
import PostgREST.Observation          (Observation (..))
import PostgREST.Response.Performance (ServerTiming (..),
                                       serverTimingHeader)
import PostgREST.SchemaCache          (SchemaCache (..))
import PostgREST.TimeIt               (timeItT)
import PostgREST.Version              (docsVersion, prettyVersion)

import qualified Data.ByteString.Char8     as BS
import qualified Data.List                 as L
import           Data.Streaming.Network    (bindPortTCP,
                                            bindRandomPortTCP)
import qualified Data.Text                 as T
import qualified Network.HTTP.Types        as HTTP
import qualified Network.HTTP.Types.Header as HTTP (hVary)
import qualified Network.Socket            as NS
import           PostgREST.Unix            (createAndBindDomainSocket)
import           Protolude                 hiding (Handler)

run :: AppState -> IO ()
run appState = do
  conf <- AppState.getConfig appState

  AppState.schemaCacheLoader appState -- Loads the initial SchemaCache
  (mainSocket, adminSocket) <- initSockets conf
  let closeSockets = do
        whenJust adminSocket NS.close
        NS.close mainSocket
  Unix.installSignalHandlers observer closeSockets (AppState.schemaCacheLoader appState) (AppState.readInDbConfig False appState)

  Listener.runListener appState

  Admin.runAdmin appState adminSocket mainSocket (serverSettings conf)

  let app = postgrest appState (AppState.schemaCacheLoader appState)

  do
    address <- resolveSocketToAddress mainSocket
    observer $ AppServerAddressObs address

  Warp.runSettingsSocket (serverSettings conf & setOnException onWarpException) mainSocket app
  where
    observer = AppState.getObserver appState

    onWarpException :: Maybe Wai.Request -> SomeException -> IO ()
    onWarpException _ ex =
      when (shouldDisplayException ex) $
        observer $ WarpServerObs $ show ex

    -- Similar to wai defaultShouldDisplayException in
    -- https://github.com/yesodweb/wai//blob/8c3882c60f6abe043889fc20c7efd3fa9747fa4a/warp/Network/Wai/Handler/Warp/Settings.hs#L251-L258
    -- but without omitting AsyncException since it's important to log for ThreadKilled, StackOverflow and other cases.
    -- We want to reuse this to avoid flooding the logs for some transient failure cases.
    shouldDisplayException :: SomeException -> Bool
    shouldDisplayException se
        | Just (_ :: Warp.InvalidRequest) <- fromException se = False
        | Just (ioeGetErrorType -> et) <- fromException se, et == ResourceVanished || et == InvalidArgument = False
        | otherwise = True

serverSettings :: AppConfig -> Warp.Settings
serverSettings AppConfig{..} =
  defaultSettings
    & setHost (fromString $ toS configServerHost)
    & setPort configServerPort
    & setServerName ("postgrest/" <> prettyVersion)

-- | PostgREST application
postgrest :: AppState.AppState -> IO () -> Wai.Application
postgrest appState connWorker =
  traceHeaderMiddleware appState .
  Cors.middleware appState $
    \req respond -> do
      appConf@AppConfig{..} <- AppState.getConfig appState -- the config must be read again because it can reload
      maybeSchemaCache <- AppState.getSchemaCache appState

      let observer = AppState.getObserver appState
          bearerAuth = ApiRequest.userBearerAuth req

      response <- do
        authResultE <- runExceptT $ withTiming appConf $
            liftIO (Auth.getAuthResult appState bearerAuth) >>= liftEither

        case authResultE of
          Left err -> do
            let resp = Error.errorResponseFor configClientErrorVerbosity err
            observer $ genResponseObs Nothing req resp
            pure resp

          Right (jwtTime, authResult@AuthResult{..}) -> do
            resp <- either (Error.errorResponseFor configClientErrorVerbosity) identity <$>
              runExceptT (postgrestResponse appState appConf maybeSchemaCache jwtTime authResult req)
            observer $ genResponseObs (Just authRole) req resp
            pure resp

      -- Launch the connWorker when the connection is down. The postgrest
      -- function can respond successfully (with a stale schema cache) before
      -- the connWorker is done. However, when there's an empty schema cache
      -- postgrest responds with the error `PGRST002`; this means that the schema
      -- cache is still loading, so we don't launch the connWorker here because
      -- it would duplicate the loading process, e.g. https://github.com/PostgREST/postgrest/issues/3704
      -- TODO: this process may be unnecessary when the Listener is enabled. Revisit once https://github.com/PostgREST/postgrest/issues/1766 is done
      when (isServiceUnavailable response && isJust maybeSchemaCache) connWorker
      delay <- AppState.getNextDelay appState
      respond $ addRetryHint delay response
  where
    -- TODO WaiHeader.contentLength does a lookup everytime, see: https://hackage.haskell.org/package/wai-extra-3.1.17/docs/src/Network.Wai.Header.html#contentLength
    -- It might be possible to gain some perf by returning the response length from `postgrestResponse`. We calculate the length manually on Response.hs.
    genResponseObs :: Maybe ByteString -> Wai.Request -> Wai.Response -> Observation
    genResponseObs user req resp =
      ResponseObs user req (Wai.responseStatus resp) (WaiHeader.contentLength $ Wai.responseHeaders resp)

postgrestResponse
  :: AppState.AppState
  -> AppConfig
  -> Maybe SchemaCache
  -> Maybe Double
  -> AuthResult
  -> Wai.Request
  -> ExceptT Error IO Wai.Response
postgrestResponse appState conf@AppConfig{..} maybeSchemaCache jwtTime authResult@AuthResult{..} req = do
  let observer = AppState.getObserver appState

  sCache <-
    case maybeSchemaCache of
      Just sCache ->
        return sCache
      Nothing -> do
        lift $ observer SchemaCacheEmptyObs
        throwError Error.NoSchemaCacheError

  let prefs = ApiRequest.userPreferences conf req (dbTimezones sCache)

  body <- lift $ Wai.strictRequestBody req

  (parseTime, apiReq@ApiRequest{..}) <- withTiming conf $ liftEither . mapLeft Error.ApiRequestErr $ ApiRequest.userApiRequest conf prefs req body
  (planTime, plan)                   <- withTiming conf $ liftEither $ Plan.actionPlan iAction conf apiReq sCache

  let mainQ = Query.mainQuery plan conf apiReq authResult configDbPreRequest
      tx = MainTx.mainTx mainQ conf authResult apiReq plan sCache
      obsQuery s = when configLogQuery $ observer $ QueryObs mainQ s

  (txTime, txResult) <- withTiming conf $ do
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

  (respTime, resp) <- withTiming conf $ do
    let response = Response.actionResponse txResult apiReq (T.decodeUtf8 prettyVersion, docsVersion) conf sCache
        status' = either Error.status Response.pgrstStatus response

    -- TODO: see above obsQuery, only this obsQuery should remain after refactoring (because the QueryObs depends on the status)
    lift $ obsQuery status'
    liftEither response

  return $ toWaiResponse (ServerTiming jwtTime parseTime planTime txTime respTime) resp

  where
    toWaiResponse :: ServerTiming -> Response.PgrstResponse -> Wai.Response
    toWaiResponse timing (Response.PgrstResponse st hdrs bod) =
      Wai.responseLBS st (hdrs ++ serverTimingHeaders timing ++ [varyHeader | not $ varyHeaderPresent hdrs]) bod

    serverTimingHeaders :: ServerTiming -> [HTTP.Header]
    serverTimingHeaders timing = [serverTimingHeader timing | configServerTimingEnabled]

    varyHeader :: HTTP.Header
    varyHeader = (HTTP.hVary, "Accept, Prefer, Range")

    varyHeaderPresent :: [HTTP.Header] -> Bool
    varyHeaderPresent = any (\(h, _v) -> h == HTTP.hVary)

withTiming :: AppConfig -> ExceptT e IO a -> ExceptT e IO (Maybe Double, a)
withTiming AppConfig{configServerTimingEnabled} f = if configServerTimingEnabled
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

type AppSockets = (NS.Socket, Maybe NS.Socket)

initSockets :: AppConfig -> IO AppSockets
initSockets AppConfig{..} = do
  let
    cfg'usp = configServerUnixSocket
    cfg'uspm = configServerUnixSocketMode
    cfg'host = configServerHost
    cfg'port = configServerPort
    cfg'adminHost = configAdminServerHost
    cfg'adminPort = configAdminServerPort

  sock <- case cfg'usp of
    -- I'm not using `streaming-commons`' bindPath function here because it's not defined for Windows,
    -- but we need to have runtime error if we try to use it in Windows, not compile time error
    Just path -> createAndBindDomainSocket path cfg'uspm
    Nothing -> do
      (_, sock) <-
        if cfg'port /= 0
          then do
            sock <- bindPortTCP cfg'port (fromString $ T.unpack cfg'host)
            pure (cfg'port, sock)
          else do
            -- explicitly bind to a random port, returning bound port number
            (num, sock) <- bindRandomPortTCP (fromString $ T.unpack cfg'host)
            pure (num, sock)
      pure sock

  adminSock <- case cfg'adminPort of
    Just adminPort -> do
      adminSock <- bindPortTCP adminPort (fromString $ T.unpack cfg'adminHost)
      pure $ Just adminSock
    Nothing -> pure Nothing

  pure (sock, adminSock)
