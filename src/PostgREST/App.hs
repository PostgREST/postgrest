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
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module PostgREST.App
  ( postgrest
  , run
  ) where

import GHC.Conc         (ThreadStatus (..), threadStatus)
import GHC.IO.Exception (IOErrorType (..))
import GHC.Weak
import System.IO.Error  (ioeGetErrorType)

import Control.Monad.Except     (liftEither)
import Data.Either.Combinators  (mapLeft, whenLeft)
import Data.IORef               (atomicWriteIORef, newIORef,
                                 readIORef)
import Data.String              (IsString (..), String)
import Network.Wai.Handler.Warp (defaultSettings, setBeforeMainLoop,
                                 setHost, setOnException, setPort,
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

import           Control.Monad.Writer
import qualified Data.ByteString.Char8     as BS
import qualified Data.List                 as L
import           Data.Streaming.Network    (bindPortTCP)
import qualified Data.Text                 as T
import qualified Network.HTTP.Types        as HTTP
import           Network.HTTP.Types.Header (hVary, hWarning)
import qualified Network.Socket            as NS
import           PostgREST.Unix            (createAndBindDomainSocket)
import           System.Posix.Types        (FileMode)

import Protolude        hiding (Handler)
import System.Directory (doesPathExist)

run :: AppState -> Weak ThreadId -> IO ()
run appState mainThreadIdRef = do
  conf <- AppState.getConfig appState

  mainSocketRef <- newIORef Nothing
  let setMainSocketRef = atomicWriteIORef mainSocketRef . Just
      clearMainSocketRef = atomicWriteIORef mainSocketRef Nothing

  bracket (initAdminServerSocket conf) ensureSocketClosed $ \adminSocket -> do

    let closeSockets = do
          ensureSocketClosed adminSocket
          ensureSocketClosed =<< readIORef mainSocketRef
    Unix.installSignalHandlers observer closeSockets (AppState.schemaCacheLoader appState) (AppState.readInDbConfig False appState)

    Admin.runAdmin appState adminSocket (checkMainAppLive (readIORef mainSocketRef) mainThreadIdRef) (serverSettings conf)

    Listener.runListener appState

    -- Kick off and wait for the initial SchemaCache load before creating the
    -- main API socket.
    AppState.schemaCacheLoader appState
    AppState.waitForSchemaCacheInit appState

    bracket (initServerSocket conf) NS.close $ \mainSocket -> do

      let app = postgrest appState (AppState.schemaCacheLoader appState)

      address <- resolveSocketToAddress mainSocket

      let
        appServerSettings = serverSettings conf
          & setPort (configServerPort conf)
          & setOnException onWarpException
          & setBeforeMainLoop (setMainSocketRef mainSocket *> observer (AppServerAddressObs address))

      Warp.runSettingsSocket appServerSettings mainSocket app
        `finally` clearMainSocketRef
  where
    observer = AppState.getObserver appState

    ensureSocketClosed = foldMap NS.close

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
    & setServerName ("postgrest/" <> prettyVersion)

-- | PostgREST application
postgrest :: AppState.AppState -> IO () -> Wai.Application
postgrest appState connWorker =
  traceHeaderMiddleware appState .
  Cors.middleware appState $
    \req respond -> do
      appConf@AppConfig{..} <- AppState.getConfig appState -- the config must be read again because it can reload
      maybeSchemaCache <- AppState.getSchemaCache appState

      let handleError = fmap (either (Error.errorResponseFor configClientErrorVerbosity) identity)

      -- writer to save authRole (uses `tell` for this and `getLast` to obtain it)
      -- has to be before runExceptT to make sure role is not lost on error
      (response, authRole) <- runWriterT . handleError . runExceptT $ do
        (jwtTime, authResult@AuthResult{..}) <- withTiming appConf $
          Auth.getAuthResult appState $ ApiRequest.userBearerAuth req

        tell $ pure authRole

        postgrestResponse appState appConf maybeSchemaCache jwtTime authResult req

      AppState.getObserver appState $ genResponseObs (getLast authRole) req response

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
  :: (MonadError Error m, MonadIO m)
  => AppState.AppState
  -> AppConfig
  -> Maybe SchemaCache
  -> Maybe Double
  -> AuthResult
  -> Wai.Request
  -> m Wai.Response
postgrestResponse appState conf@AppConfig{..} maybeSchemaCache jwtTime authResult@AuthResult{..} req = do
  let observer = AppState.getObserver appState

  sCache <-
    case maybeSchemaCache of
      Just sCache ->
        return sCache
      Nothing -> do
        liftIO $ observer SchemaCacheEmptyObs
        throwError Error.NoSchemaCacheError

  let prefs = ApiRequest.userPreferences conf req (dbTimezones sCache)

  body <- liftIO $ Wai.strictRequestBody req

  (parseTime, apiReq@ApiRequest{..}) <- withTiming conf $ liftEither . mapLeft Error.ApiRequestErr $ ApiRequest.userApiRequest conf prefs req body
  (planTime, plan)                   <- withTiming conf $ liftEither $ Plan.actionPlan iAction conf apiReq sCache

  let warnings = Plan.legacyWarnings plan
      legacyWarnMsg = "Embedded resource was referenced by relation name even though it has an alias. This is deprecated and will stop working in a future release."
      legacyWarnHint = let replacement (relName, alias) = "`" <> relName <> "` to `" <> alias <> "`" in T.intercalate ", " (replacement <$> warnings)
      shouldShowWarnings = configUrlUseLegacyTargetNames && not (null warnings)

  liftIO $ when shouldShowWarnings $
    observer $ LegacyTargetNameWarningObs (legacyWarnMsg, legacyWarnHint) iMethod (iPath <> Wai.rawQueryString req) -- TODO maybe store rawQueryString in ApiRequest for consistency

  let mainQ = Query.mainQuery plan conf apiReq authResult configDbPreRequest
      tx = MainTx.mainTx mainQ conf authResult apiReq plan sCache
      obsQuery s = when configLogQuery $ observer $ QueryObs mainQ s

  (txTime, txResult) <- withTiming conf $ do
    case tx of
      MainTx.NoDbTx r -> pure r
      MainTx.DbTx dbSession -> do
        dbRes <- liftIO $ AppState.usePool appState dbSession
        let eitherResp = join $ mapLeft (Error.PgErr . Error.PgError (Just authRole /= configDbAnonRole)) dbRes

        -- TODO: we use obsQuery twice, one here and one below because in case of an error with the usePool above, the request will finish here and return an error message.
        -- This is because of a combination of ExceptT + our Error module which has Wai.responseLBS.
        -- This needs refactoring so only the below obsQuery is used.
        liftIO $ whenLeft eitherResp $ obsQuery . Error.status
        liftEither eitherResp

  (respTime, resp) <- withTiming conf $ do
    let response = Response.actionResponse txResult apiReq (T.decodeUtf8 prettyVersion, docsVersion) conf sCache
        status' = either Error.status Response.pgrstStatus response

    -- TODO: see above obsQuery, only this obsQuery should remain after refactoring (because the QueryObs depends on the status)
    liftIO $ obsQuery status'
    liftEither response

  let warnHdrMsgs = if shouldShowWarnings then Just (legacyWarnMsg, legacyWarnHint) else Nothing

  return $ toWaiResponse (ServerTiming jwtTime parseTime planTime txTime respTime) warnHdrMsgs resp

  where
    toWaiResponse :: ServerTiming -> Maybe (Text, Text) -> Response.PgrstResponse -> Wai.Response
    toWaiResponse timing warnMsgs (Response.PgrstResponse st hdrs bod) =
      Wai.responseLBS st (hdrs ++ serverTimingHeaders timing ++ warningHeaders warnMsgs ++ [varyHeader | not $ varyHeaderPresent hdrs]) bod

    serverTimingHeaders :: ServerTiming -> [HTTP.Header]
    serverTimingHeaders timing = [serverTimingHeader timing | configServerTimingEnabled]

    varyHeader :: HTTP.Header
    varyHeader = (hVary, "Accept, Prefer, Range")

    varyHeaderPresent :: [HTTP.Header] -> Bool
    varyHeaderPresent = any (\(h, _v) -> h == hVary)

    warningHeaders :: Maybe (Text, Text) -> [HTTP.Header]
    warningHeaders Nothing = []
    warningHeaders (Just (msg, hint)) =
      let warnMsg = msg <> " Update " <> hint <> " in query string filters, orders or limits."
          pgrstVer = "PostgRESTv" <> BS.filter (/= ' ') prettyVersion
      in
      [(hWarning, "299 " <> pgrstVer <> " \"" <> encodeUtf8 warnMsg <> "\"")]

withTiming :: (MonadError e m, MonadIO m) => AppConfig -> m a -> m (Maybe Double, a)
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

initSocket :: (Applicative f, Traversable f) => Maybe String -> FileMode -> Text -> f Int -> IO (f NS.Socket)
initSocket unixSocket unixSocketMode tcpHost tcpPort =
  maybe initTCPSocket initDomainSocket unixSocket
  where
    initTCPSocket = traverse (`bindPortTCP` (fromString $ T.unpack tcpHost)) tcpPort
    -- I'm not using `streaming-commons`' bindPath function here because it's not defined for Windows,
    -- but we need to have runtime error if we try to use it in Windows, not compile time error
    initDomainSocket = fmap pure . (`createAndBindDomainSocket` unixSocketMode)

initServerSocket :: AppConfig -> IO NS.Socket
initServerSocket AppConfig{..} =
  runIdentity <$> initSocket
    configServerUnixSocket configServerUnixSocketMode
    configServerHost (pure configServerPort)

initAdminServerSocket :: AppConfig -> IO (Maybe NS.Socket)
initAdminServerSocket AppConfig{..} =
  initSocket
    configAdminServerUnixSocket configAdminServerUnixSocketMode
    configAdminServerHost configAdminServerPort

checkMainAppLive :: IO (Maybe NS.Socket) -> Weak ThreadId -> IO Bool
checkMainAppLive getMainSocket mainThreadIdRef =
  handle (\(_ :: IOException) -> pure False) $
    checkMainThread <&&> checkSocket
  where
    checkSocket = getMainSocket >>=
      maybe (pure False)
      (NS.getSocketName >=> \case
        -- in case of unix socket, check if it still exists
        NS.SockAddrUnix fp -> doesPathExist fp
        _ -> pure True)
    checkMainThread = deRefWeak mainThreadIdRef >>=
      maybe (pure False)
      (fmap isRunning . threadStatus)
    isRunning = \case
      ThreadRunning -> True
      ThreadBlocked _ -> True
      _ -> False
