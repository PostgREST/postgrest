{-|
Module      : PostgREST.App
Description : PostgREST main application

This module is in charge of mapping HTTP requests to PostgreSQL queries.
Some of its functionality includes:

- Mapping HTTP request methods to proper SQL statements. For example, a GET request is translated to executing a SELECT query in a read-only TRANSACTION.
- Producing HTTP Headers according to RFCs.
- Content Negotiation
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE TypeApplications    #-}
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
import Data.IORef               (atomicWriteIORef, newIORef,
                                 readIORef)
import Data.String              (IsString (..), String)
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
import PostgREST.Response.Performance (Timing, serverTimingHeader)
import PostgREST.SchemaCache          (SchemaCache (..))
import PostgREST.TimeIt               (timeItT)
import PostgREST.Version              (docsVersion, prettyVersion)

import           Control.Monad.Identity
import           Control.Monad.Writer
import qualified Data.ByteString.Char8     as BS
import qualified Data.List                 as L
import           Data.Streaming.Network    (bindPortTCP)
import qualified Data.Text                 as T
import qualified Network.HTTP.Types        as HTTP
import           Network.HTTP.Types.Header (hVary)
import qualified Network.Socket            as NS
import           PostgREST.Unix            (createAndBindDomainSocket)
import           System.Posix.Types        (FileMode)

import Protolude hiding (Handler)

run :: AppState -> IO ()
run appState = do
  conf <- AppState.getConfig appState

  mainSocketRef <- newIORef Nothing
  adminSocket <- initAdminServerSocket conf

  let closeSockets = do
        whenJust adminSocket NS.close
        readIORef mainSocketRef >>= foldMap NS.close
  Unix.installSignalHandlers observer closeSockets (AppState.schemaCacheLoader appState) (AppState.readInDbConfig False appState)

  Admin.runAdmin appState adminSocket (readIORef mainSocketRef) (serverSettings conf)

  Listener.runListener appState

  -- Kick off and wait for the initial SchemaCache load before creating the
  -- main API socket.
  AppState.schemaCacheLoader appState
  AppState.waitForSchemaCacheInit appState

  mainSocket <- initServerSocket conf
  atomicWriteIORef mainSocketRef $ Just mainSocket

  let app = postgrest appState (AppState.schemaCacheLoader appState)

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
      let
        observer = AppState.getObserver appState
        handleError = fmap $ either errorToWaiResponse identity
        errorToWaiResponse = Error.errorResponseFor configClientErrorVerbosity

      -- WriterT (Last ByteString) to save authRole (uses `tell` for this)
      -- (Last ByteString) is a Monoid that maintains latest ByteString written using tell
      -- see https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Monoid.html#t:Last
      -- runWriterT has to be before runExceptT to make sure role is not lost on error
      (response, Last authRole) <- runWriterT . handleError . runExceptT $
        if configServerTimingEnabled then
          evalStateT (postgrestResponse appState appConf maybeSchemaCache req) (mempty @[Timing])
        else
          runIdentityT $ postgrestResponse appState appConf maybeSchemaCache req

      observer $ genResponseObs authRole req response

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

class MonadTiming m where
  withTiming' :: forall name a. KnownSymbol name => m a -> m a
  renderTimings :: m [HTTP.Header]

withTiming :: forall s m a. (KnownSymbol s, MonadTiming m) => m a -> m a
withTiming = withTiming' @m @s

-- Dummy MonadTiming instance.
-- Used when configServerTimingEnabled is False
-- GHC specialization will optimize away all calls to withTiming and to renderTimings,
-- hence hot path does not execute any unnecessary code and does not cause unnecessary allocations.
instance Monad m => MonadTiming (IdentityT m) where
  withTiming' = identity
  renderTimings = pure mempty

-- MonadTiming instance saving timings in StateT [Timing].
-- Used when configServerTimingEnabled is True
instance MonadIO m => MonadTiming (StateT [Timing] m) where
  withTiming' @name f = do
          (t, result) <- timeItT f
          modify ((BS.pack $ symbolVal (Proxy @name), t) :)
          pure result
  -- Force inlining so that (BS.pack $ symbolVal (Proxy @name)) can be constant-folded
  {-# INLINE withTiming' #-}
  renderTimings = (foldMap (pure . serverTimingHeader) . nonEmpty) . reverse <$> get

postgrestResponse
  :: (MonadError Error m, MonadIO m, MonadWriter (Last ByteString) m, MonadTiming m)
  => AppState.AppState
  -> AppConfig
  -> Maybe SchemaCache
  -> Wai.Request
  -> m Wai.Response
postgrestResponse appState conf@AppConfig{..} maybeSchemaCache req = do
  let observer = liftIO . AppState.getObserver appState

  authResult@AuthResult{..} <- withTiming @"jwt" $ do
    time <- liftIO $ AppState.getTime appState
    Auth.getAuthResult conf time (AppState.getJwtCacheState appState) $ ApiRequest.userBearerAuth req
  -- save authRole
  tell $ pure authRole

  sCache <- maybe (observer SchemaCacheEmptyObs *> throwError Error.NoSchemaCacheError) pure maybeSchemaCache

  let prefs = ApiRequest.userPreferences conf req (dbTimezones sCache)

  body <- liftIO $ Wai.strictRequestBody req

  apiReq@ApiRequest{..} <- withTiming @"parse" $ liftEither . mapLeft Error.ApiRequestErr $ ApiRequest.userApiRequest conf prefs req body
  plan <- withTiming @"plan" $ Plan.actionPlan iAction conf apiReq sCache

  toWaiResponse =<< case plan of
    Plan.NoDb infoPlan ->
      withTiming @"response" $ liftEither $ Response.noDbActionResponse infoPlan sCache
    Plan.Db dbPlan -> do
      let mainQ = Query.mainQuery dbPlan conf apiReq authResult configDbPreRequest
          MainTx.DbTx dbSession = MainTx.mainTx mainQ conf authResult apiReq dbPlan sCache
          obsQuery s = when configLogQuery $ observer $ QueryObs mainQ s

      txResult <- withTiming @"transaction" $ do
        dbRes <- liftIO $ AppState.usePool appState dbSession
        let eitherResp = join $ mapLeft (Error.PgErr . Error.PgError (Just authRole /= configDbAnonRole)) dbRes

        -- TODO: we use obsQuery twice, one here and one below because in case of an error with the usePool above, the request will finish here and return an error message.
        -- This is because of a combination of ExceptT + our Error module which has Wai.responseLBS.
        -- This needs refactoring so only the below obsQuery is used.
        whenLeft eitherResp $ obsQuery . Error.status
        liftEither eitherResp

      withTiming @"response" $ do
        let response = Response.actionResponse txResult apiReq (T.decodeUtf8 prettyVersion, docsVersion) conf sCache
            status' = either Error.status Response.pgrstStatus response

        -- TODO: see above obsQuery, only this obsQuery should remain after refactoring (because the QueryObs depends on the status)
        obsQuery status'
        liftEither response
  where
    toWaiResponse (Response.PgrstResponse st hdrs bod) = do
      timings <- renderTimings
      pure $ Wai.responseLBS st (hdrs ++ timings ++ [varyHeader | not $ varyHeaderPresent hdrs]) bod

    varyHeader :: HTTP.Header
    varyHeader = (hVary, "Accept, Prefer, Range")

    varyHeaderPresent :: [HTTP.Header] -> Bool
    varyHeaderPresent = any (\(h, _v) -> h == hVary)


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
