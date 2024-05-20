{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module PostgREST.AppState
  ( AppState (..)
  , destroy
  , getConfig
  , getSchemaCache
  , getPgVersion
  , getRetryNextIn
  , init
  , initSockets
  , initWithPool
  , putSchemaCache
  , putPgVersion
  , usePool
  , reReadConfig
  , runListener
  , isLoaded
  , isPending
  ) where

import qualified Data.ByteString.Char8      as BS
import qualified Data.Cache                 as C
import           Data.Either.Combinators    (whenLeft)
import qualified Data.Text                  as T (unpack)
import           Hasql.Connection           (acquire)
import qualified Hasql.Notifications        as SQL
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Pool.Config          as SQL
import qualified Hasql.Session              as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.HTTP.Types.Status  as HTTP
import qualified Network.Socket             as NS
import qualified PostgREST.Error            as Error
import qualified PostgREST.Logger           as Logger
import qualified PostgREST.Metrics          as Metrics
import           PostgREST.Observation
import           PostgREST.Version          (prettyVersion)
import           System.TimeIt              (timeItT)

import {-# SOURCE #-} qualified PostgREST.Auth as Auth (Result)

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Control.Debounce
import Control.Exception  (throw)
import Control.Retry      (RetryPolicy, RetryStatus (..), capDelay,
                           exponentialBackoff, recoverAll, retrying,
                           rsPreviousDelay)
import Data.IORef         (IORef, atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time.Clock    (UTCTime, getCurrentTime)

import PostgREST.Config                  (AppConfig (..),
                                          addFallbackAppName,
                                          addTargetSessionAttrs,
                                          readAppConfig)
import PostgREST.Config.Database         (queryDbSettings,
                                          queryPgVersion,
                                          queryRoleSettings)
import PostgREST.Config.PgVersion        (PgVersion (..),
                                          minimumPgVersion)
import PostgREST.SchemaCache             (SchemaCache (..),
                                          querySchemaCache,
                                          showSummary)
import PostgREST.SchemaCache.Identifiers (dumpQi)
import PostgREST.Unix                    (createAndBindDomainSocket)

import Data.Streaming.Network (bindPortTCP, bindRandomPortTCP)
import Data.String            (IsString (..))
import Protolude

data AppState = AppState
  -- | Database connection pool
  { pool                      :: SQL.Pool
  -- | Database server version, will be updated by the connectionWorker
  , pgVersion                 :: IORef PgVersion
  -- | No schema cache at the start. Will be filled in by the connectionWorker
  , schemaCache               :: IORef (Maybe SchemaCache)
  -- | The schema cache status
  , sCacheStatus              :: IORef SchemaCacheStatus
  -- | The connection status
  , connStatus                :: IORef ConnectionStatus
  -- | State of the LISTEN channel
  , isListenerOn              :: IORef Bool
  -- | starts the connection worker with a debounce
  , debouncedConnectionWorker :: IO ()
  -- | Config that can change at runtime
  , conf                      :: IORef AppConfig
  -- | Time used for verifying JWT expiration
  , getTime                   :: IO UTCTime
  -- | Used for killing the main thread in case a subthread fails
  , mainThreadId              :: ThreadId
  -- | Keeps track of when the next retry for connecting to database is scheduled
  , retryNextIn               :: IORef Int
  -- | JWT Cache
  , jwtCache                  :: C.Cache ByteString Auth.Result
  -- | Network socket for REST API
  , socketREST                :: NS.Socket
  -- | Network socket for the admin UI
  , socketAdmin               :: Maybe NS.Socket
  -- | Observation handler
  , observer                  :: ObservationHandler
  , logger                    :: Logger.LoggerState
  , metrics                   :: Metrics.MetricsState
  }

-- | Schema cache status
data SchemaCacheStatus
  = SCLoaded
  | SCPending
  deriving Eq

-- | Current database connection status
data ConnectionStatus
  = ConnEstablished
  | ConnPending
  deriving Eq

type AppSockets = (NS.Socket, Maybe NS.Socket)


init :: AppConfig -> IO AppState
init conf@AppConfig{configLogLevel, configDbPoolSize} = do
  loggerState  <- Logger.init
  metricsState <- Metrics.init configDbPoolSize
  let observer = liftA2 (>>) (Logger.observationLogger loggerState configLogLevel) (Metrics.observationMetrics metricsState)

  pool <- initPool conf observer
  (sock, adminSock) <- initSockets conf
  state' <- initWithPool (sock, adminSock) pool conf loggerState metricsState observer
  pure state' { socketREST = sock, socketAdmin = adminSock}

initWithPool :: AppSockets -> SQL.Pool -> AppConfig -> Logger.LoggerState -> Metrics.MetricsState -> ObservationHandler -> IO AppState
initWithPool (sock, adminSock) pool conf loggerState metricsState observer = do

  appState <- AppState pool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> newIORef SCPending
    <*> newIORef ConnPending
    <*> newIORef False
    <*> pure (pure ())
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> myThreadId
    <*> newIORef 0
    <*> C.newCache Nothing
    <*> pure sock
    <*> pure adminSock
    <*> pure observer
    <*> pure loggerState
    <*> pure metricsState

  debWorker <-
    let decisecond = 100000 in
    mkDebounce defaultDebounceSettings
       { debounceAction = internalConnectionWorker appState
       , debounceFreq = decisecond
       , debounceEdge = leadingEdge -- runs the worker at the start and the end
       }

  return appState { debouncedConnectionWorker = debWorker}

destroy :: AppState -> IO ()
destroy = destroyPool

initSockets :: AppConfig -> IO AppSockets
initSockets AppConfig{..} = do
  let
    cfg'usp = configServerUnixSocket
    cfg'uspm = configServerUnixSocketMode
    cfg'host = configServerHost
    cfg'port = configServerPort
    cfg'adminport = configAdminServerPort

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

  adminSock <- case cfg'adminport of
    Just adminPort -> do
      adminSock <- bindPortTCP adminPort (fromString $ T.unpack cfg'host)
      pure $ Just adminSock
    Nothing -> pure Nothing

  pure (sock, adminSock)

initPool :: AppConfig -> ObservationHandler -> IO SQL.Pool
initPool AppConfig{..} observer =
  SQL.acquire $ SQL.settings
    [ SQL.size configDbPoolSize
    , SQL.acquisitionTimeout $ fromIntegral configDbPoolAcquisitionTimeout
    , SQL.agingTimeout $ fromIntegral configDbPoolMaxLifetime
    , SQL.idlenessTimeout $ fromIntegral configDbPoolMaxIdletime
    , SQL.staticConnectionSettings (toUtf8 $ addFallbackAppName prettyVersion configDbUri)
    , SQL.observationHandler $ observer . HasqlPoolObs
    ]

-- | Run an action with a database connection.
usePool :: AppState -> SQL.Session a -> IO (Either SQL.UsageError a)
usePool appState@AppState{observer=observer, mainThreadId=mainThreadId} sess = do
  observer PoolRequest

  res <- SQL.use appState.pool sess

  observer PoolRequestFullfilled

  whenLeft res (\case
    SQL.AcquisitionTimeoutUsageError ->
      observer $ PoolAcqTimeoutObs SQL.AcquisitionTimeoutUsageError
    err@(SQL.ConnectionUsageError e) ->
      let failureMessage = BS.unpack $ fromMaybe mempty e in
      when (("FATAL:  password authentication failed" `isInfixOf` failureMessage) || ("no password supplied" `isInfixOf` failureMessage)) $ do
        observer $ ExitDBFatalError ServerAuthError err
        killThread mainThreadId
    err@(SQL.SessionUsageError (SQL.QueryError tpl _ (SQL.ResultError resultErr))) -> do
      case resultErr of
        SQL.UnexpectedResult{} -> do
          observer $ ExitDBFatalError ServerPgrstBug err
          killThread mainThreadId
        SQL.RowError{} -> do
          observer $ ExitDBFatalError ServerPgrstBug err
          killThread mainThreadId
        SQL.UnexpectedAmountOfRows{} -> do
          observer $ ExitDBFatalError ServerPgrstBug err
          killThread mainThreadId
        -- Check for a syntax error (42601 is the pg code) only for queries that don't have `WITH pgrst_source` as prefix.
        -- This would mean the error is on our schema cache queries, so we treat it as fatal.
        -- TODO have a better way to mark this as a schema cache query
        SQL.ServerError "42601" _ _ _ _ ->
          unless ("WITH pgrst_source" `BS.isPrefixOf` tpl) $ do
            observer $ ExitDBFatalError ServerPgrstBug err
            killThread mainThreadId
        -- Check for a "prepared statement <name> already exists" error (Code 42P05: duplicate_prepared_statement).
        -- This would mean that a connection pooler in transaction mode is being used
        -- while prepared statements are enabled in the PostgREST configuration,
        -- both of which are incompatible with each other.
        SQL.ServerError "42P05" _ _ _ _ -> do
          observer $ ExitDBFatalError ServerError42P05 err
          killThread mainThreadId
        -- Check for a "transaction blocks not allowed in statement pooling mode" error (Code 08P01: protocol_violation).
        -- This would mean that a connection pooler in statement mode is being used which is not supported in PostgREST.
        SQL.ServerError "08P01" "transaction blocks not allowed in statement pooling mode" _ _ _ -> do
          observer $ ExitDBFatalError ServerError08P01 err
          killThread mainThreadId
        SQL.ServerError{} ->
          when (Error.status (Error.PgError False err) >= HTTP.status500) $
            observer $ QueryErrorCodeHighObs err
    SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ClientError _)) ->
      pure ()
    )

  return res

-- | Flush the connection pool so that any future use of the pool will
-- use connections freshly established after this call.
flushPool :: AppState -> IO ()
flushPool appState = SQL.release appState.pool

-- | Destroy the pool on shutdown.
destroyPool :: AppState -> IO ()
destroyPool appState = SQL.release appState.pool

getPgVersion :: AppState -> IO PgVersion
getPgVersion appState = readIORef appState.pgVersion

putPgVersion :: AppState -> PgVersion -> IO ()
putPgVersion appState = atomicWriteIORef appState.pgVersion

getSchemaCache :: AppState -> IO (Maybe SchemaCache)
getSchemaCache appState = readIORef appState.schemaCache

putSchemaCache :: AppState -> Maybe SchemaCache -> IO ()
putSchemaCache appState = atomicWriteIORef appState.schemaCache

getRetryNextIn :: AppState -> IO Int
getRetryNextIn appState = readIORef appState.retryNextIn

putRetryNextIn :: AppState -> Int -> IO ()
putRetryNextIn appState = atomicWriteIORef appState.retryNextIn

getConfig :: AppState -> IO AppConfig
getConfig appState = readIORef appState.conf

putConfig :: AppState -> AppConfig -> IO ()
putConfig appState = atomicWriteIORef appState.conf

instance HasField "getConnStatus" AppState (IO ConnectionStatus) where
  getField appState = readIORef appState.connStatus

instance HasField "putConnStatus" AppState (ConnectionStatus -> IO ()) where
  getField appState = atomicWriteIORef appState.connStatus

getIsListenerOn :: AppState -> IO Bool
getIsListenerOn appState = do
  AppConfig{..} <- getConfig appState
  if configDbChannelEnabled then
    readIORef appState.isListenerOn
  else
    pure True

putIsListenerOn :: AppState -> Bool -> IO ()
putIsListenerOn appState = atomicWriteIORef appState.isListenerOn

isConnEstablished :: AppState -> IO Bool
isConnEstablished appState = do
  conf <- getConfig appState
  if configDbChannelEnabled conf
    then do -- if the listener is enabled, we can be sure the connection status is always up to date
      st <- appState.getConnStatus
      return $ st == ConnEstablished
    else    -- otherwise the only way to check the connection is to make a query
      isRight <$> usePool appState (SQL.sql "SELECT 1")

isLoaded :: AppState -> IO Bool
isLoaded appState = do
  scacheStatus <- readIORef appState.sCacheStatus
  connEstablished <- isConnEstablished appState
  listenerOn <- getIsListenerOn appState
  return $ scacheStatus == SCLoaded && connEstablished && listenerOn

isPending :: AppState -> IO Bool
isPending appState = do
  scacheStatus <- readIORef appState.sCacheStatus
  connStatus <- appState.getConnStatus
  listenerOn <- getIsListenerOn appState
  return $ scacheStatus == SCPending || connStatus == ConnPending || not listenerOn

putSCacheStatus :: AppState -> SchemaCacheStatus -> IO ()
putSCacheStatus appState = atomicWriteIORef appState.sCacheStatus

-- | Load the SchemaCache by using a connection from the pool.
loadSchemaCache :: AppState -> IO SchemaCacheStatus
loadSchemaCache appState@AppState{observer=observer} = do
  conf@AppConfig{..} <- getConfig appState
  (resultTime, result) <-
    let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
    timeItT $ usePool appState (transaction SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
  case result of
    Left e -> do
      putSCacheStatus appState SCPending
      putSchemaCache appState Nothing
      observer $ SchemaCacheErrorObs e
      return SCPending

    Right sCache -> do
      -- IMPORTANT: While the pending schema cache state starts from running the above querySchemaCache, only at this stage we block API requests due to the usage of an
      -- IORef on putSchemaCache. This is why SCacheStatus is put at SCPending here to signal the Admin server (using isPending) that we're on a recovery state.
      putSCacheStatus appState SCPending
      putSchemaCache appState $ Just sCache
      observer $ SchemaCacheQueriedObs resultTime
      (t, _) <- timeItT $ observer $ SchemaCacheSummaryObs $ showSummary sCache
      observer $ SchemaCacheLoadedObs t
      putSCacheStatus appState SCLoaded
      return SCLoaded

-- | The purpose of this worker is to obtain a healthy connection to pg and an
-- up-to-date schema cache(SchemaCache).  This method is meant to be called
-- multiple times by the same thread, but does nothing if the previous
-- invocation has not terminated. In all cases this method does not halt the
-- calling thread, the work is performed in a separate thread.
--
-- Background thread that does the following :
--  1. Tries to connect to pg server and will keep trying until success.
--  2. Checks if the pg version is supported and if it's not it kills the main
--     program.
--  3. Obtains the sCache. If this fails, it goes back to 1.
internalConnectionWorker :: AppState -> IO ()
internalConnectionWorker appState@AppState{observer=observer, mainThreadId=mainThreadId} = work
  where
    work = do
      AppConfig{..} <- getConfig appState
      observer DBConnectAttemptObs
      connStatus <- establishConnection appState
      case connStatus of
        ConnPending ->
          unless configDbPoolAutomaticRecovery $ do
            observer ExitDBNoRecoveryObs
            killThread mainThreadId
        ConnEstablished -> do
          actualPgVersion <- getPgVersion appState
          when (actualPgVersion < minimumPgVersion) $ do
            observer $ ExitUnsupportedPgVersion actualPgVersion minimumPgVersion
            killThread mainThreadId
          observer (DBConnectedObs $ pgvFullName actualPgVersion)
          -- this could be fail because the connection drops, but the loadSchemaCache will pick the error and retry again
          -- We cannot retry after it fails immediately, because db-pre-config could have user errors. We just log the error and continue.
          when configDbConfig $ reReadConfig False appState
          scStatus <- loadSchemaCache appState
          case scStatus of
            SCLoaded ->
              -- do nothing and proceed if the load was successful
              return ()
            SCPending ->
              -- retry reloading the schema cache
              work

-- | One second in microseconds
oneSecondInUs :: Int
oneSecondInUs = 1000000

retryPolicy :: RetryPolicy
retryPolicy = capDelay delayMicroseconds $ exponentialBackoff oneSecondInUs
  where
    delayMicroseconds = 32000000 -- 32 seconds

-- | Repeatedly flush the pool, and check if a connection from the
-- pool allows access to the PostgreSQL database.
--
-- Releasing the pool is key for rapid recovery. Otherwise, the pool
-- timeout would have to be reached for new healthy connections to be acquired.
-- Which might not happen if the server is busy with requests. No idle
-- connection, no pool timeout.
--
-- The connection tries are capped, but if the connection times out no error is
-- thrown, just 'False' is returned.
establishConnection :: AppState -> IO ConnectionStatus
establishConnection appState@AppState{observer=observer} =
  retrying retryPolicy shouldRetry $
    const $ flushPool appState >> getConnectionStatus
  where
    getConnectionStatus :: IO ConnectionStatus
    getConnectionStatus = do
      pgVersion <- usePool appState (queryPgVersion False) -- No need to prepare the query here, as the connection might not be established
      case pgVersion of
        Left e -> do
          observer $ ConnectionPgVersionErrorObs e
          appState.putConnStatus ConnPending
          return ConnPending
        Right version -> do
          appState.putConnStatus ConnEstablished
          putPgVersion appState version
          return ConnEstablished

    shouldRetry :: RetryStatus -> ConnectionStatus -> IO Bool
    shouldRetry rs isConnSucc = do
      AppConfig{..} <- getConfig appState
      let
        delay = fromMaybe 0 (rsPreviousDelay rs) `div` oneSecondInUs
        itShould = ConnPending == isConnSucc && configDbPoolAutomaticRecovery
      when itShould $ observer $ ConnectionRetryObs delay
      when itShould $ putRetryNextIn appState delay
      return itShould

-- | Re-reads the config plus config options from the db
reReadConfig :: Bool -> AppState -> IO ()
reReadConfig startingUp appState@AppState{observer=observer} = do
  AppConfig{..} <- getConfig appState
  pgVer <- getPgVersion appState
  dbSettings <-
    if configDbConfig then do
      qDbSettings <- usePool appState (queryDbSettings (dumpQi <$> configDbPreConfig) configDbPreparedStatements)
      case qDbSettings of
        Left e -> do
          observer $ ConfigReadErrorObs e
          pure mempty
        Right x -> pure x
    else
      pure mempty
  (roleSettings, roleIsolationLvl) <-
    if configDbConfig then do
      rSettings <- usePool appState (queryRoleSettings pgVer configDbPreparedStatements)
      case rSettings of
        Left e -> do
          observer $ QueryRoleSettingsErrorObs e
          pure (mempty, mempty)
        Right x -> pure x
    else
      pure mempty
  readAppConfig dbSettings configFilePath (Just configDbUri) roleSettings roleIsolationLvl >>= \case
    Left err   ->
      if startingUp then
        panic err -- die on invalid config if the program is starting up
      else
        observer $ ConfigInvalidObs err
    Right newConf -> do
      putConfig appState newConf
      if startingUp then
        pass
      else
        observer ConfigSucceededObs

-- | Starts the Listener in a thread
runListener :: AppState -> IO ()
runListener appState = do
  AppConfig{..} <- getConfig appState
  when configDbChannelEnabled $
    void . forkIO $ retryingListen appState

-- | Starts a LISTEN connection and handles notifications. It recovers with exponential backoff if the LISTEN connection is lost.
-- TODO Once the listen channel is recovered, the retry status is not reset. So if the last backoff was 4 seconds, the next time recovery kicks in the backoff will be 8 seconds.
-- This is because `Hasql.Notifications.waitForNotifications` uses a forever loop that only finishes when it throws an exception.
retryingListen :: AppState -> IO ()
retryingListen appState@AppState{observer=observer, mainThreadId=mainThreadId} = do
  AppConfig{..} <- getConfig appState
  let
    dbChannel = toS configDbChannel
    -- Try, catch and rethrow the exception. This is done so we can observe the failure message and let Control.Retry.recoverAll do its work.
    -- There's a `Control.Retry.recovering` we could use to avoid this rethrowing, but it's more complex to use.
    -- The root cause of these workarounds is that `Hasql.Notifications.waitForNotifications` uses exceptions.
    tryRethrow :: IO () -> IO ()
    tryRethrow action =  do
      act <- try action
      whenLeft act (\ex -> do
        putIsListenerOn appState False
        observer $ DBListenFail dbChannel (Right $ Left ex)
        unless configDbPoolAutomaticRecovery $ do
          killThread mainThreadId
        throw ex)

  recoverAll retryPolicy (\RetryStatus{rsIterNumber, rsPreviousDelay} -> do

    when (rsIterNumber > 0) $
      let delay = fromMaybe 0 rsPreviousDelay `div` oneSecondInUs in
      observer $ DBListenRetry delay

    connection <- acquire $ toUtf8 (addTargetSessionAttrs $ addFallbackAppName prettyVersion configDbUri)
    case connection of
      Right conn -> do

        tryRethrow $ SQL.listen conn $ SQL.toPgIdentifier dbChannel

        putIsListenerOn appState True
        observer $ DBListenStart dbChannel

        when (rsIterNumber > 0) $ do
          -- once we can LISTEN again, we might have lost schema cache notificacions, so reload
          appState.debouncedConnectionWorker

        tryRethrow $ SQL.waitForNotifications handleNotification conn

      Left err -> do
        observer $ DBListenFail dbChannel (Left err)
        -- throw an exception so recoverAll works
        exitFailure
    )

  where
    handleNotification channel msg =
      if | BS.null msg            -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload schema" -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload config" -> observer (DBListenerGotConfigMsg channel) >> reReadConfig False appState
         | otherwise              -> pure () -- Do nothing if anything else than an empty message is sent

    cacheReloader =
      -- reloads the schema cache + restarts pool connections
      -- it's necessary to restart the pg connections because they cache the pg catalog(see #2620)
      appState.debouncedConnectionWorker
