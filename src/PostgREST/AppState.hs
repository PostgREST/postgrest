{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.AppState
  ( AppState
  , AuthResult(..)
  , destroy
  , getConfig
  , getSchemaCache
  , getMainThreadId
  , getPgVersion
  , getRetryNextIn
  , getTime
  , getJwtCache
  , getSocketREST
  , getSocketAdmin
  , init
  , initSockets
  , initWithPool
  , putSchemaCache
  , putPgVersion
  , usePool
  , reReadConfig
  , connectionWorker
  , runListener
  , getObserver
  , isLoaded
  , isPending
  ) where

import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.KeyMap          as KM
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

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Control.Debounce
import Control.Retry      (RetryStatus, capDelay, exponentialBackoff,
                           retrying, rsPreviousDelay)
import Data.IORef         (IORef, atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time.Clock    (UTCTime, getCurrentTime)

import PostgREST.Config                  (AppConfig (..),
                                          addFallbackAppName,
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

import Data.IP                (fromHostAddress, fromHostAddress6)
import Data.Streaming.Network (HostPreference, bindPortTCP,
                               bindRandomPortTCP)
import Data.String            (IsString (..))
import Protolude

data AuthResult = AuthResult
  { authClaims :: KM.KeyMap JSON.Value
  , authRole   :: BS.ByteString
  }

data AppState = AppState
  -- | Database connection pool
  { statePool                 :: SQL.Pool
  -- | Database server version, will be updated by the connectionWorker
  , statePgVersion            :: IORef PgVersion
  -- | No schema cache at the start. Will be filled in by the connectionWorker
  , stateSchemaCache          :: IORef (Maybe SchemaCache)
  -- | The schema cache status
  , stateSCacheStatus         :: IORef SchemaCacheStatus
  -- | The connection status
  , stateConnStatus           :: IORef ConnectionStatus
  -- | starts the connection worker with a debounce
  , debouncedConnectionWorker :: IO ()
  -- | Binary semaphore used to sync the listener(NOTIFY reload) with the connectionWorker.
  , stateListener             :: MVar ()
  -- | Config that can change at runtime
  , stateConf                 :: IORef AppConfig
  -- | Time used for verifying JWT expiration
  , stateGetTime              :: IO UTCTime
  -- | Used for killing the main thread in case a subthread fails
  , stateMainThreadId         :: ThreadId
  -- | Keeps track of when the next retry for connecting to database is scheduled
  , stateRetryNextIn          :: IORef Int
  -- | JWT Cache
  , jwtCache                  :: C.Cache ByteString AuthResult
  -- | Network socket for REST API
  , stateSocketREST           :: NS.Socket
  -- | Network socket for the admin UI
  , stateSocketAdmin          :: Maybe NS.Socket
  -- | Observation handler
  , stateObserver             :: ObservationHandler
  , stateLogger               :: Logger.LoggerState
  , stateMetrics              :: Metrics.MetricsState
  }

-- | Schema cache status
data SchemaCacheStatus
  = SCLoaded
  | SCPending
  | SCFatalFail
  deriving Eq

-- | Current database connection status
data ConnectionStatus
  = ConnEstablished
  | ConnPending
  | ConnFatalFail Text
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
  pure state' { stateSocketREST = sock, stateSocketAdmin = adminSock}

initWithPool :: AppSockets -> SQL.Pool -> AppConfig -> Logger.LoggerState -> Metrics.MetricsState -> ObservationHandler -> IO AppState
initWithPool (sock, adminSock) pool conf loggerState metricsState observer = do

  appState <- AppState pool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> newIORef SCPending
    <*> newIORef ConnPending
    <*> pure (pure ())
    <*> newEmptyMVar
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
      hp <- resolvedAddress sock -- reuse the same host as the main socket
      adminSock <- bindPortTCP adminPort $ fromMaybe (fromString $ T.unpack cfg'host) $ hp
      pure $ Just adminSock
    Nothing -> pure Nothing

  pure (sock, adminSock)
  where
    resolvedAddress :: NS.Socket -> IO (Maybe HostPreference)
    resolvedAddress sock = do
      sn <- NS.getSocketName sock
      case sn of
        NS.SockAddrInet _ hostAddr ->  pure $ Just $ fromString $ show $ fromHostAddress hostAddr
        NS.SockAddrInet6 _ _ hostAddr6 _ -> pure $ Just $ fromString $ show $ fromHostAddress6 hostAddr6
        _ -> pure Nothing


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
usePool AppState{stateObserver=observer,..} sess = do
  res <- SQL.use statePool sess

  whenLeft res (\case
    SQL.AcquisitionTimeoutUsageError -> observer $ PoolAcqTimeoutObs SQL.AcquisitionTimeoutUsageError
    error
      -- TODO We're using the 500 HTTP status for getting all internal db errors but there's no response here. We need a new intermediate type to not rely on the HTTP status.
      | Error.status (Error.PgError False error) >= HTTP.status500 -> observer $ QueryErrorCodeHighObs error
      | otherwise -> pure ())

  return res

-- | Flush the connection pool so that any future use of the pool will
-- use connections freshly established after this call.
flushPool :: AppState -> IO ()
flushPool AppState{..} = SQL.release statePool

-- | Destroy the pool on shutdown.
destroyPool :: AppState -> IO ()
destroyPool AppState{..} = SQL.release statePool

getPgVersion :: AppState -> IO PgVersion
getPgVersion = readIORef . statePgVersion

putPgVersion :: AppState -> PgVersion -> IO ()
putPgVersion = atomicWriteIORef . statePgVersion

getSchemaCache :: AppState -> IO (Maybe SchemaCache)
getSchemaCache = readIORef . stateSchemaCache

putSchemaCache :: AppState -> Maybe SchemaCache -> IO ()
putSchemaCache appState = atomicWriteIORef (stateSchemaCache appState)

connectionWorker :: AppState -> IO ()
connectionWorker = debouncedConnectionWorker

getRetryNextIn :: AppState -> IO Int
getRetryNextIn = readIORef . stateRetryNextIn

putRetryNextIn :: AppState -> Int -> IO ()
putRetryNextIn = atomicWriteIORef . stateRetryNextIn

getConfig :: AppState -> IO AppConfig
getConfig = readIORef . stateConf

putConfig :: AppState -> AppConfig -> IO ()
putConfig = atomicWriteIORef . stateConf

getTime :: AppState -> IO UTCTime
getTime = stateGetTime

getJwtCache :: AppState -> C.Cache ByteString AuthResult
getJwtCache = jwtCache

getSocketREST :: AppState -> NS.Socket
getSocketREST = stateSocketREST

getSocketAdmin :: AppState -> Maybe NS.Socket
getSocketAdmin = stateSocketAdmin

getMainThreadId :: AppState -> ThreadId
getMainThreadId = stateMainThreadId

-- | As this IO action uses `takeMVar` internally, it will only return once
-- `stateListener` has been set using `signalListener`. This is currently used
-- to syncronize workers.
waitListener :: AppState -> IO ()
waitListener = takeMVar . stateListener

-- tryPutMVar doesn't lock the thread. It should always succeed since
-- the connectionWorker is the only mvar producer.
signalListener :: AppState -> IO ()
signalListener appState = void $ tryPutMVar (stateListener appState) ()

isConnEstablished :: AppState -> IO Bool
isConnEstablished x = do
  conf <- getConfig x
  if configDbChannelEnabled conf
    then do -- if the listener is enabled, we can be sure the connection status is always up to date
      st <- readIORef $ stateConnStatus x
      return $ st == ConnEstablished
    else    -- otherwise the only way to check the connection is to make a query
      isRight <$> usePool x (SQL.sql "SELECT 1")

isLoaded :: AppState -> IO Bool
isLoaded x = do
  scacheStatus <- readIORef $ stateSCacheStatus x
  connEstablished <- isConnEstablished x
  return $ scacheStatus == SCLoaded && connEstablished

isPending :: AppState -> IO Bool
isPending x = do
  scacheStatus <- readIORef $ stateSCacheStatus x
  connStatus <- readIORef $ stateConnStatus x
  return $ scacheStatus == SCPending || connStatus == ConnPending

putSCacheStatus :: AppState -> SchemaCacheStatus -> IO ()
putSCacheStatus = atomicWriteIORef . stateSCacheStatus

putConnStatus :: AppState -> ConnectionStatus -> IO ()
putConnStatus = atomicWriteIORef . stateConnStatus

getObserver :: AppState -> ObservationHandler
getObserver = stateObserver

-- | Load the SchemaCache by using a connection from the pool.
loadSchemaCache :: AppState -> IO SchemaCacheStatus
loadSchemaCache appState@AppState{stateObserver=observer} = do
  conf@AppConfig{..} <- getConfig appState
  (resultTime, result) <-
    let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
    timeItT $ usePool appState (transaction SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
  case result of
    Left e -> do
      case checkIsFatal e of
        Just hint -> do
          observer $ SchemaCacheFatalErrorObs e hint
          return SCFatalFail
        Nothing -> do
          putSCacheStatus appState SCPending
          putSchemaCache appState Nothing
          observer $ SchemaCacheNormalErrorObs e
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
internalConnectionWorker appState@AppState{stateObserver=observer} = work
  where
    work = do
      AppConfig{..} <- getConfig appState
      observer DBConnectAttemptObs
      connStatus <- establishConnection appState
      case connStatus of
        ConnFatalFail reason ->
          -- Fatal error when connecting
          observer (ExitFatalObs reason) >> killThread (getMainThreadId appState)
        ConnPending ->
          unless configDbPoolAutomaticRecovery
              $ observer ExitDBNoRecoveryObs >> killThread (getMainThreadId appState)
        ConnEstablished -> do
          -- Procede with initialization
          when configDbChannelEnabled $
            signalListener appState
          actualPgVersion <- getPgVersion appState
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
            SCFatalFail ->
              -- die if our schema cache query has an error
              killThread $ getMainThreadId appState

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
establishConnection appState@AppState{stateObserver=observer} =
  retrying retrySettings shouldRetry $
    const $ flushPool appState >> getConnectionStatus
  where
    retrySettings = capDelay delayMicroseconds $ exponentialBackoff backoffMicroseconds
    delayMicroseconds = 32000000 -- 32 seconds
    backoffMicroseconds = 1000000 -- 1 second

    getConnectionStatus :: IO ConnectionStatus
    getConnectionStatus = do
      pgVersion <- usePool appState (queryPgVersion False) -- No need to prepare the query here, as the connection might not be established
      case pgVersion of
        Left e -> do
          observer $ ConnectionPgVersionErrorObs e
          case checkIsFatal e of
            Just reason ->
              return $ ConnFatalFail reason
            Nothing -> do
              putConnStatus appState ConnPending
              return ConnPending
        Right version ->
          if version < minimumPgVersion then
            return . ConnFatalFail $
              "Cannot run in this PostgreSQL version, PostgREST needs at least "
              <> pgvName minimumPgVersion
          else do
            putConnStatus appState ConnEstablished
            putPgVersion appState version
            return ConnEstablished

    shouldRetry :: RetryStatus -> ConnectionStatus -> IO Bool
    shouldRetry rs isConnSucc = do
      AppConfig{..} <- getConfig appState
      let
        delay = fromMaybe 0 (rsPreviousDelay rs) `div` backoffMicroseconds
        itShould = ConnPending == isConnSucc && configDbPoolAutomaticRecovery
      when itShould $ observer $ ConnectionRetryObs delay
      when itShould $ putRetryNextIn appState delay
      return itShould

-- | Re-reads the config plus config options from the db
reReadConfig :: Bool -> AppState -> IO ()
reReadConfig startingUp appState@AppState{stateObserver=observer} = do
  AppConfig{..} <- getConfig appState
  pgVer <- getPgVersion appState
  dbSettings <-
    if configDbConfig then do
      qDbSettings <- usePool appState (queryDbSettings (dumpQi <$> configDbPreConfig) configDbPreparedStatements)
      case qDbSettings of
        Left e -> do
          observer ConfigReadErrorObs
          case checkIsFatal e of
            Just hint -> do
              observer $ ConfigReadErrorFatalObs e hint
              killThread (getMainThreadId appState)
            Nothing -> do
              observer $ ConfigReadErrorNotFatalObs e
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

runListener :: AppConfig -> AppState -> IO ()
runListener conf@AppConfig{configDbChannelEnabled} appState = do
  when configDbChannelEnabled $ listener appState conf

-- | Starts a dedicated pg connection to LISTEN for notifications.  When a
-- NOTIFY <db-channel> - with an empty payload - is done, it refills the schema
-- cache.  It uses the connectionWorker in case the LISTEN connection dies.
listener :: AppState -> AppConfig -> IO ()
listener appState@AppState{stateObserver=observer} conf@AppConfig{..} = do
  let dbChannel = toS configDbChannel

  -- The listener has to wait for a signal from the connectionWorker.
  -- This is because when the connection to the db is lost, the listener also
  -- tries to recover the connection, but not with the same pace as the connectionWorker.
  -- Not waiting makes stderr quickly fill with connection retries messages from the listener.
  waitListener appState

  -- forkFinally allows to detect if the thread dies
  void . flip forkFinally (handleFinally dbChannel configDbPoolAutomaticRecovery) $ do
    dbOrError <- acquire $ toUtf8 (addFallbackAppName prettyVersion configDbUri)
    case dbOrError of
      Right db -> do
        observer $ DBListenerStart dbChannel
        SQL.listen db $ SQL.toPgIdentifier dbChannel
        SQL.waitForNotifications handleNotification db

      Left err -> do
        observer $ DBListenerFail dbChannel err
        exitFailure
  where
    handleFinally dbChannel False err = do
      observer $ DBListenerFailRecoverObs False dbChannel err
      killThread (getMainThreadId appState)
    handleFinally dbChannel True err = do
      -- if the thread dies, we try to recover
      observer $ DBListenerFailRecoverObs True dbChannel err
      -- assume the pool connection was also lost, call the connection worker
      connectionWorker appState
      -- retry the listener
      listener appState conf

    handleNotification channel msg =
      if | BS.null msg            -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload schema" -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload config" -> observer (DBListenerGotConfigMsg channel) >> reReadConfig False appState
         | otherwise              -> pure () -- Do nothing if anything else than an empty message is sent

    cacheReloader =
      -- reloads the schema cache + restarts pool connections
      -- it's necessary to restart the pg connections because they cache the pg catalog(see #2620)
      connectionWorker appState

checkIsFatal :: SQL.UsageError -> Maybe Text
checkIsFatal (SQL.ConnectionUsageError e)
  | isAuthFailureMessage = Just $ toS failureMessage
  | otherwise = Nothing
  where isAuthFailureMessage =
          ("FATAL:  password authentication failed" `isInfixOf` failureMessage) ||
          ("no password supplied" `isInfixOf` failureMessage)
        failureMessage = BS.unpack $ fromMaybe mempty e
checkIsFatal(SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError serverError)))
  = case serverError of
      -- Check for a syntax error (42601 is the pg code). This would mean the error is on our part somehow, so we treat it as fatal.
      SQL.ServerError "42601" _ _ _ _
        -> Just "This is probably a bug in PostgREST, please report it at https://github.com/PostgREST/postgrest/issues"
      -- Check for a "prepared statement <name> already exists" error (Code 42P05: duplicate_prepared_statement).
      -- This would mean that a connection pooler in transaction mode is being used
      -- while prepared statements are enabled in the PostgREST configuration,
      -- both of which are incompatible with each other.
      SQL.ServerError "42P05" _ _ _ _
        -> Just "If you are using connection poolers in transaction mode, try setting db-prepared-statements to false."
      -- Check for a "transaction blocks not allowed in statement pooling mode" error (Code 08P01: protocol_violation).
      -- This would mean that a connection pooler in statement mode is being used which is not supported in PostgREST.
      SQL.ServerError "08P01" "transaction blocks not allowed in statement pooling mode" _ _ _
        -> Just "Connection poolers in statement mode are not supported."
      _ -> Nothing
checkIsFatal _ = Nothing
