{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module PostgREST.AppState
  ( AppState
  , destroy
  , getConfig
  , getSchemaCache
  , getMainThreadId
  , getPgVersion
  , getNextDelay
  , getNextListenerDelay
  , getTime
  , getJwtCacheState
  , init
  , initWithPool
  , putConfig -- For tests TODO refactoring
  , putNextListenerDelay
  , putSchemaCache
  , putPgVersion
  , putIsListenerOn
  , usePool
  , readInDbConfig
  , schemaCacheLoader
  , getObserver
  , isLoaded
  , isPending
  ) where

import qualified Data.ByteString.Char8      as BS
import           Data.Either.Combinators    (whenLeft)
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Pool.Config          as SQL
import qualified Hasql.Session              as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.HTTP.Types.Status  as HTTP
import qualified PostgREST.Auth.JwtCache    as JwtCache
import qualified PostgREST.Error            as Error
import qualified PostgREST.Logger           as Logger
import qualified PostgREST.Metrics          as Metrics
import           PostgREST.Observation
import           PostgREST.TimeIt           (timeItT)
import           PostgREST.Version          (prettyVersion)

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Control.Retry      (RetryPolicy, RetryStatus (..), capDelay,
                           exponentialBackoff, retrying,
                           rsPreviousDelay)
import Data.IORef         (IORef, atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time.Clock    (UTCTime, getCurrentTime)

import PostgREST.Auth.JwtCache           (JwtCacheState, update)
import PostgREST.Config                  (AppConfig (..),
                                          readAppConfig,
                                          toConnectionSettings)
import PostgREST.Config.Database         (queryDbSettings,
                                          queryPgVersion,
                                          queryRoleSettings)
import PostgREST.Config.PgVersion        (PgVersion (..),
                                          minimumPgVersion)
import PostgREST.Debounce                (makeDebouncer)
import PostgREST.SchemaCache             (SchemaCache (..),
                                          querySchemaCache,
                                          showSummary)
import PostgREST.SchemaCache.Identifiers (quoteQi)

import Protolude

data AppState = AppState
  -- | Database connection pool
  { statePool              :: SQL.Pool
  -- | Database server version
  , statePgVersion         :: IORef PgVersion
  -- | Schema cache
  , stateSchemaCache       :: IORef (Maybe SchemaCache)
  -- | The schema cache status
  , stateSCacheStatus      :: SchemaCacheStatus
  -- | State of the LISTEN channel
  , stateIsListenerOn      :: IORef Bool
  -- | starts the connection worker with a debounce
  , debouncedSCacheLoader  :: IO ()
  -- | Config that can change at runtime
  , stateConf              :: IORef AppConfig
  -- | Time used for verifying JWT expiration
  , stateGetTime           :: IO UTCTime
  -- | Used for killing the main thread in case a subthread fails
  , stateMainThreadId      :: ThreadId
  -- | Keeps track of the next delay for db connection retry
  , stateNextDelay         :: IORef Int
  -- | Keeps track of the next delay for the listener
  , stateNextListenerDelay :: IORef Int
  -- | Observation handler
  , stateObserver          :: ObservationHandler
  -- | JWT Cache
  , stateJwtCache          :: JwtCache.JwtCacheState
  , stateLogger            :: Logger.LoggerState
  , stateMetrics           :: Metrics.MetricsState
  }

-- | Schema cache status.
-- Empty means pending and full means loaded.
newtype SchemaCacheStatus = SchemaCacheStatus
  { getSCStatusMVar :: MVar ()
  }

init :: AppConfig -> IO AppState
init conf@AppConfig{configLogLevel, configDbPoolSize} = do
  loggerState  <- Logger.init
  metricsState <- Metrics.init configDbPoolSize
  let observer = liftA2 (>>) (Logger.observationLogger loggerState configLogLevel) (Metrics.observationMetrics metricsState)

  observer $ AppStartObs prettyVersion

  pool <- initPool conf observer
  initWithPool pool conf loggerState metricsState observer

initWithPool :: SQL.Pool -> AppConfig -> Logger.LoggerState -> Metrics.MetricsState -> ObservationHandler -> IO AppState
initWithPool pool conf loggerState metricsState observer = mdo

  appState <- AppState pool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> newSchemaCacheStatus
    <*> newIORef False
    <*> makeDebouncer (retryingSchemaCacheLoad appState *> threadDelay 100000)  -- 100ms cooldown
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> myThreadId
    <*> newIORef 0
    <*> newIORef 1
    <*> pure observer
    <*> JwtCache.init conf observer
    <*> pure loggerState
    <*> pure metricsState

  return appState

destroy :: AppState -> IO ()
destroy = destroyPool

initPool :: AppConfig -> ObservationHandler -> IO SQL.Pool
initPool cfg@AppConfig{..} observer = do
  SQL.acquire $ SQL.settings
    [ SQL.size configDbPoolSize
    , SQL.acquisitionTimeout $ fromIntegral configDbPoolAcquisitionTimeout
    , SQL.agingTimeout $ fromIntegral configDbPoolMaxLifetime
    , SQL.idlenessTimeout $ fromIntegral configDbPoolMaxIdletime
    , SQL.staticConnectionSettings $ toConnectionSettings identity cfg
    , SQL.observationHandler $ observer . HasqlPoolObs
    ]

-- | Run an action with a database connection.
usePool :: AppState -> SQL.Session a -> IO (Either SQL.UsageError a)
usePool AppState{stateObserver=observer, stateMainThreadId=mainThreadId, ..} sess = do
    observer PoolRequest

    res <- SQL.use statePool sess

    observer PoolRequestFullfilled

    whenLeft res (\case
      SQL.AcquisitionTimeoutUsageError ->
        observer PoolAcqTimeoutObs
      err@(SQL.ConnectionUsageError e) ->
        let failureMessage = BS.unpack $ fromMaybe mempty e in
        when (("FATAL:  password authentication failed" `isInfixOf` failureMessage) || ("no password supplied" `isInfixOf` failureMessage)) $ do
          observer $ ExitDBFatalError ServerAuthError err
          killThread mainThreadId
      err@(SQL.SessionUsageError (SQL.QueryError tpl _ (SQL.ResultError resultErr))) ->
        handleResultError err tpl resultErr
      err@(SQL.SessionUsageError (SQL.PipelineError (SQL.ResultError resultErr))) ->
        -- Passing the empty template will not work for schema cache queries, see TODO further below.
        handleResultError err mempty resultErr
      err@(SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ClientError _))) ->
        -- An error on the client-side, usually indicates problems with connection
        observer $ QueryErrorCodeHighObs err
      SQL.SessionUsageError (SQL.PipelineError (SQL.ClientError _))  -> pure ()
      )

    return res
  where
    handleResultError err tpl resultErr = do
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

-- | Flush the connection pool so that any future use of the pool will
-- use connections freshly established after this call.
-- | Emits PoolFlushed observation
flushPool :: AppState -> IO ()
flushPool AppState{..} = do
  SQL.release statePool
  stateObserver PoolFlushed

-- | Destroy the pool on shutdown.
-- | Differs from flushPool in not emiting PoolFlushed observation.
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

schemaCacheLoader :: AppState -> IO ()
schemaCacheLoader = debouncedSCacheLoader

getNextDelay :: AppState -> IO Int
getNextDelay = readIORef . stateNextDelay

getNextListenerDelay :: AppState -> IO Int
getNextListenerDelay = readIORef . stateNextListenerDelay

putNextListenerDelay :: AppState -> Int -> IO ()
putNextListenerDelay = atomicWriteIORef . stateNextListenerDelay

getConfig :: AppState -> IO AppConfig
getConfig = readIORef . stateConf

putConfig :: AppState -> AppConfig -> IO ()
putConfig = atomicWriteIORef . stateConf

getTime :: AppState -> IO UTCTime
getTime = stateGetTime

getJwtCacheState :: AppState -> JwtCacheState
getJwtCacheState = stateJwtCache

getMainThreadId :: AppState -> ThreadId
getMainThreadId = stateMainThreadId

isConnEstablished :: AppState -> IO Bool
isConnEstablished appState = do
  AppConfig{..} <- getConfig appState
  if configDbChannelEnabled then -- if the listener is enabled, we can be sure the connection is up
    readIORef $ stateIsListenerOn appState
  else -- otherwise the only way to check the connection is to make a query
    isRight <$> usePool appState (SQL.sql "SELECT 1")

putIsListenerOn :: AppState -> Bool -> IO ()
putIsListenerOn = atomicWriteIORef . stateIsListenerOn

isLoaded :: AppState -> IO Bool
isLoaded x = do
  scacheLoaded <- isSchemaCacheLoaded x
  connEstablished <- isConnEstablished x
  return $ scacheLoaded && connEstablished

isPending :: AppState -> IO Bool
isPending x = do
  scacheLoaded <- isSchemaCacheLoaded x
  connEstablished <- isConnEstablished x
  return $ not scacheLoaded || not connEstablished

getObserver :: AppState -> ObservationHandler
getObserver = stateObserver

-- | Try to load the schema cache and retry if it fails.
--
-- This is done by repeatedly: 1) flushing the pool, 2) querying the version and validating that the postgres version is supported by us, and 3) loading the schema cache.
-- It's necessary to flush the pool:
--
-- + Because connections cache the pg catalog(see #2620)
-- + For rapid recovery. Otherwise, the pool idle or lifetime timeout would have to be reached for new healthy connections to be acquired.
retryingSchemaCacheLoad :: AppState -> IO ()
retryingSchemaCacheLoad appState@AppState{stateObserver=observer} =
  void $ retrying retryPolicy shouldRetry (\RetryStatus{rsIterNumber, rsPreviousDelay} -> do
    when (rsIterNumber > 0) $ do
      let delay = fromMaybe 0 rsPreviousDelay `div` oneSecondInUs
      observer $ ConnectionRetryObs delay

    usePool appState $ schemaCacheLoad appState
  )
  where
    shouldRetry _ result = do
      AppConfig{..} <- getConfig appState
      return $ configDbPoolAutomaticRecovery && isLeft result

    retryPolicy :: RetryPolicy
    retryPolicy =
      let delayMicroseconds = 32*oneSecondInUs {-32 seconds-} in
      capDelay delayMicroseconds $ exponentialBackoff oneSecondInUs

    oneSecondInUs = 1000000 -- one second in microseconds

onError :: MonadError e m => m a -> (e -> m ()) -> m a
onError act handler = act `catchError` \e -> handler e *> throwError e

schemaCacheLoad :: AppState -> SQL.Session ()
schemaCacheLoad appState@AppState{stateObserver=observer, stateMainThreadId=mainThreadId} = do
  conf@AppConfig{..} <- liftIO $ getConfig appState

  pgVersion <- queryPgVersion
    `onError` \e -> liftIO $ do
      observer $ QueryPgVersionError (SQL.SessionUsageError e)
      unless configDbPoolAutomaticRecovery $ do
        observer ExitDBNoRecoveryObs
        killThread mainThreadId
  if pgVersion < minimumPgVersion then liftIO $ do
    observer $ ExitUnsupportedPgVersion pgVersion minimumPgVersion
    killThread mainThreadId
  else do
    liftIO $ do
      observer $ DBConnectedObs $ pgvFullName pgVersion
      observer $ PoolInit configDbPoolSize
      putPgVersion appState pgVersion

    readInDbConfig' False appState

    (resultTime, sCache) <-
      timeItT (SQL.transaction SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
        `onError` \e -> liftIO $ do
          markSchemaCachePending appState
          putSchemaCache appState Nothing
          observer $ SchemaCacheErrorObs configDbSchemas configDbExtraSearchPath (SQL.SessionUsageError e)

    liftIO $ do
      -- IMPORTANT: While the pending schema cache state starts from running the above querySchemaCache, only at this stage we block API requests due to the usage of an
      -- IORef on putSchemaCache. This is why schema cache status is marked as pending here to signal the Admin server (using isPending) that we're on a recovery state.
      markSchemaCachePending appState
      putSchemaCache appState $ Just sCache
      (loadTime, summary) <- timeItT (evaluate $ showSummary sCache)
      -- Flush the pool after loading the schema cache to reset any stale session cache entries
      -- We do it after successfully querying the schema cache (because this can fail and during retries we would flush the pool repeatedly unnecessarily)
      -- and after marking sCacheStatus as pending,
      flushPool appState
      observer $ SchemaCacheQueriedObs resultTime $ dbQueryTimings sCache
      observer $ SchemaCacheLoadedObs loadTime summary
      markSchemaCacheLoaded appState

newSchemaCacheStatus :: IO SchemaCacheStatus
newSchemaCacheStatus = SchemaCacheStatus <$> newEmptyMVar

markSchemaCachePending :: AppState -> IO ()
markSchemaCachePending = void . tryTakeMVar . getSCStatusMVar . stateSCacheStatus

markSchemaCacheLoaded :: AppState -> IO ()
markSchemaCacheLoaded = void . (`tryPutMVar` ()) . getSCStatusMVar . stateSCacheStatus

isSchemaCacheLoaded :: AppState -> IO Bool
isSchemaCacheLoaded = fmap not . isEmptyMVar . getSCStatusMVar . stateSCacheStatus

-- | Reads the in-db config and reads the config file again
-- | We don't retry reading the in-db config after it fails immediately, because it could have user errors. We just report the error and continue.
readInDbConfig :: Bool -> AppState -> IO ()
readInDbConfig startingUp appState =
  void $ usePool appState $ readInDbConfig' startingUp appState

readInDbConfig' :: Bool -> AppState -> SQL.Session ()
readInDbConfig' startingUp appState@AppState{stateObserver=observer} = do
  conf <- liftIO $ getConfig appState
  when (configDbConfig conf) $ do
    pgVer <- liftIO $ getPgVersion appState
    dbSettings <- queryDbSettings (quoteQi <$> configDbPreConfig conf)
      `onError` \e -> liftIO $ observer $ ConfigReadErrorObs (SQL.SessionUsageError e)

    (roleSettings, roleIsolationLvl) <- queryRoleSettings pgVer
      `onError` \e -> liftIO $ observer $ QueryRoleSettingsErrorObs (SQL.SessionUsageError e)

    liftIO $ readAppConfig dbSettings (configFilePath conf) (Just $ configDbUri conf) roleSettings roleIsolationLvl >>= \case
      Left err   ->
        if startingUp then
          panic err -- die on invalid config if the program is starting up
        else
          observer $ ConfigInvalidObs err
      Right newConf -> do
        putConfig appState newConf
        -- After the config has reloaded, jwt-secret might have changed, so
        -- if it has changed, it is important to invalidate the jwt cache
        -- entries, because they were cached using the old secret
        update (getJwtCacheState appState) newConf

        if startingUp then
          pass
        else
          observer ConfigSucceededObs
