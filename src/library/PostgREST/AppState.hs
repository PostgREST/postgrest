{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module PostgREST.AppState
  ( AppState
  , destroy
  , getConfig
  , getSchemaCache
  , getPgVersion
  , getNextDelay
  , getTime
  , getJwtCacheState
  , init
  , initWithPool
  , killApp
  , putConfig -- For tests TODO refactoring
  , putSchemaCache
  , putPgVersion
  , putIsListenerOn
  , usePool
  , readInDbConfig
  , schemaCacheLoader
  , getObserver
  , isLoaded
  , isPending
  , waitForSchemaCacheInit
  , waitForSchemaCacheLoaded
  ) where

import qualified Hasql.Pool                 as SQL
import qualified Hasql.Session              as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified PostgREST.Auth.JwtCache    as JwtCache
import qualified PostgREST.Logger           as Logger
import qualified PostgREST.Metrics          as Metrics
import           PostgREST.Observation
import           PostgREST.TimeIt           (timeItT)
import           PostgREST.Version          (prettyVersion)

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate, updateAction)
import Control.Retry      (RetryPolicy, RetryStatus (..), capDelay,
                           exponentialBackoff, retrying, rsPreviousDelay)
import Data.IORef         (newIORef, readIORef)
import Data.Time.Clock    (getCurrentTime)

import Control.Concurrent.STM            (newEmptyTMVarIO, putTMVar, readTMVar,
                                          tryReadTMVar, tryTakeTMVar)
import PostgREST.AppState.Pool           (destroy, flushPool, initPool, usePool)
import PostgREST.Auth.JwtCache           (update)
import PostgREST.Config                  (AppConfig (..), readAppConfig)
import PostgREST.Config.Database         (queryDbSettings, queryPgVersion,
                                          queryRoleSettings)
import PostgREST.Config.PgVersion        (PgVersion (..), minimumPgVersion)
import PostgREST.Debounce                (makeDebouncer)
import PostgREST.SchemaCache             (SchemaCache (..), querySchemaCache,
                                          showSummary)
import PostgREST.SchemaCache.Identifiers (quoteQi)

import PostgREST.AppState.Types

import Protolude

init :: AppConfig -> IO () -> IO AppState
init conf@AppConfig{configLogLevel, configDbPoolSize} appKiller = do
  loggerState  <- Logger.init
  metricsState <- Metrics.init configDbPoolSize
  let observer = liftA2 (>>) (Logger.observationLogger loggerState configLogLevel) (Metrics.observationMetrics metricsState)

  observer $ AppStartObs prettyVersion

  pool <- initPool conf observer
  initWithPool pool conf loggerState metricsState observer appKiller

initWithPool :: SQL.Pool -> AppConfig -> Logger.LoggerState -> Metrics.MetricsState -> ObservationHandler -> IO () -> IO AppState
initWithPool pool conf loggerState metricsState observer appKiller = mdo

  appState <- AppState pool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> newSchemaCacheStatus
    <*> newIORef False
    <*> makeDebouncer (retryingSchemaCacheLoad appState *> threadDelay 100000)  -- 100ms cooldown
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> pure appKiller
    <*> newIORef 0
    <*> pure observer
    <*> JwtCache.init conf observer
    <*> pure loggerState
    <*> pure metricsState

  return appState

isConnEstablished :: AppState -> IO Bool
isConnEstablished appState = do
  AppConfig{..} <- getConfig appState
  if configDbChannelEnabled then -- if the listener is enabled, we can be sure the connection is up
    readIORef $ stateIsListenerOn appState
  else -- otherwise the only way to check the connection is to make a query
    isRight <$> usePool appState (SQL.sql "SELECT 1")

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

    (,) <$> qPgVersion <*> (qInDbConfig *> qSchemaCache)
  )
  where
    qPgVersion :: IO (Maybe PgVersion)
    qPgVersion = do
      AppConfig{..} <- getConfig appState
      pgVersion <- usePool appState queryPgVersion
      case pgVersion of
        Left e -> do
          observer $ QueryPgVersionError e
          unless configDbPoolAutomaticRecovery $ do
            observer ExitDBNoRecoveryObs
            killApp appState
          return Nothing
        Right actualPgVersion ->
          if actualPgVersion < minimumPgVersion then do
            observer $ ExitUnsupportedPgVersion actualPgVersion minimumPgVersion
            killApp appState
            return Nothing
          else do
            observer $ DBConnectedObs $ pgvFullName actualPgVersion
            observer $ PoolInit configDbPoolSize
            putPgVersion appState actualPgVersion
            return $ Just actualPgVersion

    qInDbConfig :: IO ()
    qInDbConfig = do
      AppConfig{..} <- getConfig appState
      when configDbConfig $ readInDbConfig False appState

    qSchemaCache :: IO (Maybe SchemaCache)
    qSchemaCache = do
      conf@AppConfig{..} <- getConfig appState
      (resultTime, result) <-
        timeItT $ usePool appState (SQL.transactionNoRetry SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
      case result of
        Left e -> do
          markSchemaCachePending appState
          observer $ SchemaCacheErrorObs configDbSchemas configDbExtraSearchPath e
          return Nothing

        Right (sCache, queryTimings) -> do
          -- IMPORTANT: While the pending schema cache state starts from running the above querySchemaCache, only at this stage we block API requests due to the usage of an
          -- IORef on putSchemaCache. This is why schema cache status is marked as pending here to signal the Admin server (using isPending) that we're on a recovery state.
          markSchemaCachePending appState
          putSchemaCache appState $ Just sCache
          (loadTime, summary) <- timeItT (evaluate $ showSummary sCache)
          -- Flush the pool after loading the schema cache to reset any stale session cache entries
          -- We do it after successfully querying the schema cache (because this can fail and during retries we would flush the pool repeatedly unnecessarily)
          -- and after marking sCacheStatus as pending,
          flushPool appState
          observer $ SchemaCacheQueriedObs resultTime queryTimings
          observer $ SchemaCacheLoadedObs loadTime summary
          markSchemaCacheLoaded appState
          return $ Just sCache

    shouldRetry :: RetryStatus -> (Maybe PgVersion, Maybe SchemaCache) -> IO Bool
    shouldRetry _ (pgVer, sCache) = do
      AppConfig{..} <- getConfig appState
      let itShould = configDbPoolAutomaticRecovery && (isNothing pgVer || isNothing sCache)
      return itShould

    retryPolicy :: RetryPolicy
    retryPolicy =
      let delayMicroseconds = 32*oneSecondInUs {-32 seconds-} in
      capDelay delayMicroseconds $ exponentialBackoff oneSecondInUs

    oneSecondInUs = 1_000_000 -- one second in microseconds

newSchemaCacheStatus :: IO SchemaCacheStatus
newSchemaCacheStatus = SchemaCacheStatus <$> newEmptyTMVarIO

markSchemaCachePending :: AppState -> IO ()
markSchemaCachePending = atomically . liftA2 (*>) tryTakeTMVar (`putTMVar` False) . getSCStatusTMVar . stateSCacheStatus

markSchemaCacheLoaded :: AppState -> IO ()
markSchemaCacheLoaded = atomically . liftA2 (*>) tryTakeTMVar (`putTMVar` True) . getSCStatusTMVar . stateSCacheStatus

isSchemaCacheLoaded :: AppState -> IO Bool
isSchemaCacheLoaded = atomically . (pure . fromMaybe False <=< tryReadTMVar) . getSCStatusTMVar . stateSCacheStatus

-- | Wait for initial schema cache load to either finish or retry
-- | We wait until scStatusTMVar is not empty.
waitForSchemaCacheInit :: AppState -> IO ()
waitForSchemaCacheInit = atomically . void . readTMVar . getSCStatusTMVar . stateSCacheStatus

waitForSchemaCacheLoaded :: AppState -> IO ()
waitForSchemaCacheLoaded = atomically . (check <=< readTMVar) . getSCStatusTMVar . stateSCacheStatus

-- | Reads the in-db config and reads the config file again
-- | We don't retry reading the in-db config after it fails immediately, because it could have user errors. We just report the error and continue.
readInDbConfig :: Bool -> AppState -> IO ()
readInDbConfig startingUp appState@AppState{stateObserver=observer} = do
  conf <- getConfig appState
  pgVer <- getPgVersion appState
  dbSettings <-
    if configDbConfig conf then do
      qDbSettings <- usePool appState (queryDbSettings (quoteQi <$> configDbPreConfig conf))
      case qDbSettings of
        Left e -> do
          observer $ ConfigReadErrorObs e
          pure mempty
        Right x -> pure x
    else
      pure mempty
  (roleSettings, roleIsolationLvl) <-
    if configDbConfig conf then do
      rSettings <- usePool appState (queryRoleSettings pgVer)
      case rSettings of
        Left e -> do
          observer $ QueryRoleSettingsErrorObs e
          pure (mempty, mempty)
        Right x -> pure x
    else
      pure mempty
  readAppConfig dbSettings (configFilePath conf) (Just $ configDbUri conf) roleSettings roleIsolationLvl >>= \case
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
