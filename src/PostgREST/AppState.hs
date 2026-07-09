{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module PostgREST.AppState
  ( AppState
  , ResidentTenantSchemaCache (..)
  , TenantState
  , destroy
  , getConfig
  , getConfigWithTenantGeneration
  , getSchemaCache
  , getResidentTenantSchemaCache
  , getTenantConfig
  , getTenantSchemaCache
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
  , useTenantPool
  , resolveTenant
  , releaseTenant
  , reloadTenantSchemaCache
  , tenantStateKey
  , readInDbConfig
  , schemaCacheLoader
  , getObserver
  , isLoaded
  , isPending
  , waitForSchemaCacheInit
  ) where

import qualified Data.ByteString.Char8      as BS
import           Data.Either.Combinators    (whenLeft)
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Pool.Config          as SQL
import qualified Hasql.Session              as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.HTTP.Types.Status  as HTTP
import qualified PostgREST.Auth.JwtCache    as JwtCache
import qualified PostgREST.Error            as Error
import qualified PostgREST.Logger           as Logger
import qualified PostgREST.Metrics          as Metrics
import qualified Control.Exception          as E
import           PostgREST.Observation
import           PostgREST.TimeIt           (timeItT)
import           PostgREST.Version          (prettyVersion)

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Control.Retry      (RetryPolicy, RetryStatus (..), capDelay,
                           exponentialBackoff, retrying,
                           rsPreviousDelay)
import Data.IORef         (IORef, atomicModifyIORef', atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time.Clock    (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

import Control.Concurrent.STM            (TMVar, newEmptyTMVarIO,
                                          putTMVar, readTMVar,
                                          tryReadTMVar, tryTakeTMVar)
import PostgREST.Auth.JwtCache           (JwtCacheState, update)
import qualified PostgREST.Config        as Config
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

data TenantState = TenantState
  { tenantStateKey    :: Maybe ByteString
  , tenantPool        :: SQL.Pool
  , tenantConfigRef   :: IORef AppConfig
  , tenantSchemaCache :: IORef (Maybe SchemaCache)
  , tenantLastUsed    :: IORef POSIXTime
  , tenantActiveReqs  :: IORef Int
  , tenantRetired     :: IORef Bool
  }

data ResidentTenantSchemaCache
  = ResidentTenantMissing
  | ResidentTenantSchemaCachePending
  | ResidentTenantSchemaCacheLoaded SchemaCache

data TenantInitLock = TenantInitLock
  { tenantInitLock  :: MVar ()
  , tenantInitUsers :: IORef Int
  }

data TenantLookup
  = TenantFound TenantState
  | TenantMissing
  | TenantTooMany Int

data TenantRegistration
  = TenantRegistered
  | TenantRegistrationFound TenantState
  | TenantRegistrationTooMany Int
  | TenantRegistrationStale

data AppState = AppState
  -- | Database connection pool
  { statePool              :: SQL.Pool
  , stateDefaultTenant     :: TenantState
  , stateTenants           :: IORef (HM.HashMap ByteString TenantState)
  , stateTenantInitLocks   :: IORef (HM.HashMap ByteString TenantInitLock)
  , stateTenantFailures    :: IORef (HM.HashMap ByteString POSIXTime)
  , stateTenantColdStarts  :: IORef Int
  , stateTenantGeneration  :: IORef Int
  , stateTenantsLock       :: MVar ()
  -- | Database server version
  , statePgVersion        :: IORef PgVersion
  -- | Schema cache
  , stateSchemaCache      :: IORef (Maybe SchemaCache)
  -- | The schema cache status
  , stateSCacheStatus     :: SchemaCacheStatus
  -- | State of the LISTEN channel
  , stateIsListenerOn     :: IORef Bool
  -- | starts the connection worker with a debounce
  , debouncedSCacheLoader :: IO ()
  -- | Config that can change at runtime
  , stateConf             :: IORef AppConfig
  -- | Time used for verifying JWT expiration
  , stateGetTime          :: IO UTCTime
  -- | Used for killing the main thread in case a subthread fails
  , stateKillApp          :: IO ()
  -- | Keeps track of the next delay for db connection retry
  , stateNextDelay        :: IORef Int
  -- | Observation handler
  , stateObserver         :: ObservationHandler
  -- | JWT Cache
  , stateJwtCache         :: JwtCache.JwtCacheState
  , stateLogger           :: Logger.LoggerState
  , stateMetrics          :: Metrics.MetricsState
  }

-- | Schema cache status.
-- Empty means initial loading on startup, False means pending and True means loaded.
-- "Initial" state is needed so that we can wait with application socket listening
-- until after initial schema cache querying.
newtype SchemaCacheStatus = SchemaCacheStatus
  { getSCStatusTMVar :: TMVar Bool
  }

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
  pgVersionRef <- newIORef minimumPgVersion
  schemaCacheRef <- newIORef Nothing
  configRef <- newIORef conf
  now <- getPOSIXTime
  lastUsedRef <- newIORef now
  activeReqsRef <- newIORef 0
  retiredRef <- newIORef False
  let defaultTenant = TenantState Nothing pool configRef schemaCacheRef lastUsedRef activeReqsRef retiredRef

  appState <- AppState pool defaultTenant
    <$> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef 0
    <*> newIORef 0
    <*> newMVar ()
    <*> pure pgVersionRef -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> pure schemaCacheRef
    <*> newSchemaCacheStatus
    <*> newIORef False
    <*> makeDebouncer (retryingSchemaCacheLoad appState *> threadDelay 100000)  -- 100ms cooldown
    <*> pure configRef
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> pure appKiller
    <*> newIORef 0
    <*> pure observer
    <*> JwtCache.init conf observer
    <*> pure loggerState
    <*> pure metricsState

  return appState

-- | Destroy the pool on shutdown.
-- | Differs from flushPool in not emiting PoolFlushed observation.
destroy :: AppState -> IO ()
destroy AppState{..} = do
  SQL.release statePool
  tenants <- readIORef stateTenants
  traverse_ (SQL.release . tenantPool) tenants

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
useTenantPool :: AppState -> TenantState -> SQL.Session a -> IO (Either SQL.UsageError a)
useTenantPool AppState{stateObserver=observer} tenantState@TenantState{tenantPool} sess =
  bracket_ (retainTenant tenantState) (releaseTenant tenantState) $ do
    observer PoolRequest
    res <- SQL.use tenantPool sess
    observer PoolRequestFullfilled
    whenLeft res $ \case
      SQL.AcquisitionTimeoutUsageError -> observer PoolAcqTimeoutObs
      err@(SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError (SQL.ServerError{})))) ->
        when (Error.status (Error.PgError False err) >= HTTP.status500) $
          observer $ QueryErrorCodeHighObs err
      _ -> pure ()
    pure res

retainTenant :: TenantState -> IO ()
retainTenant TenantState{tenantLastUsed, tenantActiveReqs} = do
  now <- getPOSIXTime
  atomicWriteIORef tenantLastUsed now
  atomicModifyIORef' tenantActiveReqs $ \n -> (n + 1, ())

releaseTenant :: TenantState -> IO ()
releaseTenant tenantState@TenantState{tenantLastUsed, tenantActiveReqs} = do
  now <- getPOSIXTime
  atomicWriteIORef tenantLastUsed now
  activeReqs <- atomicModifyIORef' tenantActiveReqs $ \n ->
    let n' = max 0 (n - 1) in (n', n')
  releaseRetiredTenantIfIdle tenantState activeReqs

retireTenant :: TenantState -> IO ()
retireTenant TenantState{tenantRetired} =
  atomicWriteIORef tenantRetired True

releaseRetiredTenantIfIdle :: TenantState -> Int -> IO ()
releaseRetiredTenantIfIdle TenantState{tenantStateKey, tenantPool, tenantRetired} activeReqs =
  when (isJust tenantStateKey && activeReqs == 0) $ do
    shouldRelease <- atomicModifyIORef' tenantRetired $ \retired ->
      (False, retired)
    when shouldRelease $ SQL.release tenantPool

-- | Run an action with a database connection.
usePool :: AppState -> SQL.Session a -> IO (Either SQL.UsageError a)
usePool appState@AppState{stateObserver=observer, ..} sess = do
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
          killApp appState
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
          killApp appState
        SQL.RowError{} -> do
          observer $ ExitDBFatalError ServerPgrstBug err
          killApp appState
        SQL.UnexpectedAmountOfRows{} -> do
          observer $ ExitDBFatalError ServerPgrstBug err
          killApp appState
        -- Check for a syntax error (42601 is the pg code) only for queries that don't have `WITH pgrst_source` as prefix.
        -- This would mean the error is on our schema cache queries, so we treat it as fatal.
        -- TODO have a better way to mark this as a schema cache query
        SQL.ServerError "42601" _ _ _ _ ->
          unless ("WITH pgrst_source" `BS.isPrefixOf` tpl) $ do
            observer $ ExitDBFatalError ServerPgrstBug err
            killApp appState
        -- Check for a "prepared statement <name> already exists" error (Code 42P05: duplicate_prepared_statement).
        -- This would mean that a connection pooler in transaction mode is being used
        -- while prepared statements are enabled in the PostgREST configuration,
        -- both of which are incompatible with each other.
        SQL.ServerError "42P05" _ _ _ _ -> do
          observer $ ExitDBFatalError ServerError42P05 err
          killApp appState
        -- Check for a "transaction blocks not allowed in statement pooling mode" error (Code 08P01: protocol_violation).
        -- This would mean that a connection pooler in statement mode is being used which is not supported in PostgREST.
        SQL.ServerError "08P01" "transaction blocks not allowed in statement pooling mode" _ _ _ -> do
          observer $ ExitDBFatalError ServerError08P01 err
          killApp appState
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

getPgVersion :: AppState -> IO PgVersion
getPgVersion = readIORef . statePgVersion

putPgVersion :: AppState -> PgVersion -> IO ()
putPgVersion = atomicWriteIORef . statePgVersion

getSchemaCache :: AppState -> IO (Maybe SchemaCache)
getSchemaCache = readIORef . stateSchemaCache

getResidentTenantSchemaCache :: AppState -> ByteString -> IO (Either Error.Error ResidentTenantSchemaCache)
getResidentTenantSchemaCache AppState{stateTenants, stateTenantsLock} tenantKey
  | not $ validTenantKey tenantKey = pure $ Left Error.TenantIdInvalid
  | otherwise =
      withMVar stateTenantsLock $ \_ -> do
        tenants <- readIORef stateTenants
        case HM.lookup tenantKey tenants of
          Nothing -> pure $ Right ResidentTenantMissing
          Just TenantState{tenantSchemaCache} ->
            readIORef tenantSchemaCache <&> \case
              Nothing -> Right ResidentTenantSchemaCachePending
              Just sCache -> Right $ ResidentTenantSchemaCacheLoaded sCache

putSchemaCache :: AppState -> Maybe SchemaCache -> IO ()
putSchemaCache appState = atomicWriteIORef (stateSchemaCache appState)

getTenantConfig :: TenantState -> IO AppConfig
getTenantConfig = readIORef . tenantConfigRef

getTenantSchemaCache :: TenantState -> IO (Maybe SchemaCache)
getTenantSchemaCache = readIORef . tenantSchemaCache

resolveTenant :: AppState -> AppConfig -> Int -> Maybe ByteString -> IO (Either Error.Error TenantState)
resolveTenant appState conf configGeneration maybeTenantKey
  | maybe False (not . validTenantKey) maybeTenantKey =
      pure $ Left Error.TenantIdInvalid
  | otherwise =
      case (configTenantDbUriTemplate conf, maybeTenantKey) of
        (Just template, Just tenantKey) -> getOrCreateTenant appState conf configGeneration template tenantKey
        (Just _, Nothing)              -> pure $ Left Error.TenantIdRequired
        (Nothing, Nothing)
          | tenantContextRequired conf  -> pure $ Left Error.TenantIdRequired
        (Nothing, _)                   -> do
          let tenantState = stateDefaultTenant appState
          retainTenant tenantState
          pure $ Right tenantState
  where
    tenantContextRequired AppConfig{configTenantClaimKey, configTenantHeader, configTenantHostRegex} =
      isJust configTenantClaimKey || isJust configTenantHeader || isJust configTenantHostRegex

getOrCreateTenant :: AppState -> AppConfig -> Int -> Text -> ByteString -> IO (Either Error.Error TenantState)
getOrCreateTenant appState@AppState{stateObserver=observer} baseConf configGeneration template tenantKey =
  if not $ validTenantKey tenantKey
    then pure $ Left Error.TenantIdInvalid
    else do
      lookupResult <- lookupTenant appState baseConf tenantKey
      case lookupResult of
        TenantFound tenantState -> ensureResolvedTenant appState baseConf tenantKey tenantState
        TenantTooMany maxResident -> pure $ Left $ Error.TenantResidentLimitExceeded maxResident
        TenantMissing -> do
          cachedFailure <- tenantFailureCached appState baseConf tenantKey
          if cachedFailure
            then pure $ Left Error.TenantTemporarilyUnavailable
            else withTenantInitLock appState tenantKey $ do
              secondLookup <- lookupTenant appState baseConf tenantKey
              case secondLookup of
                TenantFound tenantState -> ensureResolvedTenantLocked appState baseConf tenantKey tenantState
                TenantTooMany maxResident -> pure $ Left $ Error.TenantResidentLimitExceeded maxResident
                TenantMissing -> do
                  cachedFailureAgain <- tenantFailureCached appState baseConf tenantKey
                  if cachedFailureAgain
                    then pure $ Left Error.TenantTemporarilyUnavailable
                    else withTenantColdStartPermit appState baseConf $ do
                      let tenantText = decodeUtf8 tenantKey
                          tenantConf = baseConf { configDbUri = T.replace "{tenant}" tenantText template }
                      pool <- initPool tenantConf observer
                      (do
                         schemaCacheRef <- newIORef Nothing
                         confRef <- newIORef tenantConf
                         now <- getPOSIXTime
                         lastUsedRef <- newIORef now
                         activeReqsRef <- newIORef 0
                         retiredRef <- newIORef False
                         let tenantState = TenantState (Just tenantKey) pool confRef schemaCacheRef lastUsedRef activeReqsRef retiredRef
                         loaded <- loadTenantSchemaCache appState tenantState
                         if loaded
                           then do
                             clearTenantFailure appState tenantKey
                             registerTenant appState baseConf configGeneration tenantKey tenantState >>= \case
                               TenantRegistered -> do
                                 pure $ Right tenantState
                               TenantRegistrationFound existingTenantState -> do
                                 SQL.release pool
                                 ensureResolvedTenantLocked appState baseConf tenantKey existingTenantState
                               TenantRegistrationTooMany maxResident -> do
                                 SQL.release pool
                                 pure $ Left $ Error.TenantResidentLimitExceeded maxResident
                               TenantRegistrationStale -> do
                                 retainTenant tenantState
                                 retireTenant tenantState
                                 pure $ Right tenantState
                           else do
                             recordTenantFailure appState baseConf tenantKey
                             SQL.release pool
                             pure $ Left Error.NoSchemaCacheError)
                        `E.onException` (recordTenantFailure appState baseConf tenantKey >> SQL.release pool)
validTenantKey :: ByteString -> Bool
validTenantKey key = not (BS.null key) && BS.all validTenantKeyChar key
  where
    validTenantKeyChar c =
      BS.elem c ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-" :: ByteString)

lookupTenant :: AppState -> AppConfig -> ByteString -> IO TenantLookup
lookupTenant appState@AppState{stateTenants, stateTenantsLock} baseConf tenantKey =
  withMVar stateTenantsLock $ \_ -> do
    evictIdleTenants appState baseConf
    tenants <- readIORef stateTenants
    case HM.lookup tenantKey tenants of
      Just tenantState -> do
        retainTenant tenantState
        pure $ TenantFound tenantState
      Nothing
        | configTenantMaxResident baseConf > 0 && HM.size tenants >= configTenantMaxResident baseConf ->
            pure $ TenantTooMany $ configTenantMaxResident baseConf
        | otherwise ->
            pure TenantMissing

registerTenant :: AppState -> AppConfig -> Int -> ByteString -> TenantState -> IO TenantRegistration
registerTenant appState@AppState{stateTenants, stateTenantGeneration, stateTenantsLock} baseConf configGeneration tenantKey tenantState =
  withMVar stateTenantsLock $ \_ -> do
    currentGeneration <- readIORef stateTenantGeneration
    if currentGeneration /= configGeneration
      then pure TenantRegistrationStale
      else do
        evictIdleTenants appState baseConf
        tenants <- readIORef stateTenants
        case HM.lookup tenantKey tenants of
          Just existingTenantState -> do
            retainTenant existingTenantState
            pure $ TenantRegistrationFound existingTenantState
          Nothing
            | configTenantMaxResident baseConf > 0 && HM.size tenants >= configTenantMaxResident baseConf ->
                pure $ TenantRegistrationTooMany $ configTenantMaxResident baseConf
            | otherwise -> do
                retainTenant tenantState
                atomicModifyIORef' stateTenants $ \m -> (HM.insert tenantKey tenantState m, ())
                pure TenantRegistered

tenantFailureCached :: AppState -> AppConfig -> ByteString -> IO Bool
tenantFailureCached AppState{stateTenantFailures, stateTenantsLock} AppConfig{configTenantFailureCacheTtl} tenantKey
  | configTenantFailureCacheTtl <= 0 = do
      withMVar stateTenantsLock $ \_ ->
        atomicModifyIORef' stateTenantFailures $ \m -> (HM.delete tenantKey m, ())
      pure False
  | otherwise =
      withMVar stateTenantsLock $ \_ -> do
        now <- getPOSIXTime
        failures <- readIORef stateTenantFailures
        case HM.lookup tenantKey failures of
          Just expiresAt
            | now < expiresAt -> pure True
            | otherwise -> do
                atomicModifyIORef' stateTenantFailures $ \m -> (HM.delete tenantKey m, ())
                pure False
          Nothing -> pure False

recordTenantFailure :: AppState -> AppConfig -> ByteString -> IO ()
recordTenantFailure AppState{stateTenantFailures, stateTenantsLock} AppConfig{configTenantFailureCacheTtl} tenantKey
  | configTenantFailureCacheTtl <= 0 =
      withMVar stateTenantsLock $ \_ ->
        atomicModifyIORef' stateTenantFailures $ \m -> (HM.delete tenantKey m, ())
  | otherwise =
      withMVar stateTenantsLock $ \_ -> do
        now <- getPOSIXTime
        let expiresAt = now + fromIntegral configTenantFailureCacheTtl
        atomicModifyIORef' stateTenantFailures $ \m -> (HM.insert tenantKey expiresAt m, ())

clearTenantFailure :: AppState -> ByteString -> IO ()
clearTenantFailure AppState{stateTenantFailures, stateTenantsLock} tenantKey =
  withMVar stateTenantsLock $ \_ ->
    atomicModifyIORef' stateTenantFailures $ \m -> (HM.delete tenantKey m, ())

withTenantColdStartPermit :: AppState -> AppConfig -> IO (Either Error.Error a) -> IO (Either Error.Error a)
withTenantColdStartPermit appState baseConf@AppConfig{configTenantMaxColdStarts} action
  | configTenantMaxColdStarts <= 0 = action
  | otherwise = do
      acquired <- tryAcquireTenantColdStart appState baseConf
      if not acquired
        then pure $ Left $ Error.TenantColdStartLimitExceeded configTenantMaxColdStarts
        else E.finally action $ releaseTenantColdStart appState

tryAcquireTenantColdStart :: AppState -> AppConfig -> IO Bool
tryAcquireTenantColdStart AppState{stateTenantColdStarts, stateTenantsLock} AppConfig{configTenantMaxColdStarts} =
  withMVar stateTenantsLock $ \_ -> do
    active <- readIORef stateTenantColdStarts
    if active >= configTenantMaxColdStarts
      then pure False
      else do
        atomicModifyIORef' stateTenantColdStarts $ \n -> (n + 1, ())
        pure True

releaseTenantColdStart :: AppState -> IO ()
releaseTenantColdStart AppState{stateTenantColdStarts, stateTenantsLock} =
  withMVar stateTenantsLock $ \_ ->
    atomicModifyIORef' stateTenantColdStarts $ \n -> (max 0 (n - 1), ())

ensureResolvedTenant :: AppState -> AppConfig -> ByteString -> TenantState -> IO (Either Error.Error TenantState)
ensureResolvedTenant appState baseConf tenantKey tenantState =
  readIORef (tenantSchemaCache tenantState) >>= \case
    Just _  -> pure $ Right tenantState
    Nothing -> do
      cachedFailure <- tenantFailureCached appState baseConf tenantKey
      if cachedFailure
        then do
          releaseTenant tenantState
          pure $ Left Error.TenantTemporarilyUnavailable
        else withTenantInitLock appState tenantKey $
          ensureResolvedTenantLocked appState baseConf tenantKey tenantState

ensureResolvedTenantLocked :: AppState -> AppConfig -> ByteString -> TenantState -> IO (Either Error.Error TenantState)
ensureResolvedTenantLocked appState baseConf tenantKey tenantState =
  readIORef (tenantSchemaCache tenantState) >>= \case
    Just _ -> do
      clearTenantFailure appState tenantKey
      pure $ Right tenantState
    Nothing -> do
      cachedFailure <- tenantFailureCached appState baseConf tenantKey
      if cachedFailure
        then do
          releaseTenant tenantState
          pure $ Left Error.TenantTemporarilyUnavailable
        else do
          loaded <- loadTenantSchemaCache appState tenantState
          if loaded
            then do
              clearTenantFailure appState tenantKey
              pure $ Right tenantState
            else do
              recordTenantFailure appState baseConf tenantKey
              releaseTenant tenantState
              dropTenant appState tenantKey tenantState
              pure $ Left Error.NoSchemaCacheError

withTenantInitLock :: AppState -> ByteString -> IO a -> IO a
withTenantInitLock appState tenantKey action =
  E.bracket (acquireTenantInitLock appState tenantKey) (releaseTenantInitLock appState tenantKey) $
    \TenantInitLock{tenantInitLock} -> withMVar tenantInitLock $ \_ -> action

acquireTenantInitLock :: AppState -> ByteString -> IO TenantInitLock
acquireTenantInitLock AppState{stateTenantInitLocks, stateTenantsLock} tenantKey =
  withMVar stateTenantsLock $ \_ -> do
    locks <- readIORef stateTenantInitLocks
    case HM.lookup tenantKey locks of
      Just lock@TenantInitLock{tenantInitUsers} -> do
        atomicModifyIORef' tenantInitUsers $ \n -> (n + 1, ())
        pure lock
      Nothing -> do
        lock <- TenantInitLock <$> newMVar () <*> newIORef 1
        atomicModifyIORef' stateTenantInitLocks $ \m -> (HM.insert tenantKey lock m, ())
        pure lock

releaseTenantInitLock :: AppState -> ByteString -> TenantInitLock -> IO ()
releaseTenantInitLock AppState{stateTenantInitLocks, stateTenantsLock} tenantKey TenantInitLock{tenantInitUsers} =
  withMVar stateTenantsLock $ \_ -> do
    users <- atomicModifyIORef' tenantInitUsers $ \n ->
      let n' = max 0 (n - 1) in (n', n')
    when (users == 0) $
      atomicModifyIORef' stateTenantInitLocks $ \m -> (HM.delete tenantKey m, ())

dropTenant :: AppState -> ByteString -> TenantState -> IO ()
dropTenant AppState{stateTenants, stateTenantsLock} tenantKey tenantState =
  withMVar stateTenantsLock (\_ -> do
    removed <- atomicModifyIORef' stateTenants $ \m ->
      case HM.lookup tenantKey m of
        Just current | tenantSchemaCache current == tenantSchemaCache tenantState ->
          (HM.delete tenantKey m, True)
        _ ->
          (m, False)
    when removed $ retireTenant tenantState
    pure removed
  ) >>= \removed ->
    when removed $ do
      -- Waiters for the same tenant already retained this state before blocking
      -- on the init lock, so the pool can only be released after they drain.
      activeReqs <- readIORef $ tenantActiveReqs tenantState
      releaseRetiredTenantIfIdle tenantState activeReqs

evictIdleTenants :: AppState -> AppConfig -> IO ()
evictIdleTenants AppState{stateTenants} AppConfig{configTenantMaxIdletime} =
  when (configTenantMaxIdletime > 0) $ do
    now <- getPOSIXTime
    tenants <- readIORef stateTenants
    evictable <- fmap catMaybes . for (HM.toList tenants) $ \(tenantKey, tenantState@TenantState{tenantLastUsed, tenantActiveReqs}) -> do
      lastUsed <- readIORef tenantLastUsed
      activeReqs <- readIORef tenantActiveReqs
      pure $
        if activeReqs == 0 && now - lastUsed >= fromIntegral configTenantMaxIdletime
          then Just (tenantKey, tenantState)
          else Nothing
    unless (null evictable) $ do
      atomicModifyIORef' stateTenants $ \m -> (foldr (HM.delete . fst) m evictable, ())
      traverse_ (SQL.release . tenantPool . snd) evictable

loadTenantSchemaCache :: AppState -> TenantState -> IO Bool
loadTenantSchemaCache appState@AppState{stateObserver=observer} tenantState@TenantState{tenantStateKey=maybeTenantKey, tenantPool, tenantConfigRef, tenantSchemaCache} = do
  conf@AppConfig{..} <- readIORef tenantConfigRef
  result <- useTenantPool appState tenantState (SQL.transactionNoRetry SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
  case result of
    Left e -> do
      atomicWriteIORef tenantSchemaCache Nothing
      for_ maybeTenantKey $ recordTenantFailure appState conf
      observer $ SchemaCacheErrorObs configDbSchemas configDbExtraSearchPath e
      pure False
    Right (sCache, _queryTimings) -> do
      atomicWriteIORef tenantSchemaCache $ Just sCache
      for_ maybeTenantKey $ clearTenantFailure appState
      observer $ SchemaCacheLoadedObs 0 $ showSummary sCache
      SQL.release tenantPool
      observer PoolFlushed
      pure True

reloadResidentTenantSchemaCaches :: AppState -> IO ()
reloadResidentTenantSchemaCaches appState = do
  tenants <- retainedResidentTenants appState
  for_ tenants $ \(tenantKey, tenantState) -> do
    loaded <- E.bracket_ (pure ()) (releaseTenant tenantState) $
      withTenantInitLock appState tenantKey $
        loadTenantSchemaCache appState tenantState
    unless loaded $ dropTenant appState tenantKey tenantState

reloadTenantSchemaCache :: AppState -> ByteString -> IO Bool
reloadTenantSchemaCache appState tenantKey
  | not $ validTenantKey tenantKey = pure False
  | otherwise =
      retainedTenant appState tenantKey >>= \case
        Nothing -> pure False
        Just tenantState -> do
          loaded <- E.bracket_ (pure ()) (releaseTenant tenantState) $
            withTenantInitLock appState tenantKey $
              loadTenantSchemaCache appState tenantState
          unless loaded $ dropTenant appState tenantKey tenantState
          pure loaded

retainedResidentTenants :: AppState -> IO [(ByteString, TenantState)]
retainedResidentTenants AppState{stateTenants, stateTenantsLock} =
  withMVar stateTenantsLock $ \_ -> do
    tenants <- HM.toList <$> readIORef stateTenants
    traverse_ (retainTenant . snd) tenants
    pure tenants

retainedTenant :: AppState -> ByteString -> IO (Maybe TenantState)
retainedTenant AppState{stateTenants, stateTenantsLock} tenantKey =
  withMVar stateTenantsLock $ \_ -> do
    tenants <- readIORef stateTenants
    case HM.lookup tenantKey tenants of
      Nothing -> pure Nothing
      Just tenantState -> do
        retainTenant tenantState
        pure $ Just tenantState

schemaCacheLoader :: AppState -> IO ()
schemaCacheLoader = debouncedSCacheLoader

getNextDelay :: AppState -> IO Int
getNextDelay = readIORef . stateNextDelay

getConfig :: AppState -> IO AppConfig
getConfig = readIORef . stateConf

getConfigWithTenantGeneration :: AppState -> IO (AppConfig, Int)
getConfigWithTenantGeneration AppState{stateConf, stateTenantGeneration, stateTenantsLock} =
  withMVar stateTenantsLock $ \_ ->
    (,) <$> readIORef stateConf <*> readIORef stateTenantGeneration

putConfig :: AppState -> AppConfig -> IO ()
putConfig = atomicWriteIORef . stateConf

putConfigAndMaybeRetireResidentTenants :: AppState -> AppConfig -> IO ()
putConfigAndMaybeRetireResidentTenants AppState{stateConf, stateTenantGeneration, stateTenants, stateTenantFailures, stateTenantsLock} newConf = do
  retiredTenants <- withMVar stateTenantsLock $ \_ -> do
    oldConf <- readIORef stateConf
    tenants <- HM.elems <$> readIORef stateTenants
    atomicWriteIORef stateConf newConf
    if Config.toText oldConf == Config.toText newConf
      then pure mempty
      else do
        atomicModifyIORef' stateTenantGeneration $ \generation ->
          let generation' = generation + 1 in (generation', ())
        atomicWriteIORef stateTenants mempty
        atomicWriteIORef stateTenantFailures mempty
        traverse_ retireTenant tenants
        pure tenants

  for_ retiredTenants $ \tenantState -> do
    activeReqs <- readIORef $ tenantActiveReqs tenantState
    releaseRetiredTenantIfIdle tenantState activeReqs

getTime :: AppState -> IO UTCTime
getTime = stateGetTime

getJwtCacheState :: AppState -> JwtCacheState
getJwtCacheState = stateJwtCache

killApp :: AppState -> IO ()
killApp = stateKillApp

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
          reloadResidentTenantSchemaCaches appState
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
      if startingUp then
        putConfig appState newConf
      else
        putConfigAndMaybeRetireResidentTenants appState newConf
      -- After the config has reloaded, jwt-secret might have changed, so
      -- if it has changed, it is important to invalidate the jwt cache
      -- entries, because they were cached using the old secret
      update (getJwtCacheState appState) newConf

      if startingUp then
        pass
      else
        observer ConfigSucceededObs
