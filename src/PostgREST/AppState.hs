{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

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
  , getSocketREST
  , getSocketAdmin
  , init
  , initSockets
  , initWithPool
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
import qualified Data.Text                  as T (unpack)
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Pool.Config          as SQL
import qualified Hasql.Session              as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.HTTP.Types.Status  as HTTP
import qualified Network.Socket             as NS
import qualified PostgREST.Auth.JwtCache    as JwtCache
import qualified PostgREST.Error            as Error
import qualified PostgREST.Logger           as Logger
import qualified PostgREST.Metrics          as Metrics
import           PostgREST.Observation
import           PostgREST.Version          (prettyVersion)
import           System.TimeIt              (timeItT)

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Control.Debounce
import Control.Retry      (RetryPolicy, RetryStatus (..), capDelay,
                           exponentialBackoff, retrying,
                           rsPreviousDelay)
import Data.IORef         (IORef, atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time.Clock    (UTCTime, getCurrentTime)

import PostgREST.Auth.JwtCache           (JwtCacheState)
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

import Data.Streaming.Network (bindPortTCP, bindRandomPortTCP)
import Data.String            (IsString (..))
import Protolude

data AppState = AppState
  -- | Database connection pool
  { statePool              :: SQL.Pool
  -- | Database server version
  , statePgVersion         :: IORef PgVersion
  -- | Schema cache
  , stateSchemaCache       :: IORef (Maybe SchemaCache)
  -- | The schema cache status
  , stateSCacheStatus      :: IORef SchemaCacheStatus
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
  -- | Network socket for REST API
  , stateSocketREST        :: NS.Socket
  -- | Network socket for the admin UI
  , stateSocketAdmin       :: Maybe NS.Socket
  -- | Observation handler
  , stateObserver          :: ObservationHandler
  -- | JWT Cache
  , stateJwtCache          :: JwtCache.JwtCacheState
  , stateLogger            :: Logger.LoggerState
  , stateMetrics           :: Metrics.MetricsState
  }

-- | Schema cache status
data SchemaCacheStatus
  = SCLoaded
  | SCPending
  deriving Eq

type AppSockets = (NS.Socket, Maybe NS.Socket)

init :: AppConfig -> IO AppState
init conf@AppConfig{configLogLevel, configDbPoolSize} = do
  loggerState  <- Logger.init
  metricsState <- Metrics.init configDbPoolSize
  let observer = liftA2 (>>) (Logger.observationLogger loggerState configLogLevel) (Metrics.observationMetrics metricsState)

  observer $ AppStartObs prettyVersion

  jwtCacheState <- JwtCache.init
  pool <- initPool conf observer
  (sock, adminSock) <- initSockets conf
  state' <- initWithPool (sock, adminSock) pool conf jwtCacheState loggerState metricsState observer
  pure state' { stateSocketREST = sock, stateSocketAdmin = adminSock}

initWithPool :: AppSockets -> SQL.Pool -> AppConfig -> JwtCache.JwtCacheState -> Logger.LoggerState -> Metrics.MetricsState -> ObservationHandler -> IO AppState
initWithPool (sock, adminSock) pool conf jwtCacheState loggerState metricsState observer = do

  appState <- AppState pool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> newIORef SCPending
    <*> newIORef False
    <*> pure (pure ())
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> myThreadId
    <*> newIORef 0
    <*> newIORef 1
    <*> pure sock
    <*> pure adminSock
    <*> pure observer
    <*> pure jwtCacheState
    <*> pure loggerState
    <*> pure metricsState

  deb <-
    let decisecond = 100000 in
    mkDebounce defaultDebounceSettings
       { debounceAction = retryingSchemaCacheLoad appState
       , debounceFreq = decisecond
       , debounceEdge = leadingEdge -- runs the worker at the start and the end
       }

  return appState { debouncedSCacheLoader = deb}

destroy :: AppState -> IO ()
destroy = destroyPool

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

initPool :: AppConfig -> ObservationHandler -> IO SQL.Pool
initPool AppConfig{..} observer = do
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
usePool AppState{stateObserver=observer, stateMainThreadId=mainThreadId, ..} sess = do
  observer PoolRequest

  res <- SQL.use statePool sess

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
    err@(SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ClientError _))) ->
      -- An error on the client-side, usually indicates problems wth connection
        observer $ QueryErrorCodeHighObs err
    )

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

getSocketREST :: AppState -> NS.Socket
getSocketREST = stateSocketREST

getSocketAdmin :: AppState -> Maybe NS.Socket
getSocketAdmin = stateSocketAdmin

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
  scacheStatus <- readIORef $ stateSCacheStatus x
  connEstablished <- isConnEstablished x
  return $ scacheStatus == SCLoaded && connEstablished

isPending :: AppState -> IO Bool
isPending x = do
  scacheStatus <- readIORef $ stateSCacheStatus x
  connEstablished <- isConnEstablished x
  return $ scacheStatus == SCPending || not connEstablished

putSCacheStatus :: AppState -> SchemaCacheStatus -> IO ()
putSCacheStatus = atomicWriteIORef . stateSCacheStatus

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
retryingSchemaCacheLoad appState@AppState{stateObserver=observer, stateMainThreadId=mainThreadId} =
  void $ retrying retryPolicy shouldRetry (\RetryStatus{rsIterNumber, rsPreviousDelay} -> do
    when (rsIterNumber > 0) $ do
      let delay = fromMaybe 0 rsPreviousDelay `div` oneSecondInUs
      observer $ ConnectionRetryObs delay
      putNextListenerDelay appState delay

    flushPool appState

    (,) <$> qPgVersion <*> (qInDbConfig *> qSchemaCache)
  )
  where
    qPgVersion :: IO (Maybe PgVersion)
    qPgVersion = do
      AppConfig{..} <- getConfig appState
      pgVersion <- usePool appState (queryPgVersion False) -- No need to prepare the query here, as the connection might not be established
      case pgVersion of
        Left e -> do
          observer $ QueryPgVersionError e
          unless configDbPoolAutomaticRecovery $ do
            observer ExitDBNoRecoveryObs
            killThread mainThreadId
          return Nothing
        Right actualPgVersion -> do
          when (actualPgVersion < minimumPgVersion) $ do
            observer $ ExitUnsupportedPgVersion actualPgVersion minimumPgVersion
            killThread mainThreadId
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
        let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
        timeItT $ usePool appState (transaction SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
      case result of
        Left e -> do
          putSCacheStatus appState SCPending
          putSchemaCache appState Nothing
          observer $ SchemaCacheErrorObs e
          return Nothing

        Right sCache -> do
          -- IMPORTANT: While the pending schema cache state starts from running the above querySchemaCache, only at this stage we block API requests due to the usage of an
          -- IORef on putSchemaCache. This is why SCacheStatus is put at SCPending here to signal the Admin server (using isPending) that we're on a recovery state.
          putSCacheStatus appState SCPending
          putSchemaCache appState $ Just sCache
          observer $ SchemaCacheQueriedObs resultTime
          (t, _) <- timeItT $ observer $ SchemaCacheSummaryObs $ showSummary sCache
          observer $ SchemaCacheLoadedObs t
          putSCacheStatus appState SCLoaded
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

    oneSecondInUs = 1000000 -- one second in microseconds

-- | Reads the in-db config and reads the config file again
-- | We don't retry reading the in-db config after it fails immediately, because it could have user errors. We just report the error and continue.
readInDbConfig :: Bool -> AppState -> IO ()
readInDbConfig startingUp appState@AppState{stateObserver=observer} = do
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
