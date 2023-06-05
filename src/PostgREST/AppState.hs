{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.AppState
  ( AppState
  , destroy
  , getConfig
  , getSchemaCache
  , getIsListenerOn
  , getMainThreadId
  , getPgVersion
  , getRetryNextIn
  , getTime
  , init
  , initWithPool
  , logWithZTime
  , putSchemaCache
  , putPgVersion
  , usePool
  , loadSchemaCache
  , reReadConfig
  , connectionWorker
  , runListener
  ) where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.Either.Combinators    (whenLeft)
import qualified Data.Text.Encoding         as T
import           Hasql.Connection           (acquire)
import qualified Hasql.Notifications        as SQL
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Session              as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified PostgREST.Error            as Error

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Control.Debounce
import Control.Retry      (RetryStatus, capDelay, exponentialBackoff,
                           retrying, rsPreviousDelay)
import Data.IORef         (IORef, atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time          (ZonedTime, defaultTimeLocale, formatTime,
                           getZonedTime)
import Data.Time.Clock    (UTCTime, getCurrentTime)

import PostgREST.Config                  (AppConfig (..),
                                          readAppConfig)
import PostgREST.Config.Database         (queryDbSettings,
                                          queryPgVersion,
                                          queryRoleSettings)
import PostgREST.Config.PgVersion        (PgVersion (..),
                                          minimumPgVersion)
import PostgREST.SchemaCache             (SchemaCache,
                                          querySchemaCache)
import PostgREST.SchemaCache.Identifiers (dumpQi)

import Protolude


data AppState = AppState
  -- | Database connection pool
  { statePool                     :: SQL.Pool
  -- | Database server version, will be updated by the connectionWorker
  , statePgVersion                :: IORef PgVersion
  -- | No schema cache at the start. Will be filled in by the connectionWorker
  , stateSchemaCache              :: IORef (Maybe SchemaCache)
  -- | starts the connection worker with a debounce
  , debouncedConnectionWorker     :: IO ()
  -- | Binary semaphore used to sync the listener(NOTIFY reload) with the connectionWorker.
  , stateListener                 :: MVar ()
  -- | State of the LISTEN channel, used for the admin server checks
  , stateIsListenerOn             :: IORef Bool
  -- | Config that can change at runtime
  , stateConf                     :: IORef AppConfig
  -- | Time used for verifying JWT expiration
  , stateGetTime                  :: IO UTCTime
  -- | Time with time zone used for worker logs
  , stateGetZTime                 :: IO ZonedTime
  -- | Used for killing the main thread in case a subthread fails
  , stateMainThreadId             :: ThreadId
  -- | Keeps track of when the next retry for connecting to database is scheduled
  , stateRetryNextIn              :: IORef Int
  -- | Logs a pool error with a debounce
  , debounceLogAcquisitionTimeout :: IO ()
  }

init :: AppConfig -> IO AppState
init conf = do
  pool <- initPool conf
  initWithPool pool conf

initWithPool :: SQL.Pool -> AppConfig -> IO AppState
initWithPool pool conf = do
  appState <- AppState pool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> pure (pure ())
    <*> newEmptyMVar
    <*> newIORef False
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
    <*> myThreadId
    <*> newIORef 0
    <*> pure (pure ())


  debLogTimeout <-
    let oneSecond = 1000000 in
    mkDebounce defaultDebounceSettings
       { debounceAction = logPgrstError appState SQL.AcquisitionTimeoutUsageError
       , debounceFreq = 5*oneSecond
       , debounceEdge = leadingEdge -- logs at the start and the end
       }

  debWorker <-
    let decisecond = 100000 in
    mkDebounce defaultDebounceSettings
       { debounceAction = internalConnectionWorker appState
       , debounceFreq = decisecond
       , debounceEdge = leadingEdge -- runs the worker at the start and the end
       }

  return appState { debounceLogAcquisitionTimeout = debLogTimeout, debouncedConnectionWorker = debWorker }

destroy :: AppState -> IO ()
destroy = destroyPool

initPool :: AppConfig -> IO SQL.Pool
initPool AppConfig{..} =
  SQL.acquire
    configDbPoolSize
    (fromIntegral configDbPoolAcquisitionTimeout)
    (fromIntegral configDbPoolMaxLifetime)
    (fromIntegral configDbPoolMaxIdletime)
    (toUtf8 configDbUri)

-- | Run an action with a database connection.
usePool :: AppState -> SQL.Session a -> IO (Either SQL.UsageError a)
usePool AppState{..} x = do
  res <- SQL.use statePool x
  whenLeft res (\case
    SQL.AcquisitionTimeoutUsageError -> debounceLogAcquisitionTimeout -- this can happen rapidly for many requests, so we debounce
    _                                -> pure ())
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

-- | Log to stderr with local time
logWithZTime :: AppState -> Text -> IO ()
logWithZTime appState txt = do
  zTime <- stateGetZTime appState
  hPutStrLn stderr $ toS (formatTime defaultTimeLocale "%d/%b/%Y:%T %z: " zTime) <> txt

logPgrstError :: AppState -> SQL.UsageError -> IO ()
logPgrstError appState e = logWithZTime appState . T.decodeUtf8 . LBS.toStrict $ Error.errorPayload $ Error.PgError False e

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

getIsListenerOn :: AppState -> IO Bool
getIsListenerOn = readIORef . stateIsListenerOn

putIsListenerOn :: AppState -> Bool -> IO ()
putIsListenerOn = atomicWriteIORef . stateIsListenerOn

-- | Schema cache status
data SCacheStatus
  = SCLoaded
  | SCOnRetry
  | SCFatalFail

-- | Load the SchemaCache by using a connection from the pool.
loadSchemaCache :: AppState -> IO SCacheStatus
loadSchemaCache appState = do
  conf@AppConfig{..} <- getConfig appState
  result <-
    let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
    usePool appState . transaction SQL.ReadCommitted SQL.Read $
      querySchemaCache conf
  case result of
    Left e -> do
      case Error.checkIsFatal e of
        Just hint -> do
          logWithZTime appState "A fatal error ocurred when loading the schema cache"
          logPgrstError appState e
          logWithZTime appState hint
          return SCFatalFail
        Nothing -> do
          putSchemaCache appState Nothing
          logWithZTime appState "An error ocurred when loading the schema cache"
          logPgrstError appState e
          return SCOnRetry

    Right sCache -> do
      putSchemaCache appState (Just sCache)
      logWithZTime appState "Schema cache loaded"
      return SCLoaded

-- | Current database connection status data ConnectionStatus
data ConnectionStatus
  = NotConnected
  | Connected PgVersion
  | FatalConnectionError Text
  deriving (Eq)

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
internalConnectionWorker appState = work
  where
    work = do
      AppConfig{..} <- getConfig appState
      logWithZTime appState "Attempting to connect to the database..."
      connected <- establishConnection appState
      case connected of
        FatalConnectionError reason ->
          -- Fatal error when connecting
          logWithZTime appState reason >> killThread (getMainThreadId appState)
        NotConnected ->
          -- Unreachable because establishConnection will keep trying to connect
          return ()
        Connected actualPgVersion -> do
          -- Procede with initialization
          putPgVersion appState actualPgVersion
          when configDbChannelEnabled $
            signalListener appState
          logWithZTime appState "Connection successful"
          -- this could be fail because the connection drops, but the loadSchemaCache will pick the error and retry again
          -- We cannot retry after it fails immediately, because db-pre-config could have user errors. We just log the error and continue.
          when configDbConfig $ reReadConfig False appState
          scStatus <- loadSchemaCache appState
          case scStatus of
            SCLoaded ->
              -- do nothing and proceed if the load was successful
              return ()
            SCOnRetry ->
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
establishConnection appState =
  retrying retrySettings shouldRetry $
    const $ flushPool appState >> getConnectionStatus
  where
    retrySettings = capDelay delayMicroseconds $ exponentialBackoff backoffMicroseconds
    delayMicroseconds = 32000000 -- 32 seconds
    backoffMicroseconds = 1000000 -- 1 second

    getConnectionStatus :: IO ConnectionStatus
    getConnectionStatus = do
      pgVersion <- usePool appState $ queryPgVersion False -- No need to prepare the query here, as the connection might not be established
      case pgVersion of
        Left e -> do
          logPgrstError appState e
          case Error.checkIsFatal e of
            Just reason ->
              return $ FatalConnectionError reason
            Nothing ->
              return NotConnected
        Right version ->
          if version < minimumPgVersion then
            return . FatalConnectionError $
              "Cannot run in this PostgreSQL version, PostgREST needs at least "
              <> pgvName minimumPgVersion
          else
            return . Connected  $ version

    shouldRetry :: RetryStatus -> ConnectionStatus -> IO Bool
    shouldRetry rs isConnSucc = do
      let
        delay = fromMaybe 0 (rsPreviousDelay rs) `div` backoffMicroseconds
        itShould = NotConnected == isConnSucc
      when itShould . logWithZTime appState $
        "Attempting to reconnect to the database in "
        <> (show delay::Text)
        <> " seconds..."
      when itShould $ putRetryNextIn appState delay
      return itShould

-- | Re-reads the config plus config options from the db
reReadConfig :: Bool -> AppState -> IO ()
reReadConfig startingUp appState = do
  AppConfig{..} <- getConfig appState
  dbSettings <-
    if configDbConfig then do
      qDbSettings <- usePool appState $ queryDbSettings (dumpQi <$> configDbPreConfig) configDbPreparedStatements
      case qDbSettings of
        Left e -> do
          logWithZTime appState
            "An error ocurred when trying to query database settings for the config parameters"
          case Error.checkIsFatal e of
            Just hint -> do
              logPgrstError appState e
              logWithZTime appState hint
              killThread (getMainThreadId appState)
            Nothing -> do
              logPgrstError appState e
          pure mempty
        Right x -> pure x
    else
      pure mempty
  (roleSettings, roleIsolationLvl) <-
    if configDbConfig then do
      rSettings <- usePool appState $ queryRoleSettings configDbPreparedStatements
      case rSettings of
        Left e -> do
          logWithZTime appState "An error ocurred when trying to query the role settings"
          logPgrstError appState e
          pure (mempty, mempty)
        Right x -> pure x
    else
      pure mempty
  readAppConfig dbSettings configFilePath (Just configDbUri) roleSettings roleIsolationLvl >>= \case
    Left err   ->
      if startingUp then
        panic err -- die on invalid config if the program is starting up
      else
        logWithZTime appState $ "Failed reloading config: " <> err
    Right newConf -> do
      putConfig appState newConf
      if startingUp then
        pass
      else
        logWithZTime appState "Config reloaded"


runListener :: AppConfig -> AppState -> IO ()
runListener AppConfig{configDbChannelEnabled} appState =
  when configDbChannelEnabled $ listener appState

-- | Starts a dedicated pg connection to LISTEN for notifications.  When a
-- NOTIFY <db-channel> - with an empty payload - is done, it refills the schema
-- cache.  It uses the connectionWorker in case the LISTEN connection dies.
listener :: AppState -> IO ()
listener appState = do
  AppConfig{..} <- getConfig appState
  let dbChannel = toS configDbChannel

  -- The listener has to wait for a signal from the connectionWorker.
  -- This is because when the connection to the db is lost, the listener also
  -- tries to recover the connection, but not with the same pace as the connectionWorker.
  -- Not waiting makes stderr quickly fill with connection retries messages from the listener.
  waitListener appState

  -- forkFinally allows to detect if the thread dies
  void . flip forkFinally (handleFinally dbChannel) $ do
    dbOrError <- acquire $ toUtf8 configDbUri
    case dbOrError of
      Right db -> do
        logWithZTime appState $ "Listening for notifications on the " <> dbChannel <> " channel"
        putIsListenerOn appState True
        SQL.listen db $ SQL.toPgIdentifier dbChannel
        SQL.waitForNotifications handleNotification db
      _ ->
        die $ "Could not listen for notifications on the " <> dbChannel <> " channel"
  where
    handleFinally dbChannel _ = do
      -- if the thread dies, we try to recover
      logWithZTime appState $ "Retrying listening for notifications on the " <> dbChannel <> " channel.."
      putIsListenerOn appState False
      -- assume the pool connection was also lost, call the connection worker
      connectionWorker appState
      -- retry the listener
      listener appState

    handleNotification _ msg
      | BS.null msg            = cacheReloader
      | msg == "reload schema" = cacheReloader
      | msg == "reload config" = reReadConfig False appState
      | otherwise              = pure () -- Do nothing if anything else than an empty message is sent

    cacheReloader =
      -- reloads the schema cache + restarts pool connections
      -- it's necessary to restart the pg connections because they cache the pg catalog(see #2620)
      connectionWorker appState
