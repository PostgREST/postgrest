{-# LANGUAGE RecordWildCards #-}

module PostgREST.AppState
  ( AppState
  , destroy
  , flushPool
  , getConfig
  , getSchemaCache
  , getIsListenerOn
  , getMainThreadId
  , getPgVersion
  , getRetryNextIn
  , getTime
  , getWorkerSem
  , init
  , initWithPool
  , logWithZTime
  , logPgrstError
  , putConfig
  , putSchemaCache
  , putIsListenerOn
  , putPgVersion
  , putRetryNextIn
  , signalListener
  , usePool
  , waitListener
  , debounceLogAcquisitionTimeout
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as T
import qualified Hasql.Pool           as SQL
import qualified Hasql.Session        as SQL
import qualified PostgREST.Error      as Error

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Control.Debounce
import Data.IORef         (IORef, atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time          (ZonedTime, defaultTimeLocale, formatTime,
                           getZonedTime)
import Data.Time.Clock    (UTCTime, getCurrentTime)

import PostgREST.Config           (AppConfig (..))
import PostgREST.Config.PgVersion (PgVersion (..), minimumPgVersion)
import PostgREST.SchemaCache      (SchemaCache)

import Protolude


data AppState = AppState
  -- | Database connection pool
  { statePool                     :: SQL.Pool
  -- | Database server version, will be updated by the connectionWorker
  , statePgVersion                :: IORef PgVersion
  -- | No schema cache at the start. Will be filled in by the connectionWorker
  , stateSchemaCache              :: IORef (Maybe SchemaCache)
  -- | Binary semaphore to make sure just one connectionWorker can run at a time
  , stateWorkerSem                :: MVar ()
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
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newIORef False
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
    <*> myThreadId
    <*> newIORef 0
    <*> pure (pure ())

  deb <-
    let oneSecond = 1000000 in
    mkDebounce defaultDebounceSettings
       { debounceAction = logPgrstError appState SQL.AcquisitionTimeoutUsageError
       , debounceFreq = 5*oneSecond
       , debounceEdge = leadingEdge -- logs at the start and the end
       }

  return appState { debounceLogAcquisitionTimeout = deb }

destroy :: AppState -> IO ()
destroy = destroyPool

initPool :: AppConfig -> IO SQL.Pool
initPool AppConfig{..} =
  SQL.acquire
    configDbPoolSize
    (fromIntegral configDbPoolAcquisitionTimeout)
    (fromIntegral configDbPoolMaxLifetime)
    (toUtf8 configDbUri)

-- | Run an action with a database connection.
usePool :: AppState -> SQL.Session a -> IO (Either SQL.UsageError a)
usePool AppState{..} = SQL.use statePool

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

getWorkerSem :: AppState -> MVar ()
getWorkerSem = stateWorkerSem

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
