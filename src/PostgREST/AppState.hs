{-# LANGUAGE RecordWildCards #-}

module PostgREST.AppState
  ( AppState
  , destroy
  , flushPool
  , getConfig
  , getDbStructure
  , getIsListenerOn
  , getJsonDbS
  , getMainThreadId
  , getPgVersion
  , getRetryNextIn
  , getTime
  , getWorkerSem
  , init
  , initWithPool
  , logWithZTime
  , putConfig
  , putDbStructure
  , putIsListenerOn
  , putJsonDbS
  , putPgVersion
  , putRetryNextIn
  , signalListener
  , usePool
  , waitListener
  ) where

import qualified Hasql.Pool    as SQL
import qualified Hasql.Session as SQL

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Data.IORef         (IORef, atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time          (ZonedTime, defaultTimeLocale, formatTime,
                           getZonedTime)
import Data.Time.Clock    (UTCTime, getCurrentTime)

import PostgREST.Config           (AppConfig (..))
import PostgREST.Config.PgVersion (PgVersion (..), minimumPgVersion)
import PostgREST.DbStructure      (DbStructure)

import Protolude


data AppState = AppState
  -- | Database connection pool
  { statePool         :: SQL.Pool
  -- | Database server version, will be updated by the connectionWorker
  , statePgVersion    :: IORef PgVersion
  -- | No schema cache at the start. Will be filled in by the connectionWorker
  , stateDbStructure  :: IORef (Maybe DbStructure)
  -- | Cached DbStructure in json
  , stateJsonDbS      :: IORef ByteString
  -- | Binary semaphore to make sure just one connectionWorker can run at a time
  , stateWorkerSem    :: MVar ()
  -- | Binary semaphore used to sync the listener(NOTIFY reload) with the connectionWorker.
  , stateListener     :: MVar ()
  -- | State of the LISTEN channel, used for the admin server checks
  , stateIsListenerOn :: IORef Bool
  -- | Config that can change at runtime
  , stateConf         :: IORef AppConfig
  -- | Time used for verifying JWT expiration
  , stateGetTime      :: IO UTCTime
  -- | Time with time zone used for worker logs
  , stateGetZTime     :: IO ZonedTime
  -- | Used for killing the main thread in case a subthread fails
  , stateMainThreadId :: ThreadId
  -- | Keeps track of when the next retry for connecting to database is scheduled
  , stateRetryNextIn  :: IORef Int
  }

init :: AppConfig -> IO AppState
init conf = do
  pool <- initPool conf
  initWithPool pool conf

initWithPool :: SQL.Pool -> AppConfig -> IO AppState
initWithPool pool conf =
  AppState pool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> newIORef mempty
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newIORef False
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
    <*> myThreadId
    <*> newIORef 0

destroy :: AppState -> IO ()
destroy = destroyPool

initPool :: AppConfig -> IO SQL.Pool
initPool AppConfig{..} =
  SQL.acquire configDbPoolSize $ toUtf8 configDbUri

-- | Run an action with a database connection.
usePool :: AppState -> SQL.Session a -> IO (Either SQL.UsageError a)
usePool AppState{..} = SQL.use statePool

-- | Flush the connection pool so that any future use of the pool will
-- use connections freshly established after this call.
flushPool :: AppState -> IO ()
flushPool AppState{..} = SQL.flush statePool

-- | Destroy the pool on shutdown.
destroyPool :: AppState -> IO ()
destroyPool AppState{..} = SQL.release statePool

getPgVersion :: AppState -> IO PgVersion
getPgVersion = readIORef . statePgVersion

putPgVersion :: AppState -> PgVersion -> IO ()
putPgVersion = atomicWriteIORef . statePgVersion

getDbStructure :: AppState -> IO (Maybe DbStructure)
getDbStructure = readIORef . stateDbStructure

putDbStructure :: AppState -> Maybe DbStructure -> IO ()
putDbStructure appState = atomicWriteIORef (stateDbStructure appState)

getJsonDbS :: AppState -> IO ByteString
getJsonDbS = readIORef . stateJsonDbS

putJsonDbS :: AppState -> ByteString -> IO ()
putJsonDbS appState = atomicWriteIORef (stateJsonDbS appState)

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
