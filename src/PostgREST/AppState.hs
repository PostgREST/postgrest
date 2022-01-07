{-# LANGUAGE RecordWildCards #-}

module PostgREST.AppState
  ( AppState
  , getConfig
  , getDbStructure
  , getIsListenerOn
  , getIsWorkerOn
  , getJsonDbS
  , getMainThreadId
  , getPgVersion
  , getPool
  , getTime
  , getRetryNextIn
  , init
  , initWithPool
  , logWithZTime
  , putConfig
  , putDbStructure
  , putIsListenerOn
  , putIsWorkerOn
  , putJsonDbS
  , putPgVersion
  , putRetryNextIn
  , releasePool
  , signalListener
  , waitListener
  ) where

import qualified Hasql.Pool as SQL

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
  { statePool         :: SQL.Pool -- | Connection pool, either a 'Connection' or a 'ConnectionError'
  , statePgVersion    :: IORef PgVersion
  -- | No schema cache at the start. Will be filled in by the connectionWorker
  , stateDbStructure  :: IORef (Maybe DbStructure)
  -- | Cached DbStructure in json
  , stateJsonDbS      :: IORef ByteString
  -- | Helper ref to make sure just one connectionWorker can run at a time
  , stateIsWorkerOn   :: IORef Bool
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
  newPool <- initPool conf
  initWithPool newPool conf

initWithPool :: SQL.Pool -> AppConfig -> IO AppState
initWithPool newPool conf =
  AppState newPool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> newIORef mempty
    <*> newIORef False
    <*> newEmptyMVar
    <*> newIORef False
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
    <*> myThreadId
    <*> newIORef 0

initPool :: AppConfig -> IO SQL.Pool
initPool AppConfig{..} =
  SQL.acquire (configDbPoolSize, configDbPoolTimeout, toUtf8 configDbUri)

getPool :: AppState -> SQL.Pool
getPool = statePool

releasePool :: AppState -> IO ()
releasePool AppState{..} = SQL.release statePool >> throwTo stateMainThreadId UserInterrupt

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

getIsWorkerOn :: AppState -> IO Bool
getIsWorkerOn = readIORef . stateIsWorkerOn

putIsWorkerOn :: AppState -> Bool -> IO ()
putIsWorkerOn = atomicWriteIORef . stateIsWorkerOn

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
