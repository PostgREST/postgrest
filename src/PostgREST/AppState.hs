{-# LANGUAGE RecordWildCards #-}

module PostgREST.AppState
  ( AppState
  , getConfig
  , getDbStructure
  , getIsWorkerOn
  , getJsonDbS
  , getMainThreadId
  , getPgVersion
  , getPool
  , getTime
  , init
  , initWithPool
  , logWithZTime
  , putConfig
  , putDbStructure
  , putIsWorkerOn
  , putJsonDbS
  , putPgVersion
  , releasePool
  , signalListener
  , waitListener
  ) where

import qualified Hasql.Pool as P

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

import Protolude      hiding (toS)
import Protolude.Conv (toS)


data AppState = AppState
  { statePool         :: P.Pool -- | Connection pool, either a 'Connection' or a 'ConnectionError'
  , statePgVersion    :: IORef PgVersion
  -- | No schema cache at the start. Will be filled in by the connectionWorker
  , stateDbStructure  :: IORef (Maybe DbStructure)
  -- | Cached DbStructure in json
  , stateJsonDbS      :: IORef ByteString
  -- | Helper ref to make sure just one connectionWorker can run at a time
  , stateIsWorkerOn   :: IORef Bool
  -- | Binary semaphore used to sync the listener(NOTIFY reload) with the connectionWorker.
  , stateListener     :: MVar ()
  -- | Config that can change at runtime
  , stateConf         :: IORef AppConfig
  -- | Time used for verifying JWT expiration
  , stateGetTime      :: IO UTCTime
  -- | Time with time zone used for worker logs
  , stateGetZTime     :: IO ZonedTime
  -- | Used for killing the main thread in case a subthread fails
  , stateMainThreadId :: ThreadId
  }

init :: AppConfig -> IO AppState
init conf = do
  newPool <- initPool conf
  initWithPool newPool conf

initWithPool :: P.Pool -> AppConfig -> IO AppState
initWithPool newPool conf =
  AppState newPool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> newIORef mempty
    <*> newIORef False
    <*> newEmptyMVar
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
    <*> myThreadId

initPool :: AppConfig -> IO P.Pool
initPool AppConfig{..} =
  P.acquire (configDbPoolSize, configDbPoolTimeout, toS configDbUri)

getPool :: AppState -> P.Pool
getPool = statePool

releasePool :: AppState -> IO ()
releasePool AppState{..} = P.release statePool >> throwTo stateMainThreadId UserInterrupt

getPgVersion :: AppState -> IO PgVersion
getPgVersion = readIORef . statePgVersion

putPgVersion :: AppState -> PgVersion -> IO ()
putPgVersion = atomicWriteIORef . statePgVersion

getDbStructure :: AppState -> IO (Maybe DbStructure)
getDbStructure = readIORef . stateDbStructure

putDbStructure :: AppState -> DbStructure -> IO ()
putDbStructure appState structure =
  atomicWriteIORef (stateDbStructure appState) $ Just structure

getJsonDbS :: AppState -> IO ByteString
getJsonDbS = readIORef . stateJsonDbS

putJsonDbS :: AppState -> ByteString -> IO ()
putJsonDbS appState = atomicWriteIORef (stateJsonDbS appState)

getIsWorkerOn :: AppState -> IO Bool
getIsWorkerOn = readIORef . stateIsWorkerOn

putIsWorkerOn :: AppState -> Bool -> IO ()
putIsWorkerOn = atomicWriteIORef . stateIsWorkerOn

getConfig :: AppState -> IO AppConfig
getConfig = readIORef . stateConf

putConfig :: AppState -> AppConfig -> IO ()
putConfig = atomicWriteIORef . stateConf

getTime :: AppState -> IO UTCTime
getTime = stateGetTime

-- | Log with local time
logWithZTime :: AppState -> Handle -> Text -> IO ()
logWithZTime appState hdl txt = do
  zTime <- stateGetZTime appState
  hPutStrLn hdl $ toS (formatTime defaultTimeLocale "%d/%b/%Y:%T %z: " zTime) <> txt

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
