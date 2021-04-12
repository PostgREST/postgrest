{-# LANGUAGE RecordWildCards #-}

module PostgREST.AppState
  ( AppState
  , getConfig
  , getDbStructure
  , getIsWorkerOn
  , getMainThreadId
  , getPgVersion
  , getPool
  , getTime
  , init
  , initWithPool
  , putConfig
  , putDbStructure
  , putIsWorkerOn
  , putPgVersion
  , releasePool
  ) where

import qualified Hasql.Pool as P

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Data.IORef         (IORef, atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time.Clock    (UTCTime, getCurrentTime)

import PostgREST.Config                (AppConfig (..))
import PostgREST.DbStructure           (DbStructure)
import PostgREST.DbStructure.PgVersion (PgVersion (..))

import Protolude      hiding (toS)
import Protolude.Conv (toS)


data AppState = AppState
  { statePool         :: P.Pool -- | Connection pool, either a 'Connection' or a 'ConnectionError'
  -- | Used to sync the listener(NOTIFY reload) with the connectionWorker. No
  -- connection for the listener at first. Only used if dbChannelEnabled=true.
  , statePgVersion    :: MVar PgVersion
  -- | No schema cache at the start. Will be filled in by the connectionWorker
  , stateDbStructure  :: IORef (Maybe DbStructure)
  -- | Helper ref to make sure just one connectionWorker can run at a time
  , stateIsWorkerOn   :: IORef Bool
  -- | Config that can change at runtime
  , stateConf         :: IORef AppConfig
  , stateGetTime      :: IO UTCTime
  , stateMainThreadId :: ThreadId
  }

init :: AppConfig -> IO AppState
init conf = do
  newPool <- initPool conf
  initWithPool newPool conf

initWithPool :: P.Pool -> AppConfig -> IO AppState
initWithPool newPool conf =
  AppState newPool
    <$> newEmptyMVar
    <*> newIORef Nothing
    <*> newIORef False
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> myThreadId

initPool :: AppConfig -> IO P.Pool
initPool AppConfig{..} =
  P.acquire (configDbPoolSize, configDbPoolTimeout, toS configDbUri)

getPool :: AppState -> P.Pool
getPool = statePool

releasePool :: AppState -> IO ()
releasePool AppState{..} = P.release statePool >> throwTo stateMainThreadId UserInterrupt

-- | As this IO action uses `takeMVar` internally, it will only return once
-- `statePgVersion` has been set using `putPgVersion`. This is currently used
-- to syncronize workers.
getPgVersion :: AppState -> IO PgVersion
getPgVersion = takeMVar . statePgVersion

putPgVersion :: AppState -> PgVersion -> IO ()
putPgVersion appState pgVer = void $ tryPutMVar (statePgVersion appState) pgVer

getDbStructure :: AppState -> IO (Maybe DbStructure)
getDbStructure = readIORef . stateDbStructure

putDbStructure :: AppState -> DbStructure -> IO ()
putDbStructure appState structure =
  atomicWriteIORef (stateDbStructure appState) $ Just structure

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

getMainThreadId :: AppState -> ThreadId
getMainThreadId = stateMainThreadId
