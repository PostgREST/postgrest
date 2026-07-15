{-|
Module      : PostgREST.AppState.Types
Description : AppState data type and stateful functions
-}
module PostgREST.AppState.Types where

import qualified Hasql.Pool              as SQL
import qualified PostgREST.Auth.JwtCache as JwtCache
import qualified PostgREST.Logger        as Logger
import qualified PostgREST.Metrics       as Metrics
import           PostgREST.Observation

import Data.IORef      (IORef, atomicWriteIORef, readIORef)
import Data.Time.Clock (UTCTime)

import Control.Concurrent.STM     (TMVar)
import PostgREST.Auth.JwtCache    (JwtCacheState)
import PostgREST.Config           (AppConfig (..))
import PostgREST.Config.PgVersion (PgVersion (..))
import PostgREST.SchemaCache      (SchemaCache (..))

import Protolude

data AppState = AppState
  -- | Database connection pool
  { statePool             :: SQL.Pool
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

getConfig :: AppState -> IO AppConfig
getConfig = readIORef . stateConf

putConfig :: AppState -> AppConfig -> IO ()
putConfig = atomicWriteIORef . stateConf

getTime :: AppState -> IO UTCTime
getTime = stateGetTime

getJwtCacheState :: AppState -> JwtCacheState
getJwtCacheState = stateJwtCache

killApp :: AppState -> IO ()
killApp = stateKillApp

putIsListenerOn :: AppState -> Bool -> IO ()
putIsListenerOn = atomicWriteIORef . stateIsListenerOn

getObserver :: AppState -> ObservationHandler
getObserver = stateObserver
