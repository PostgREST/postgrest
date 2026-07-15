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

import qualified Hasql.Pool              as SQL
import qualified Hasql.Session           as SQL
import qualified PostgREST.Auth.JwtCache as JwtCache
import qualified PostgREST.Logger        as Logger
import qualified PostgREST.Metrics       as Metrics
import           PostgREST.Observation
import           PostgREST.Version       (prettyVersion)

import Control.AutoUpdate         (defaultUpdateSettings, mkAutoUpdate,
                                   updateAction)
import Control.Concurrent.STM     (newEmptyTMVarIO)
import Data.IORef                 (newIORef, readIORef)
import Data.Time.Clock            (getCurrentTime)
import PostgREST.AppState.Pool    (destroy, initPool, usePool)
import PostgREST.AppState.Reload  (isSchemaCacheLoaded, readInDbConfig,
                                   retryingSchemaCacheLoad,
                                   waitForSchemaCacheInit,
                                   waitForSchemaCacheLoaded)
import PostgREST.AppState.Types
import PostgREST.Config           (AppConfig (..))
import PostgREST.Config.PgVersion (minimumPgVersion)
import PostgREST.Debounce         (makeDebouncer)

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

newSchemaCacheStatus :: IO SchemaCacheStatus
newSchemaCacheStatus = SchemaCacheStatus <$> newEmptyTMVarIO
