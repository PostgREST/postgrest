{-# LANGUAGE RecordWildCards #-}
{-|
Module      : PostgREST.Logger
Description : Metrics based on the Observation module. See Observation.hs.
-}
module PostgREST.Metrics
  ( init
  , MetricsState (..)
  , PoolAvailableState(..)
  , emptyPoolAvailableState
  , stepPoolAvailable
  , poolAvailableGaugeValue
  , observationMetrics
  , metricsToText
  ) where

import qualified Data.ByteString.Lazy   as LBS
import           Data.IORef             (IORef, atomicModifyIORef',
                                         newIORef)
import qualified Data.Map.Strict        as M
import qualified Hasql.Pool.Observation as SQL

import Prometheus

import PostgREST.Observation

import Protolude

data PoolAvailableState k =
  PoolAvailableState {
    poolAvailableById  :: M.Map k Bool,
    poolAvailableCount :: Int,
    poolAvailableMax   :: Maybe Int
  }

emptyPoolAvailableState :: Maybe Int -> PoolAvailableState k
emptyPoolAvailableState maxSize =
  PoolAvailableState {
    poolAvailableById = M.empty,
    poolAvailableCount = 0,
    poolAvailableMax = maxSize
  }

stepPoolAvailable :: Ord k => PoolAvailableState k -> (k, SQL.ConnectionStatus) -> PoolAvailableState k
stepPoolAvailable st (connId, status) =
  let
    PoolAvailableState{..} = st
    wasReady = M.lookup connId poolAvailableById == Just True
    countWithoutOld = poolAvailableCount - if wasReady then 1 else 0
    (nextById, nextCount) = case status of
      SQL.ReadyForUseConnectionStatus ->
        (M.insert connId True poolAvailableById, countWithoutOld + 1)
      SQL.TerminatedConnectionStatus _ ->
        (M.delete connId poolAvailableById, countWithoutOld)
      _ ->
        (M.insert connId False poolAvailableById, countWithoutOld)
  in st { poolAvailableById = nextById, poolAvailableCount = nextCount }

poolAvailableGaugeValue :: PoolAvailableState k -> Int
poolAvailableGaugeValue PoolAvailableState{..} =
  let lowerBound = max 0 poolAvailableCount
  in maybe lowerBound (`min` lowerBound) poolAvailableMax

data MetricsState =
  MetricsState {
    poolTimeouts         :: Counter,
    poolAvailable        :: Gauge,
    poolWaiting          :: Gauge,
    poolMaxSize          :: Gauge,
    schemaCacheLoads     :: Vector Label1 Counter,
    schemaCacheQueryTime :: Gauge,
    jwtCacheRequests     :: Counter,
    jwtCacheHits         :: Counter,
    jwtCacheEvictions    :: Counter,
    poolAvailableState   :: IORef (PoolAvailableState Text)
  }

init :: Int -> IO MetricsState
init configDbPoolSize = do
  poolAvailableStateRef <- newIORef (emptyPoolAvailableState (Just configDbPoolSize))
  metricState <- MetricsState <$>
    register (counter (Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts")) <*>
    register (gauge (Info "pgrst_db_pool_available" "Available connections in the pool")) <*>
    register (gauge (Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection")) <*>
    register (gauge (Info "pgrst_db_pool_max" "Max pool connections")) <*>
    register (vector "status" $ counter (Info "pgrst_schema_cache_loads_total" "The total number of times the schema cache was loaded")) <*>
    register (gauge (Info "pgrst_schema_cache_query_time_seconds" "The query time in seconds of the last schema cache load")) <*>
    register (counter (Info "pgrst_jwt_cache_requests_total" "The total number of JWT cache lookups")) <*>
    register (counter (Info "pgrst_jwt_cache_hits_total" "The total number of JWT cache hits")) <*>
    register (counter (Info "pgrst_jwt_cache_evictions_total" "The total number of JWT cache evictions")) <*>
    pure poolAvailableStateRef
  setGauge (poolMaxSize metricState) (fromIntegral configDbPoolSize)
  pure metricState

-- Only some observations are used as metrics
observationMetrics :: MetricsState -> ObservationHandler
observationMetrics MetricsState{..} obs = case obs of
  (PoolAcqTimeoutObs _) -> do
    incCounter poolTimeouts
  (HasqlPoolObs (SQL.ConnectionObservation uuid status)) -> do
    updatePoolAvailable poolAvailableState poolAvailable (show uuid) status
  PoolRequest ->
    incGauge poolWaiting
  PoolRequestFullfilled ->
    decGauge poolWaiting
  SchemaCacheLoadedObs resTime -> do
    withLabel schemaCacheLoads "SUCCESS" incCounter
    setGauge schemaCacheQueryTime resTime
  SchemaCacheErrorObs{} -> do
    withLabel schemaCacheLoads "FAIL" incCounter
  JwtCacheLookup True -> incCounter jwtCacheRequests *> incCounter jwtCacheHits
  JwtCacheLookup False -> incCounter jwtCacheRequests
  JwtCacheEviction -> incCounter jwtCacheEvictions
  _ ->
    pure ()

updatePoolAvailable :: IORef (PoolAvailableState Text) -> Gauge -> Text -> SQL.ConnectionStatus -> IO ()
updatePoolAvailable stateRef poolGauge connId status = do
  gaugeValue <- atomicModifyIORef' stateRef $ \st ->
    let
      nextState = stepPoolAvailable st (connId, status)
      nextGauge = poolAvailableGaugeValue nextState
    in (nextState, nextGauge)
  setGauge poolGauge (fromIntegral gaugeValue)

metricsToText :: IO LBS.ByteString
metricsToText = exportMetricsAsText
