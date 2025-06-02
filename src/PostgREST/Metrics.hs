{-# LANGUAGE RecordWildCards #-}
{-|
Module      : PostgREST.Logger
Description : Metrics based on the Observation module. See Observation.hs.
-}
module PostgREST.Metrics
  ( init
  , MetricsState (..)
  , observationMetrics
  , metricsToText
  ) where

import qualified Data.ByteString.Lazy   as LBS
import qualified Hasql.Pool.Observation as SQL

import Prometheus

import PostgREST.Observation

import Protolude

data MetricsState =
  MetricsState {
    poolTimeouts         :: Counter,
    poolAvailable        :: Gauge,
    poolWaiting          :: Gauge,
    poolMaxSize          :: Gauge,
    schemaCacheLoads     :: Vector Label1 Counter,
    schemaCacheQueryTime :: Gauge
  }

init :: Int -> IO MetricsState
init configDbPoolSize = do
  metricState <- MetricsState <$>
    register (counter (Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts")) <*>
    register (gauge (Info "pgrst_db_pool_available" "Available connections in the pool")) <*>
    register (gauge (Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection")) <*>
    register (gauge (Info "pgrst_db_pool_max" "Max pool connections")) <*>
    register (vector "status" $ counter (Info "pgrst_schema_cache_loads_total" "The total number of times the schema cache was loaded")) <*>
    register (gauge (Info "pgrst_schema_cache_query_time_seconds" "The query time in seconds of the last schema cache load"))
  setGauge (poolMaxSize metricState) (fromIntegral configDbPoolSize)
  pure metricState

-- Only some observations are used as metrics
observationMetrics :: MetricsState -> ObservationHandler
observationMetrics MetricsState{..} obs = case obs of
  (PoolAcqTimeoutObs _) -> do
    incCounter poolTimeouts
  (HasqlPoolObs (SQL.ConnectionObservation _ status)) -> case status of
     SQL.ReadyForUseConnectionStatus  -> do
      incGauge poolAvailable
     SQL.InUseConnectionStatus        -> do
      decGauge poolAvailable
     SQL.TerminatedConnectionStatus  _ -> do
      decGauge poolAvailable
     SQL.ConnectingConnectionStatus -> pure ()
  PoolRequest ->
    incGauge poolWaiting
  PoolRequestFullfilled ->
    decGauge poolWaiting
  SchemaCacheLoadedObs resTime -> do
    withLabel schemaCacheLoads "SUCCESS" incCounter
    setGauge schemaCacheQueryTime resTime
  SchemaCacheErrorObs{} -> do
    withLabel schemaCacheLoads "FAIL" incCounter
  _ ->
    pure ()

metricsToText :: IO LBS.ByteString
metricsToText = exportMetricsAsText
