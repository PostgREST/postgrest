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
  MetricsState Counter Gauge Gauge Gauge (Vector Label1 Counter) Gauge

init :: Int -> IO MetricsState
init configDbPoolSize = do
  poolTimeouts <- register $ counter (Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts")
  poolAvailable <- register $ gauge (Info "pgrst_db_pool_available" "Available connections in the pool")
  poolWaiting <- register $ gauge (Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection")
  poolMaxSize <- register $ gauge (Info "pgrst_db_pool_max" "Max pool connections")
  schemaCacheLoads <- register $ vector "status" $ counter (Info "pgrst_schema_cache_loads_total" "The total number of times the schema cache was loaded")
  schemaCacheQueryTime <- register $ gauge (Info "pgrst_schema_cache_query_time_seconds" "The query time in seconds of the last schema cache load")
  setGauge poolMaxSize (fromIntegral configDbPoolSize)
  pure $ MetricsState poolTimeouts poolAvailable poolWaiting poolMaxSize schemaCacheLoads schemaCacheQueryTime

-- Only some observations are used as metrics
observationMetrics :: MetricsState -> ObservationHandler
observationMetrics (MetricsState poolTimeouts poolAvailable poolWaiting _ schemaCacheLoads schemaCacheQueryTime) obs = case obs of
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
  SchemaCacheErrorObs _ -> do
    withLabel schemaCacheLoads "FAIL" incCounter
  _ ->
    pure ()

metricsToText :: IO LBS.ByteString
metricsToText = exportMetricsAsText
