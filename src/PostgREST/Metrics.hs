module PostgREST.Metrics
  ( init
  , MetricsState (..)
  , observationMetrics
  , metricsToText
  ) where

import qualified Data.ByteString.Lazy   as LBS
import qualified Hasql.Pool.Observation as SQL

import qualified Prometheus as Prom

import PostgREST.Observation

import Protolude

data MetricsState =
  MetricsState Prom.Counter Prom.Gauge Prom.Gauge Prom.Gauge Prom.Counter Prom.Gauge

init :: Int -> IO MetricsState
init configDbPoolSize = do
  poolTimeouts <- Prom.register $ Prom.counter (Prom.Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts")
  poolAvailable <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_available" "Available connections in the pool")
  poolWaiting <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection")
  poolMaxSize <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_max" "Max pool connections")
  schemaCacheLoads <- Prom.register $ Prom.counter (Prom.Info "pgrst_schema_cache_loads_total" "The total number of times the schema cache was loaded")
  schemaCacheQueryTime <- Prom.register $ Prom.gauge (Prom.Info "pgrst_schema_cache_query_time_seconds" "The query time in seconds of the last schema cache load")
  Prom.setGauge poolMaxSize (fromIntegral configDbPoolSize)
  pure $ MetricsState poolTimeouts poolAvailable poolWaiting poolMaxSize schemaCacheLoads schemaCacheQueryTime

observationMetrics :: MetricsState -> ObservationHandler
observationMetrics (MetricsState poolTimeouts poolAvailable poolWaiting _ schemaCacheLoads schemaCacheQueryTime) obs = case obs of
  (PoolAcqTimeoutObs _) -> do
    Prom.incCounter poolTimeouts
  (HasqlPoolObs (SQL.ConnectionObservation _ status)) -> case status of
     SQL.ReadyForUseConnectionStatus  -> do
      Prom.incGauge poolAvailable
     SQL.InUseConnectionStatus        -> do
      Prom.decGauge poolAvailable
     SQL.TerminatedConnectionStatus  _ -> do
      Prom.decGauge poolAvailable
     SQL.ConnectingConnectionStatus -> pure ()
  PoolRequest ->
    Prom.incGauge poolWaiting
  PoolRequestFullfilled ->
    Prom.decGauge poolWaiting
  SchemaCacheLoadedObs resTime -> do
    Prom.incCounter schemaCacheLoads
    Prom.setGauge schemaCacheQueryTime resTime
  _ ->
    pure ()

metricsToText :: IO LBS.ByteString
metricsToText = Prom.exportMetricsAsText
