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

data MetricsState = MetricsState
  { msPoolTimeouts      :: Prom.Counter
  , msPoolCreated       :: Prom.Counter
  , msPoolAvailable     :: Prom.Gauge
  , msPoolWaiting       :: Prom.Gauge
  , msPoolUsed          :: Prom.Gauge
  , msPoolMaxSize       :: Prom.Gauge
  , msSchCacheLoads     :: Prom.Counter
  , msSchCacheQueryTime :: Prom.Gauge
  }

init :: Int -> IO MetricsState
init configDbPoolSize = do
  poolTimeouts <- Prom.register $ Prom.counter (Prom.Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts")
  poolCreated <- Prom.register $ Prom.counter (Prom.Info "pgrst_db_pool_created" "The total number of created pool connections")
  poolAvailable <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_available" "Available connections in the pool")
  poolWaiting <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection")
  poolUsed <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_used" "Used connections in the pool")
  poolMaxSize <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_max" "Max pool connections")
  schemaCacheLoads <- Prom.register $ Prom.counter (Prom.Info "pgrst_schema_cache_loads_total" "The total number of times the schema cache was loaded")
  schemaCacheQueryTime <- Prom.register $ Prom.gauge (Prom.Info "pgrst_schema_cache_query_time_seconds" "The query time in seconds of the last schema cache load")
  Prom.setGauge poolMaxSize (fromIntegral configDbPoolSize)
  pure $ MetricsState poolTimeouts poolCreated poolAvailable poolWaiting poolUsed poolMaxSize schemaCacheLoads schemaCacheQueryTime

observationMetrics :: MetricsState -> ObservationHandler
observationMetrics (MetricsState poolTimeouts poolCreated poolAvailable poolWaiting poolUsed _ schemaCacheLoads schemaCacheQueryTime) obs = case obs of
  (PoolAcqTimeoutObs _) -> do
    Prom.incCounter poolTimeouts
  (HasqlPoolObs (SQL.ConnectionObservation _ status)) -> case status of
    SQL.ReadyForUseConnectionStatus reason -> case reason of
      SQL.EstablishedConnectionReadyForUseReason -> do
        Prom.incCounter poolCreated
        Prom.incGauge poolAvailable
      _ -> do
        Prom.decGauge poolUsed
        Prom.incGauge poolAvailable
    SQL.InUseConnectionStatus -> do
      Prom.decGauge poolAvailable
      Prom.incGauge poolUsed
    SQL.TerminatedConnectionStatus _ ->
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
