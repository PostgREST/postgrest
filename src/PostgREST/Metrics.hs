{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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
  { poolTimeouts         :: Prom.Counter
  , poolAvailable        :: Prom.Gauge
  , poolWaiting          :: Prom.Gauge
  , poolMaxSize          :: Prom.Gauge
  }

init :: Int -> IO MetricsState
init poolMaxSize = do
  timeouts <- Prom.register $ Prom.counter (Prom.Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts")
  available <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_available" "Available connections in the pool")
  waiting <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection")
  maxSize <- Prom.register $ Prom.gauge (Prom.Info "pgrst_db_pool_max" "Max pool connections")
  Prom.setGauge maxSize (fromIntegral poolMaxSize)
  pure $ MetricsState timeouts available waiting maxSize

observationMetrics :: MetricsState -> ObservationHandler
observationMetrics MetricsState{poolTimeouts, poolAvailable, poolWaiting} obs = case obs of
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
  _ ->
    pure ()

metricsToText :: IO LBS.ByteString
metricsToText = Prom.exportMetricsAsText
