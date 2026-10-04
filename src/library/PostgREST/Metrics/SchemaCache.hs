{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PostgREST.Metrics.SchemaCache
-- Description : Metrics of the schema cache loads.
module PostgREST.Metrics.SchemaCache
  ( SchemaCacheMetrics (..)
  , init
  )
where

import Prometheus
import Protolude

import PostgREST.Metrics.Types (MetricsModule (..), collect)
import PostgREST.Observation

data SchemaCacheMetrics = SchemaCacheMetrics
  { schemaCacheLoads :: Vector Label1 Counter
  , schemaCacheQueryTime :: Gauge
  }

init :: IO (SchemaCacheMetrics, MetricsModule)
init = do
  (metrics, samples) <- collect $ \create ->
    SchemaCacheMetrics
      <$> create (vector "status" $ counter (Info "pgrst_schema_cache_loads_total" "The total number of times the schema cache was loaded"))
      <*> create (gauge (Info "pgrst_schema_cache_query_time_seconds" "The query time in seconds of the last schema cache load"))
  pure (metrics, MetricsModule (onObservation metrics) samples)
  where
    onObservation SchemaCacheMetrics{..} = \case
      SchemaCacheLoadedObs resTime _ -> do
        withLabel schemaCacheLoads "SUCCESS" incCounter
        setGauge schemaCacheQueryTime resTime
      SchemaCacheErrorObs{} -> do
        withLabel schemaCacheLoads "FAIL" incCounter
      _ -> pure ()
