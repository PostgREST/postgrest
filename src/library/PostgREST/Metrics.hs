-- |
-- Module      : PostgREST.Metrics
-- Description : Metrics based on the Observation module. See Observation.hs.
--
-- The metrics of each concern live in a module of their own (see
-- 'MetricsModule'); this module composes those of an AppState and registers
-- the metrics of the process.
module PostgREST.Metrics
  ( init
  , MetricsState (..)
  , observationMetrics
  , metricsSamples
  , registerMetrics
  , metricsToText
  )
where

import GHC.Stats (getRTSStatsEnabled)
import Prometheus
import Protolude

import Data.ByteString.Lazy qualified as LBS
import Prometheus.Metric.GHC qualified as PMG

import PostgREST.Metrics.Types (MetricsModule (..))
import PostgREST.Observation

import PostgREST.Metrics.JwtCache qualified as JwtCache
import PostgREST.Metrics.Pool qualified as Pool
import PostgREST.Metrics.SchemaCache qualified as SchemaCache

-- | The metrics of an AppState
data MetricsState
  = MetricsState
  { poolMetrics :: Pool.PoolMetrics
  , schemaCacheMetrics :: SchemaCache.SchemaCacheMetrics
  , jwtCacheMetrics :: JwtCache.JwtCacheMetrics
  , metricsModule :: MetricsModule
  -- ^ All of the metrics above
  }

-- | Create the metrics. They are not registered, so several can be created in
-- a process; they are exported once their samples are registered, see
-- 'registerMetrics'.
init :: Int -> IO MetricsState
init configDbPoolSize = do
  (pool, poolModule) <- Pool.init configDbPoolSize
  (schemaCache, schemaCacheModule) <- SchemaCache.init
  (jwtCache, jwtCacheModule) <- JwtCache.init
  pure . MetricsState pool schemaCache jwtCache $
    mconcat
      [ poolModule
      , schemaCacheModule
      , jwtCacheModule
      ]

-- Only some observations are used as metrics
observationMetrics :: MetricsState -> ObservationHandler
observationMetrics = moduleObserve . metricsModule

-- | The current samples of the metrics
metricsSamples :: MetricsState -> IO [SampleGroup]
metricsSamples = moduleSamples . metricsModule

-- | Register the GHC runtime metrics and the given samples (e.g. of an
-- AppState's metrics), to be exported by 'metricsToText'. Called once per
-- process.
registerMetrics :: IO [SampleGroup] -> IO ()
registerMetrics samples = do
  whenM getRTSStatsEnabled $ void $ register PMG.ghcMetrics
  void . register $ Metric (pure ((), samples))

metricsToText :: IO LBS.ByteString
metricsToText = exportMetricsAsText
