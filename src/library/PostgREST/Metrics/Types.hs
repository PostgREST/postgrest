{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : PostgREST.Metrics.Types
-- Description : The metrics of a concern, composed with those of others.
module PostgREST.Metrics.Types
  ( MetricsModule (..)
  , initMetricModule
  )
where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Prometheus
import Protolude

import PostgREST.Observation (ObservationHandler)

-- | The metrics of a concern: how they react to observations and their
-- current samples. Modules are composed with '<>'.
data MetricsModule = MetricsModule
  { moduleObserve :: ObservationHandler,
    moduleSamples :: IO [SampleGroup]
  }

instance Semigroup MetricsModule where
  MetricsModule observe1 samples1 <> MetricsModule observe2 samples2 =
    MetricsModule (observe1 <> observe2) ((<>) <$> samples1 <*> samples2)

instance Monoid MetricsModule where
  mempty = MetricsModule mempty (pure [])

type CreateMetric = forall s. Metric s -> IO s

-- | Create metrics with the given function, without registering them, so
-- several can be created in a process. Returns them with their samples.
collect :: (CreateMetric -> IO a) -> IO (a, IO [SampleGroup])
collect build = do
  samplersRef <- newIORef []

  let
    createMetric :: CreateMetric
    createMetric metric = do
      (value, sampler) <- construct metric
      modifyIORef' samplersRef (<> [sampler])
      pure value

  metrics <- build createMetric
  samplers <- readIORef samplersRef

  let sampleAll = concat <$> sequence samplers
  pure (metrics, sampleAll)

-- | Create metrics and MetricsModule without registering them, so
-- several can be created in a process. Returns metrics with their MetricsModule
initMetricModule :: (CreateMetric -> IO a) -> (a -> ObservationHandler) -> IO (a, MetricsModule)
initMetricModule build onObservation = do
  (metrics, samples) <- collect build
  pure (metrics, MetricsModule (onObservation metrics) samples)
