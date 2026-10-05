{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : PostgREST.Metrics.Types
-- Description : The metrics of a concern, composed with those of others.
module PostgREST.Metrics.Types
  ( MetricsModule (..)
  , collect
  )
where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Prometheus
import Protolude

import PostgREST.Observation (ObservationHandler)

-- | The metrics of a concern: how they react to observations and their
-- current samples. Modules are composed with '<>'.
data MetricsModule = MetricsModule
  { moduleObserve :: ObservationHandler
  , moduleSamples :: IO [SampleGroup]
  }

instance Semigroup MetricsModule where
  MetricsModule observe1 samples1 <> MetricsModule observe2 samples2 =
    MetricsModule (observe1 <> observe2) ((<>) <$> samples1 <*> samples2)

instance Monoid MetricsModule where
  mempty = MetricsModule mempty (pure [])

-- | Create metrics with the given function, without registering them, so
-- several can be created in a process. Returns them with their samples.
collect :: ((forall s. Metric s -> IO s) -> IO a) -> IO (a, IO [SampleGroup])
collect build = do
  samplers <- newIORef []
  metrics <- build $ \metric -> do
    (value, sample) <- construct metric
    modifyIORef' samplers (<> [sample])
    pure value
  (metrics,) . fmap concat . sequence <$> readIORef samplers
