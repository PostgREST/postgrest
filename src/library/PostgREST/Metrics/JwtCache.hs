{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PostgREST.Metrics.JwtCache
-- Description : Metrics of the JWT cache.
module PostgREST.Metrics.JwtCache
  ( JwtCacheMetrics (..)
  , init
  )
where

import Prometheus
import Protolude

import PostgREST.Metrics.Types (MetricsModule, initMetricModule)
import PostgREST.Observation
import PostgREST.Observation.JwtCache (JwtCacheObservation (..))

data JwtCacheMetrics = JwtCacheMetrics
  { jwtCacheRequests :: Counter,
    jwtCacheHits :: Counter,
    jwtCacheEvictions :: Counter
  }

init :: IO (JwtCacheMetrics, MetricsModule)
init =
  initMetricModule
    ( \create ->
        JwtCacheMetrics
          <$> create (counter (Info "pgrst_jwt_cache_requests_total" "The total number of JWT cache lookups"))
          <*> create (counter (Info "pgrst_jwt_cache_hits_total" "The total number of JWT cache hits"))
          <*> create (counter (Info "pgrst_jwt_cache_evictions_total" "The total number of JWT cache evictions"))
    )
    onObservation
  where
    onObservation JwtCacheMetrics{..} = \case
      JwtCacheObs (JwtCacheLookup True) -> incCounter jwtCacheRequests *> incCounter jwtCacheHits
      JwtCacheObs (JwtCacheLookup False) -> incCounter jwtCacheRequests
      JwtCacheObs JwtCacheEviction -> incCounter jwtCacheEvictions
      _ -> pure ()
