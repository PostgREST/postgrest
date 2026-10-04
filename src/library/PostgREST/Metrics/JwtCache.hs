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

import PostgREST.Metrics.Types (MetricsModule (..), collect)
import PostgREST.Observation

data JwtCacheMetrics = JwtCacheMetrics
  { jwtCacheRequests :: Counter
  , jwtCacheHits :: Counter
  , jwtCacheEvictions :: Counter
  }

init :: IO (JwtCacheMetrics, MetricsModule)
init = do
  (metrics, samples) <- collect $ \create ->
    JwtCacheMetrics
      <$> create (counter (Info "pgrst_jwt_cache_requests_total" "The total number of JWT cache lookups"))
      <*> create (counter (Info "pgrst_jwt_cache_hits_total" "The total number of JWT cache hits"))
      <*> create (counter (Info "pgrst_jwt_cache_evictions_total" "The total number of JWT cache evictions"))
  pure (metrics, MetricsModule (onObservation metrics) samples)
  where
    onObservation JwtCacheMetrics{..} = \case
      JwtCacheLookup True -> incCounter jwtCacheRequests *> incCounter jwtCacheHits
      JwtCacheLookup False -> incCounter jwtCacheRequests
      JwtCacheEviction -> incCounter jwtCacheEvictions
      _ -> pure ()
