{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PostgREST.Logger
-- Description : Metrics based on the Observation module. See Observation.hs.
module PostgREST.Metrics
  ( init
  , ConnTrack
  , ConnStats (..)
  , MetricsState (..)
  , connectionCounts
  , observationMetrics
  , registerMetrics
  , metricsToText
  )
where

import Control.Arrow ((&&&))
import Data.Bitraversable (bisequenceA)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Tuple.Extra (both)
import Data.UUID (UUID)
import GHC.Stats (getRTSStatsEnabled)
import Prometheus
import Protolude

import Data.ByteString.Lazy qualified as LBS
import Focus qualified
import Prometheus.Metric.GHC qualified as PMG
import StmHamt.SizedHamt qualified as SH

import PostgREST.Observation

import Hasql.Pool.Observation qualified as SQL

data MetricsState
  = MetricsState
  { poolTimeouts :: Counter
  , connTrack :: ConnTrack
  , poolWaiting :: Gauge
  , poolMaxSize :: Gauge
  , schemaCacheLoads :: Vector Label1 Counter
  , schemaCacheQueryTime :: Gauge
  , jwtCacheRequests :: Counter
  , jwtCacheHits :: Counter
  , jwtCacheEvictions :: Counter
  , metricsSamples :: IO [SampleGroup]
  -- ^ The current samples of the metrics above
  }

-- | Create the metrics. They are not registered, so several can be created in
-- a process; they are exported once their samples are registered, see
-- 'registerMetrics'.
init :: Int -> IO MetricsState
init configDbPoolSize = do
  samplers <- newIORef []
  let
    create :: Metric s -> IO s
    create metric = do
      (value, sample) <- construct metric
      modifyIORef' samplers (<> [sample])
      pure value
  metricState <-
    MetricsState
      <$> create (counter (Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts"))
      <*> create (Metric ((identity &&& dbPoolAvailable) <$> connectionTracker))
      <*> create (gauge (Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection"))
      <*> create (gauge (Info "pgrst_db_pool_max" "Max pool connections"))
      <*> create (vector "status" $ counter (Info "pgrst_schema_cache_loads_total" "The total number of times the schema cache was loaded"))
      <*> create (gauge (Info "pgrst_schema_cache_query_time_seconds" "The query time in seconds of the last schema cache load"))
      <*> create (counter (Info "pgrst_jwt_cache_requests_total" "The total number of JWT cache lookups"))
      <*> create (counter (Info "pgrst_jwt_cache_hits_total" "The total number of JWT cache hits"))
      <*> create (counter (Info "pgrst_jwt_cache_evictions_total" "The total number of JWT cache evictions"))
      <*> (fmap concat . sequence <$> readIORef samplers)
  setGauge (poolMaxSize metricState) (fromIntegral configDbPoolSize)
  pure metricState
  where
    dbPoolAvailable = (pure . noLabelsGroup (Info "pgrst_db_pool_available" "Available connections in the pool") GaugeType . calcAvailable <$>) . connectionCounts
      where
        calcAvailable = liftA2 (-) connected inUse
    toSample name labels = Sample name labels . encodeUtf8 . show
    noLabelsGroup info sampleType = SampleGroup info sampleType . pure . toSample (metricName info) mempty

-- Only some observations are used as metrics
observationMetrics :: MetricsState -> ObservationHandler
observationMetrics MetricsState{..} obs = case obs of
  PoolAcqTimeoutObs -> do
    incCounter poolTimeouts
  -- Handle pool observations with connection tracking
  -- this is necessary because it is not possible
  -- to accurately maintain open/in use conneciton counts
  -- statelessly based only on pool observation events.
  -- The reason is that hasql-pool emits TerminatedConnectionStatus
  -- both for connections successfully established and failed when connecting.
  -- When receiving TerminatedConnectionStatus we have to find out
  -- if we can decrement established connection count. To do that we have to track
  -- established connections.
  (HasqlPoolObs sqlObs) -> trackConnections connTrack sqlObs
  PoolRequest ->
    incGauge poolWaiting
  PoolRequestFullfilled ->
    decGauge poolWaiting
  SchemaCacheLoadedObs resTime _ -> do
    withLabel schemaCacheLoads "SUCCESS" incCounter
    setGauge schemaCacheQueryTime resTime
  SchemaCacheErrorObs{} -> do
    withLabel schemaCacheLoads "FAIL" incCounter
  JwtCacheLookup True -> incCounter jwtCacheRequests *> incCounter jwtCacheHits
  JwtCacheLookup False -> incCounter jwtCacheRequests
  JwtCacheEviction -> incCounter jwtCacheEvictions
  _ ->
    pure ()

-- | Register the GHC runtime metrics and the given samples (e.g. of an
-- AppState's metrics), to be exported by 'metricsToText'. Called once per
-- process.
registerMetrics :: IO [SampleGroup] -> IO ()
registerMetrics samples = do
  whenM getRTSStatsEnabled $ void $ register PMG.ghcMetrics
  void . register $ Metric (pure ((), samples))

metricsToText :: IO LBS.ByteString
metricsToText = exportMetricsAsText

data ConnStats = ConnStats
  { connected :: Int
  , inUse :: Int
  }
  deriving (Eq, Show)

data ConnTrack = ConnTrack {connTrackConnected :: SH.SizedHamt UUID, connTrackInUse :: SH.SizedHamt UUID}

connectionTracker :: IO ConnTrack
connectionTracker = ConnTrack <$> SH.newIO <*> SH.newIO

trackConnections :: ConnTrack -> SQL.Observation -> IO ()
trackConnections ConnTrack{..} (SQL.ConnectionObservation uuid status) = case status of
  SQL.ReadyForUseConnectionStatus _ ->
    atomically $
      SH.insert identity uuid connTrackConnected
        *> SH.focus Focus.delete identity uuid connTrackInUse
  SQL.TerminatedConnectionStatus _ ->
    atomically $
      SH.focus Focus.delete identity uuid connTrackConnected
        *> SH.focus Focus.delete identity uuid connTrackInUse
  SQL.InUseConnectionStatus ->
    atomically $
      SH.insert identity uuid connTrackInUse
  _ -> mempty

connectionCounts :: ConnTrack -> IO ConnStats
connectionCounts = atomically . fmap (uncurry ConnStats) . bisequenceA . both SH.size . (connTrackConnected &&& connTrackInUse)
