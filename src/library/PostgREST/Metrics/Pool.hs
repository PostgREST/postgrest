{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PostgREST.Metrics.Pool
-- Description : Metrics of the connection pool.
module PostgREST.Metrics.Pool
  ( PoolMetrics (..)
  , ConnTrack
  , ConnStats (..)
  , connectionCounts
  , init
  )
where

import Control.Arrow ((&&&))
import Data.Bitraversable (bisequenceA)
import Data.Tuple.Extra (both)
import Data.UUID (UUID)
import Prometheus
import Protolude

import Focus qualified
import StmHamt.SizedHamt qualified as SH

import PostgREST.Metrics.Types (MetricsModule, initMetricModule)
import PostgREST.Observation

import Hasql.Pool.Observation qualified as SQL

data PoolMetrics = PoolMetrics
  { poolTimeouts :: Counter,
    connTrack :: ConnTrack,
    poolWaiting :: Gauge,
    poolWaitTimeSeconds :: Histogram,
    poolMaxSize :: Gauge
  }

init :: Int -> Int -> IO (PoolMetrics, MetricsModule)
init configDbPoolSize configDbPoolAcquisitionTimeout =
  initMetricModule
    ( \create -> do
        metrics <-
          PoolMetrics
            <$> create (counter (Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts"))
            <*> create (Metric ((identity &&& dbPoolAvailable) <$> connectionTracker))
            <*> create (gauge (Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection"))
            <*> create (histogram (Info "pgrst_db_pool_wait_time_seconds" "Time waiting to acquire contended pool connections (includes timeouts)") connectionWaitTimeBuckets)
            <*> create (gauge (Info "pgrst_db_pool_max" "Max pool connections"))
        setGauge (poolMaxSize metrics) (fromIntegral configDbPoolSize)
        pure metrics
    )
    onObservation
  where
    maxWaitTime = max 0.1 $ fromIntegral configDbPoolAcquisitionTimeout
    numberOfBuckets = 12
    initialBucket = 0.001
    connectionWaitTimeBuckets = linearBuckets initialBucket ((maxWaitTime - initialBucket) / fromIntegral (numberOfBuckets - 1)) numberOfBuckets

    dbPoolAvailable = (pure . noLabelsGroup (Info "pgrst_db_pool_available" "Available connections in the pool") GaugeType . calcAvailable <$>) . connectionCounts
      where
        calcAvailable = liftA2 (-) connected inUse
    toSample name labels = Sample name labels . encodeUtf8 . show
    noLabelsGroup info sampleType = SampleGroup info sampleType . pure . toSample (metricName info) mempty

    onObservation PoolMetrics{..} = \case
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
      HasqlPoolObs (SQL.ConnectionObservation uuid status) -> trackConnections connTrack uuid status
      HasqlPoolObs (SQL.ConnectionWaitCompletedStatus ms) -> void $ observe poolWaitTimeSeconds $ ms / 1000
      HasqlPoolObs SQL.PoolRequest -> incGauge poolWaiting
      HasqlPoolObs SQL.PoolRequestFullfilled -> decGauge poolWaiting
      _ ->
        pure ()

data ConnStats = ConnStats
  { connected :: Int,
    inUse :: Int
  }
  deriving (Eq, Show)

data ConnTrack = ConnTrack {connTrackConnected :: SH.SizedHamt UUID, connTrackInUse :: SH.SizedHamt UUID}

connectionTracker :: IO ConnTrack
connectionTracker = ConnTrack <$> SH.newIO <*> SH.newIO

trackConnections :: ConnTrack -> UUID -> SQL.ConnectionStatus -> IO ()
trackConnections ConnTrack{..} uuid status = case status of
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
