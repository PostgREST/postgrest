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

import PostgREST.Metrics.Types (MetricsModule (..), collect)
import PostgREST.Observation

import Hasql.Pool.Observation qualified as SQL

data PoolMetrics = PoolMetrics
  { poolTimeouts :: Counter,
    connTrack :: ConnTrack,
    poolWaiting :: Gauge,
    poolMaxSize :: Gauge,
    poolWait :: Histogram
  }

init :: Int -> IO (PoolMetrics, MetricsModule)
init configDbPoolSize = do
  (metrics, samples) <- collect $ \create ->
    PoolMetrics
      <$> create (counter (Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts"))
      <*> create (Metric ((identity &&& dbPoolAvailable) <$> connectionTracker))
      <*> create (gauge (Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection"))
      <*> create (gauge (Info "pgrst_db_pool_max" "Max pool connections"))
      <*> create (histogram (Info "pgrst_db_pool_wait_seconds" "The wait for a pool connection") poolWaitBuckets)
  setGauge (poolMaxSize metrics) (fromIntegral configDbPoolSize)
  pure (metrics, MetricsModule (onObservation metrics) samples)
  where
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
      (HasqlPoolObs sqlObs) -> trackConnections connTrack sqlObs
      PoolRequest ->
        incGauge poolWaiting
      PoolRequestFullfilled ->
        decGauge poolWaiting
      PoolWaitedObs seconds ->
        observe poolWait seconds
      _ ->
        pure ()

-- | From a millisecond to the default db-pool-acquisition-timeout, 10 s
poolWaitBuckets :: [Double]
poolWaitBuckets = [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10]

data ConnStats = ConnStats
  { connected :: Int,
    inUse :: Int
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
