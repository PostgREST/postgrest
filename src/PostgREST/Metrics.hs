{-# LANGUAGE RecordWildCards #-}
{-|
Module      : PostgREST.Logger
Description : Metrics based on the Observation module. See Observation.hs.
-}
module PostgREST.Metrics
  ( init
  , ConnTrack
  , ConnStats (..)
  , MetricsState (..)
  , connectionCounts
  , observationMetrics
  , metricsToText
  ) where

import qualified Data.ByteString.Lazy   as LBS
import qualified Hasql.Pool.Observation as SQL

import Prometheus

import PostgREST.Observation

import           Control.Arrow      ((&&&))
import           Data.Bitraversable (bisequenceA)
import           Data.Tuple.Extra   (both)
import           Data.UUID          (UUID)
import qualified Focus
import           Protolude
import qualified StmHamt.SizedHamt  as SH

data MetricsState =
  MetricsState {
    poolTimeouts         :: Counter,
    connTrack            :: ConnTrack,
    poolWaiting          :: Gauge,
    poolMaxSize          :: Gauge,
    schemaCacheLoads     :: Vector Label1 Counter,
    schemaCacheQueryTime :: Gauge,
    jwtCacheRequests     :: Counter,
    jwtCacheHits         :: Counter,
    jwtCacheEvictions    :: Counter
  }

init :: Int -> IO MetricsState
init configDbPoolSize = do
  metricState <- MetricsState <$>
    register (counter (Info "pgrst_db_pool_timeouts_total" "The total number of pool connection timeouts")) <*>
    register (Metric ((identity &&& dbPoolAvailable) <$> connectionTracker)) <*>
    register (gauge (Info "pgrst_db_pool_waiting" "Requests waiting to acquire a pool connection")) <*>
    register (gauge (Info "pgrst_db_pool_max" "Max pool connections")) <*>
    register (vector "status" $ counter (Info "pgrst_schema_cache_loads_total" "The total number of times the schema cache was loaded")) <*>
    register (gauge (Info "pgrst_schema_cache_query_time_seconds" "The query time in seconds of the last schema cache load")) <*>
    register (counter (Info "pgrst_jwt_cache_requests_total" "The total number of JWT cache lookups")) <*>
    register (counter (Info "pgrst_jwt_cache_hits_total" "The total number of JWT cache hits")) <*>
    register (counter (Info "pgrst_jwt_cache_evictions_total" "The total number of JWT cache evictions"))
  setGauge (poolMaxSize metricState) (fromIntegral configDbPoolSize)
  pure metricState
  where
    dbPoolAvailable = (pure . noLabelsGroup (Info "pgrst_db_pool_available" "Available connections in the pool") GaugeType . calcAvailable <$>) . connectionCounts
      where
        calcAvailable = (configDbPoolSize -) . inUse
    toSample name labels = Sample name labels . encodeUtf8 . show
    noLabelsGroup info sampleType = SampleGroup info sampleType . pure . toSample (metricName info) mempty

-- Only some observations are used as metrics
observationMetrics :: MetricsState -> ObservationHandler
observationMetrics MetricsState{..} obs = case obs of
  PoolAcqTimeoutObs -> do
    incCounter poolTimeouts
  (HasqlPoolObs sqlObs) -> trackConnections connTrack sqlObs
  PoolRequest ->
    incGauge poolWaiting
  PoolRequestFullfilled ->
    decGauge poolWaiting
  SchemaCacheLoadedObs resTime -> do
    withLabel schemaCacheLoads "SUCCESS" incCounter
    setGauge schemaCacheQueryTime resTime
  SchemaCacheErrorObs{} -> do
    withLabel schemaCacheLoads "FAIL" incCounter
  JwtCacheLookup True -> incCounter jwtCacheRequests *> incCounter jwtCacheHits
  JwtCacheLookup False -> incCounter jwtCacheRequests
  JwtCacheEviction -> incCounter jwtCacheEvictions
  _ ->
    pure ()

metricsToText :: IO LBS.ByteString
metricsToText = exportMetricsAsText

data ConnStats = ConnStats {
    connected :: Int,
    inUse     :: Int
} deriving (Eq, Show)

data ConnTrack = ConnTrack { connTrackConnected :: SH.SizedHamt UUID, connTrackInUse :: SH.SizedHamt UUID }

connectionTracker :: IO ConnTrack
connectionTracker =  ConnTrack <$> SH.newIO <*> SH.newIO

trackConnections :: ConnTrack -> SQL.Observation -> IO ()
trackConnections ConnTrack{..} (SQL.ConnectionObservation uuid status) = case status of
  SQL.ReadyForUseConnectionStatus -> atomically $
    SH.insert identity uuid connTrackConnected *>
    SH.focus Focus.delete identity uuid connTrackInUse
  SQL.TerminatedConnectionStatus _ -> atomically $
    SH.focus Focus.delete identity uuid connTrackConnected *>
    SH.focus Focus.delete identity uuid connTrackInUse
  SQL.InUseConnectionStatus -> atomically $
    SH.insert identity uuid connTrackInUse
  _ -> mempty

connectionCounts :: ConnTrack -> IO ConnStats
connectionCounts = atomically . fmap (uncurry ConnStats) . bisequenceA . both SH.size . (connTrackConnected &&& connTrackInUse)
