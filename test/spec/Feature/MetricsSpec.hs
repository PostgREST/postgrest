{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeApplications    #-}

module Feature.MetricsSpec where

import qualified Hasql.Pool.Observation as SQL
import           Network.Wai            (Application)
import qualified PostgREST.AppState     as AppState
import           PostgREST.Config       (AppConfig (configDbSchemas))
import           PostgREST.Metrics      (ConnStats (..),
                                         connectionCounts)
import qualified PostgREST.Metrics      as Metrics
import           PostgREST.Observation
import           Prometheus             (getCounter, getVectorWith)
import           Protolude
import           SpecHelper
import           Test.Hspec             (SpecWith, describe, it)

spec :: SpecWith (((AppState.AppState, Metrics.MetricsState), Chan Observation), Application)
spec = describe "Server started with metrics enabled" $ do
  it "Should update pgrst_schema_cache_loads_total[SUCCESS]" $ do
    ((appState, metrics), waitFor) <- prepareState

    liftIO $ checkState' metrics [
        schemaCacheLoads "SUCCESS" (+1)
      ] $ do
        AppState.schemaCacheLoader appState
        waitFor (1 * sec) "SchemaCacheLoadedObs" $ \x -> [ o | o@(SchemaCacheLoadedObs{}) <- pure x]

  it "Should update pgrst_schema_cache_loads_total[ERROR]" $ do
    ((appState, metrics), waitFor) <- prepareState

    liftIO $ checkState' metrics [
        schemaCacheLoads "FAIL" (+1),
        schemaCacheLoads "SUCCESS" (+1)
      ] $ do
        AppState.getConfig appState >>= \prev -> do
          AppState.putConfig appState $ prev { configDbSchemas = pure "bad_schema" }
          AppState.schemaCacheLoader appState
          waitFor (1 * sec) "SchemaCacheErrorObs" $ \x -> [ o | o@(SchemaCacheErrorObs{}) <- pure x]
          AppState.putConfig appState prev

        -- wait up to 2 secs so that retry can happen
        waitFor (2 * sec) "SchemaCacheLoadedObs" $ \x -> [ o | o@(SchemaCacheLoadedObs{}) <- pure x]

  it "Should track in use connections" $ do
    ((appState, metrics), waitFor) <- prepareState

    -- we expect in use connections to be the same once finished
    liftIO $ checkState' metrics
        [
            inUseConnections (+ 0)
        ] $ do
          signal <- newEmptyMVar
          -- make sure waiting thread is signaled
          bracket_ (pure ()) (putMVar signal ()) $
            -- expecting one more connection in use
            checkState' metrics
            [
                inUseConnections (+ 1)
            ] $ do
                -- start a thread hanging on a single connection until signaled
                void $ forkIO $ void $ AppState.usePool appState $ liftIO (readMVar signal)
                -- main thread waits for ConnectionObservation with InUseConnectionStatus
                -- after which used connections count should be incremented
                waitFor (1 * sec) "InUseConnectionStatus" $ \x -> [o | o@(HasqlPoolObs (SQL.ConnectionObservation _ SQL.InUseConnectionStatus)) <- pure x]

          -- hanging thread was signaled and should return the connection
          waitFor (1 * sec) "ReadyForUseConnectionStatus" $ \x -> [o | o@(HasqlPoolObs (SQL.ConnectionObservation _ SQL.ReadyForUseConnectionStatus)) <- pure x]
  where
    -- prometheus-client api to handle vectors is convoluted
    schemaCacheLoads label = expectField @"schemaCacheLoads" $
      (foldMap (Sum . round @Double @Int . snd) . find ((== label) . fst) <$>) . (`getVectorWith` getCounter)

    inUseConnections = expectField @"connTrack" ((inUse <$>) . connectionCounts)
    sec = 1000000
