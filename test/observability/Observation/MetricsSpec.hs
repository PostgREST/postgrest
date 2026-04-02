{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TypeApplications    #-}

module Observation.MetricsSpec where

import           Data.List             (lookup)
import           Network.Wai           (Application)
import           ObsHelper
import qualified PostgREST.AppState    as AppState
import           PostgREST.Config      (AppConfig (configDbSchemas))
import qualified PostgREST.Metrics     as Metrics
import           PostgREST.Observation
import           Prometheus            (getCounter, getVectorWith)
import           Protolude
import           Test.Hspec            (SpecWith, describe, it)
import           Test.Hspec.Wai        (getState)

spec :: SpecWith (SpecState, Application)
spec = describe "Server started with metrics enabled" $ do
  it "Should update pgrst_schema_cache_loads_total[SUCCESS]" $ do
    SpecState{specAppState = appState, specMetrics = metrics, specObsChan} <- getState
    let waitFor = waitForObs specObsChan

    liftIO $ checkState' metrics [
        schemaCacheLoads "SUCCESS" (+1)
      ] $ do
        AppState.schemaCacheLoader appState
        waitFor (1 * sec) "SchemaCacheLoadedObs" $ \x -> [ o | o@(SchemaCacheLoadedObs{}) <- pure x]

  it "Should update pgrst_schema_cache_loads_total[ERROR]" $ do
    SpecState{specAppState = appState, specMetrics = metrics, specObsChan} <- getState
    let waitFor = waitForObs specObsChan

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

  it "Should debounce schema cache loads" $ do
    SpecState{specAppState = appState, specMetrics = metrics, specObsChan} <- getState
    let waitFor = waitForObs specObsChan

    liftIO $ checkState' metrics [
        -- we expect exactly 2 successful schema cache loads
        schemaCacheLoads "FAIL" (+0),
        schemaCacheLoads "SUCCESS" (+2)
      ] $ do
        AppState.schemaCacheLoader appState
        -- at this moment there is no dedicated observation emited
        -- when schema cache load starts
        -- so we wait for DBConnectedObs which is emited right after successful
        -- PostgreSQL version query during schema cache loading
        waitFor (1 * sec) "DBConnectedObs" $ \x -> [ o | o@(DBConnectedObs{}) <- pure x]
        -- request schema cache load multiple times
        -- all of them should be handled by a single schema cache load
        replicateM_ 100 (AppState.schemaCacheLoader appState)
        -- wait for two expected SchemaCacheLoadedObs events
        replicateM_ 2 $ waitFor (1 * sec) "SchemaCacheLoadedObs" $ \x -> [ o | o@(SchemaCacheLoadedObs{}) <- pure x]
        -- wait 1 sec to make sure we capture all potential schema cache loads
        -- (there should be none but we need to verify that)
        threadDelay $ 1 * sec


  where
    -- prometheus-client api to handle vectors is convoluted
    schemaCacheLoads label = expectField @"schemaCacheLoads" $
      fmap (maybe (0::Int) round . lookup label) . (`getVectorWith` getCounter)
    sec = 1000000
