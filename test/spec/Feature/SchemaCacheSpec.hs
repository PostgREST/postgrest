{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MonadComprehensions #-}
module Feature.SchemaCacheSpec

where

import Network.Wai (Application)

import qualified PostgREST.AppState    as AppState
import           PostgREST.Config      (configDbSchemas)
import qualified PostgREST.Metrics     as Metrics
import           PostgREST.Observation
import           Protolude
import           SpecHelper
import           Test.Hspec            (SpecWith, describe, it)

spec :: SpecWith (((AppState.AppState, Metrics.MetricsState), Chan Observation), Application)
spec = describe "Server started with metrics enabled" $ do

  it "Should emit PoolFlushed, SchemaCacheQueriedObs and SchemaCacheLoadedObs when schema cache is reloaded" $ do
    ((appState, _), waitFor) <- prepareState

    liftIO $ do
      AppState.schemaCacheLoader appState

      waitFor (1 * sec) "PoolFlushed" $ \x -> [ o | o@PoolFlushed <- pure x ]
      waitFor (1 * sec) "SchemaCacheQueriedObs" $ \x -> [ o | o@SchemaCacheQueriedObs{} <- pure x ]
      waitFor (1 * sec) "SchemaCacheLoadedObs" $ \x -> [ o | o@SchemaCacheLoadedObs{} <- pure x ]


  it "Should flush pool multiple times when schema reloading retries" $ do
    ((appState, _), waitFor) <- prepareState

    liftIO $ do
        AppState.getConfig appState >>= \cfg -> do
          AppState.putConfig appState $ cfg { configDbSchemas = pure "bad_schema" }
          AppState.schemaCacheLoader appState

          waitFor (1 * sec) "PoolFlushed 1" $ \x -> [ o | o@PoolFlushed <- pure x ]
          waitFor (1 * sec) "SchemaCacheErrorObs" $ \x -> [ o | o@SchemaCacheErrorObs{} <- pure x ]

          -- Restore configuration
          AppState.putConfig appState cfg

        -- Wait for 2 seconds so that retry can happen
        waitFor (2 * sec) "PoolFlushed 2" $ \x -> [ o | o@PoolFlushed <- pure x ]
        waitFor (1 * sec) "SchemaCacheQueriedObs" $ \x -> [ o | o@SchemaCacheQueriedObs{} <- pure x ]
        waitFor (1 * sec) "SchemaCacheLoadedObs" $ \x -> [ o | o@SchemaCacheLoadedObs{} <- pure x ]

  where
    sec = 1000000
