{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
module Observation.ToxiSpec where

import           Control.Monad.Trans.Control (liftBaseDiscard)
import           Network.Wai                 (Application)
import           ObsHelper
import qualified PostgREST.AppState          as AppState
import           PostgREST.Metrics           (MetricsState (poolAvailable))
import qualified PostgREST.Metrics           as Metric
import           Prometheus                  (getGauge)
import           Protolude                   hiding (get)
import           Test.Hspec                  (SpecWith, describe, it)
import           Test.Hspec.Expectations
import           Test.Hspec.Wai
import           Toxiproxy                   (withDisabled)

spec :: SpecWith (SpecState, Application)
spec = describe "Tests using Toxiproxy" $ do
  it "Should return 503 on temporary database server unavailability" $ do
    pendingWith "TODO fix"
    SpecState{specAppState, specToxiProxy} <- getState

    -- make sure there are no open connections
    liftIO $ AppState.flushPool specAppState

    liftBaseDiscard (withDisabled specToxiProxy) $ do
        void $ get "/items?id=eq.5"
            `shouldRespondWith` 503

    void $ get "/items?id=eq.5"
        `shouldRespondWith` 200

    liftBaseDiscard (withDisabled specToxiProxy) $ do
        void $ get "/items?id=eq.5"
            `shouldRespondWith` 503

  it "Must not have negative connection count" $ do
    SpecState{specAppState, specToxiProxy, specMetrics=Metric.MetricsState{poolAvailable}} <- getState

    -- make sure there are no open connections
    liftIO $ AppState.flushPool specAppState

    liftBaseDiscard (withDisabled specToxiProxy) $ do
        replicateM_ 5 $ get "/authors_only?id=eq.5"
            `shouldRespondWith` 503

    -- TODO https://github.com/PostgREST/postgrest/issues/4622
    -- change to 0 when fixed
    -- HSpec does not support xfail which should be used instead
    liftIO $ getGauge poolAvailable >>= (`shouldBe` (-5::Int)) . round
