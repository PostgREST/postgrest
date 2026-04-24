{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
module Observation.ToxiSpec where

import           Control.Monad.Trans.Control (liftBaseDiscard)
import qualified Data.Map                    as M
import           Network.Wai                 (Application)
import           ObsHelper
import qualified PostgREST.AppState          as AppState
import           PostgREST.Listener          (runListener,
                                              runListener')
import           PostgREST.Observation       (Observation (..))
import           Protolude                   hiding (get)
import           Test.Hspec                  (SpecWith, describe, it)
import           Test.Hspec.Wai
import           Toxiproxy                   (Stream (..), Toxic (..),
                                              ToxicType (..),
                                              withDisabled, withToxic)

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

  describe "Toxiproxy tests of notification listener" $ do
    it "should start listener" $ do
      SpecState {specAppState, specObsChan} <- getState
      let waitFor = waitForObs specObsChan

      liftIO $ withListener specAppState $
          waitFor (1*sec) "DBListenStart" $ \x -> [ o | o@DBListenStart{} <- pure x]

    it "should retry listener" $ do
      SpecState {specAppState, specObsChan, specToxiProxy} <- getState
      let waitFor = waitForObs specObsChan

      liftIO $ bracket (
        withDisabled specToxiProxy $ do
          stopListener <- runListener specAppState
          (do
            waitFor (1*sec) "DBListenFail" $ \x -> [ o | o@DBListenFail{} <- pure x ]
            waitFor (1*sec) "DBListenRetry" $ \x -> [ o | o@DBListenRetry{} <- pure x ])
            `onException` stopListener
          pure stopListener)
        identity
        (const $ waitFor (2*sec) "DBListenStart" $ \x -> [ o | o@DBListenStart{} <- pure x])

    it "should retry listener with exponential backoff when connection broken" $ do
      SpecState {specAppState, specObsChan, specToxiProxy} <- getState
      let waitFor = waitForObs specObsChan

      liftIO $ withListener specAppState $ do
        waitFor (1*sec) "DBListenStart" $ \x -> [ o | o@DBListenStart{} <- pure x]
        withDisabled specToxiProxy $ do
          waitFor (1*sec) "DBListenFail" $ \x -> [ o | o@DBListenFail{} <- pure x ]
          waitFor (1*sec) "DBListenRetry 1" $ \x -> [ o | o@(DBListenRetry 1) <- pure x ]
          waitFor (2*sec) "DBListenRetry 2" $ \x -> [ o | o@(DBListenRetry 2) <- pure x ]
          waitFor (3*sec) "DBListenRetry 4" $ \x -> [ o | o@(DBListenRetry 4) <- pure x ]
        waitFor (5*sec) "DBListenStart after retries" $ \x -> [ o | o@DBListenStart{} <- pure x]
        waitFor (1*sec) "SchemaCacheLoadedObs" $ \x -> [ o | o@SchemaCacheLoadedObs{} <- pure x ]

    -- this scenario cannot be tested with Toxiproxy
    -- because keepalives are handled by the kernel TCP/IP stack
    -- left here as an example and template for a test
    -- using some future more advanced tool
    it "should detect broken connection using keepalives" $ do
      pendingWith "Cannot be tested with Toxiproxy"
      SpecState {specAppState, specObsChan, specToxiProxy} <- getState
      let waitFor = waitForObs specObsChan

      liftIO $ withKeepAliveListener specAppState $ do
        waitFor (1*sec) "DBListenStart" $ \x -> [ o | o@DBListenStart{} <- pure x]
        withTimedOutConnection specToxiProxy $ do
          waitFor (10*sec) "DBListenFail" $ \x -> [ o | o@DBListenFail{} <- pure x ]
          waitFor (1*sec) "DBListenRetry 1" $ \x -> [ o | o@(DBListenRetry 1) <- pure x ]
        waitFor (2*sec) "DBListenStart after timeout toxic" $ \x -> [ o | o@DBListenStart{} <- pure x]

  where
    withListener appState = bracket (runListener appState) identity . const
    withKeepAliveListener appState = bracket (runListener' appState 1 1) identity . const
    withTimedOutConnection proxy =
      withToxic proxy (timeoutToxic Upstream) .
      withToxic proxy (timeoutToxic Downstream)
    timeoutToxic stream = Toxic
      { toxicName = case stream of
          Upstream   -> "listener-timeout-upstream"
          Downstream -> "listener-timeout-downstream"
      , toxicType = Timeout
      , toxicStream = stream
      , toxicToxicity = 1
      , toxicAttributes = M.fromList [("timeout", 0)]
      }
    sec = 1_000_000
