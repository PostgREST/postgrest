{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Observation.ListenerSpec where

import Network.Wai (Application)

import ObsHelper

import qualified PostgREST.Listener    as Listener
import           PostgREST.Observation

import Test.Hspec     (SpecWith, describe, it)
import Test.Hspec.Wai (getState)

import Protolude

spec :: SpecWith (SpecState, Application)
spec = describe "Listener tests" $ do

  it "Should start the listener" $ do
    SpecState{specAppState = appState, specObsChan} <- getState
    let waitFor = waitForObs specObsChan

    liftIO $ withListener appState $ do
      waitFor (1 * sec) "DBListenStart" $ \x -> [ o | o@(DBListenStart {}) <- pure x ]

  where
    sec = 1000000
    withListener appState = bracket (Listener.runListener appState) identity . const
