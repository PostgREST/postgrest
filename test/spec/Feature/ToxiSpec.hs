{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
module Feature.ToxiSpec where

import           Control.Monad.Trans.Control (liftBaseDiscard)
import           Network.Wai                 (Application)
import qualified PostgREST.AppState          as AppState
import           Protolude                   hiding (get)
import           SpecHelper
import           Test.Hspec                  (SpecWith, describe, it)
import           Test.Hspec.Wai
import           Toxiproxy                   (withDisabled)

spec :: SpecWith (SpecState, Application)
spec = describe "Tests using Toxiproxy" $ do
  it "Should return 503 on temporary database server unavailability" $ do
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
