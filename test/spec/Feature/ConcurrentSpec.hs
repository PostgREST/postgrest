{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Feature.ConcurrentSpec where

import Control.Concurrent.Async (mapConcurrently)
import Network.Wai              (Application)

import Control.Monad.Trans.Control

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import SpecHelper ()

import Protolude hiding (get)

spec :: SpecWith ((), Application)
spec =
  describe "Querying in parallel" $
    it "should not raise 'transaction in progress' error" $
      raceTest 10 $
        get "/fakefake"
          `shouldRespondWith`
          [json| {"code":"PGRST205","details":null,"hint":"Perhaps you meant the table 'test.factories'","message":"Could not find the table 'test.fakefake' in the schema cache"} |]
          { matchStatus  = 404
          , matchHeaders = []
          }

raceTest :: Int -> WaiExpectation st -> WaiExpectation st
raceTest times = liftBaseDiscard go
 where
  go test = void $ mapConcurrently (const test) [1..times]
