{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Feature.ConcurrentSpec where

import Control.Concurrent.Async (mapConcurrently)
import Network.Wai              (Application)

import Control.Monad.Base
import Control.Monad.Trans.Control

import Network.Wai.Test        (Session)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Internal
import Test.Hspec.Wai.JSON

import Protolude hiding (get)

spec :: SpecWith ((), Application)
spec =
  describe "Querying in parallel" $
    it "should not raise 'transaction in progress' error" $
      raceTest 10 $
        get "/fakefake"
          `shouldRespondWith` [json|
              { "hint": null,
                "details":null,
                "code":"42P01",
                "message":"relation \"test.fakefake\" does not exist"
              } |]
          { matchStatus  = 404
          , matchHeaders = []
          }

raceTest :: Int -> WaiExpectation st -> WaiExpectation st
raceTest times = liftBaseDiscard go
 where
  go test = void $ mapConcurrently (const test) [1..times]

instance MonadBaseControl IO (WaiSession st) where
  type StM (WaiSession st) a = StM Session a
  liftBaseWith f = WaiSession $
    liftBaseWith $ \runInBase ->
      f $ \k -> runInBase (unWaiSession k)
  restoreM = WaiSession . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadBase IO (WaiSession st) where
  liftBase = liftIO
