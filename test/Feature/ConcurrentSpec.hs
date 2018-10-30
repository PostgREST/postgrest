{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Feature.ConcurrentSpec where

import Control.Monad (void)
import Control.Monad.Base

import Control.Monad.Trans.Control
import Control.Concurrent.Async (mapConcurrently)

import Test.Hspec
import Test.Hspec.Wai.Internal
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Test (Session)

import Network.Wai (Application)

import Protolude hiding (get)

spec :: SpecWith Application
spec =
  describe "Queryiny in parallel" $
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

raceTest :: Int -> WaiExpectation -> WaiExpectation
raceTest times = liftBaseDiscard go
 where
  go test = void $ mapConcurrently (const test) [1..times]

instance MonadBaseControl IO WaiSession where
  type StM WaiSession a = StM Session a
  liftBaseWith f = WaiSession $
    liftBaseWith $ \runInBase ->
      f $ \k -> runInBase (unWaiSession k)
  restoreM = WaiSession . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadBase IO WaiSession where
  liftBase = liftIO
