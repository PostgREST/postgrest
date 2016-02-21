{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Feature.ConcurrentSpec where

import Control.Monad (void)
import Control.Monad.Base

import Control.Monad.Trans.Control
import Control.Concurrent.Async (mapConcurrently)

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai.Internal
import Test.Hspec.Wai
import Network.Wai.Test (Session)
import qualified Hasql.Connection  as H

import SpecHelper
import PostgREST.Types (DbStructure(..))

spec :: DbStructure -> H.Connection -> Spec
spec struct c = around (withApp cfgDefault struct c) $

  describe "Queryiny in parallel" $
    it "should not raise 'transaction in progress' error" $
      raceTest 3 $
        get "/fakefake" `shouldRespondWith` 404

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
