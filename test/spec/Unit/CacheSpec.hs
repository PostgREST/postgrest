module Unit.CacheSpec (spec) where

import Control.Concurrent.STM
import Protolude
import Test.Hspec

import PostgREST.Cache.Sieve qualified as SC

spec :: Spec
spec =
  describe "Sieve cache" $
    describe "tracking thrashing" $ do
      it "reports a miss of a key among the last evicted ones once" $ do
        thrashes <- newTVarIO 0
        c <- SC.cacheIO (config pure){SC.maxSize = pure 1} >>= atomically . SC.trackThrashing (pure 1) (void $ increment thrashes)
        traverse_ (SC.cached c) [1, 2, 1, 1]
        readTVarIO thrashes `shouldReturn` 1
        -- 3 was never cached, and 2 is not the last evicted key anymore
        traverse_ (SC.cached c) [3, 2]
        readTVarIO thrashes `shouldReturn` 1

      it "doesn't report misses of invalidated keys" $ do
        thrashes <- newTVarIO 0
        loads <- newTVarIO 0
        c <-
          SC.cacheIO
            (config $ const $ increment loads)
              { SC.validator = pure $ \_ value ->
                  if value == 1 then Just (SC.Invalid $ pure (-1)) else Nothing
              }
            >>= (atomically . SC.trackThrashing (pure 16) (void $ increment thrashes))
        SC.cached c 1 `shouldReturn` 1
        SC.cached c 1 `shouldReturn` (-1)
        SC.cached c 1 `shouldReturn` 2
        readTVarIO thrashes `shouldReturn` 0

config :: (Applicative m) => (Int -> m v) -> SC.CacheConfig m Int v
config loader =
  SC.CacheConfig
    { SC.maxSize = pure 16,
      SC.prepareKey = identity,
      SC.load = loader,
      SC.requestListener = const $ pure (),
      SC.evictionListener = const $ const $ pure (),
      SC.validator = SC.alwaysValid
    }

increment :: TVar Int -> IO Int
increment counter = atomically $ do
  modifyTVar' counter (+ 1)
  readTVar counter
