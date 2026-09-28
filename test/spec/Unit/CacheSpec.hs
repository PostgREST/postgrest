module Unit.CacheSpec (spec) where

import Control.Concurrent.STM
import Protolude
import Test.Hspec

import PostgREST.Cache.Sieve qualified as SC

spec :: Spec
spec =
  describe "Sieve cache" $
    describe "validating cached values" $ do
      it "keeps values that are always valid" $ do
        loads <- newTVarIO 0
        c <- SC.cacheIO (config $ const $ increment loads)
        SC.cached c 1 `shouldReturn` 1
        SC.cached c 1 `shouldReturn` 1
        readTVarIO loads `shouldReturn` 1

      it "validates a value with its key" $ do
        loads <- newTVarIO 0
        c <-
          SC.cacheIO
            (config $ const $ increment loads)
              { SC.validator = \k -> pure $ \_ -> pure $ if k == 1 then Just (SC.Invalid $ pure 0) else Nothing
              }
        SC.cached c 1 `shouldReturn` 1
        SC.cached c 2 `shouldReturn` 2
        -- the value of 1 is discarded, the one of 2 stays cached
        SC.cached c 1 `shouldReturn` 0
        SC.cached c 2 `shouldReturn` 2
        readTVarIO loads `shouldReturn` 2

      it "validates a value with the state of the transaction that looks it up" $ do
        loads <- newTVarIO 0
        valid <- newTVarIO True
        c <-
          SC.cacheIO
            (config $ const $ increment loads)
              { SC.validator = const . pure $ \_ -> bool (Just $ SC.Refresh pass) Nothing <$> readTVar valid
              }
        SC.cached c 1 `shouldReturn` 1
        atomically $ writeTVar valid False
        -- the value is refreshed: loaded again
        SC.cached c 1 `shouldReturn` 2

config :: (Applicative m) => (Int -> m v) -> SC.CacheConfig m Int v
config loader =
  SC.CacheConfig
    { SC.maxSize = pure 16,
      SC.load = loader,
      SC.requestListener = const $ pure (),
      SC.evictionListener = const $ const $ pure (),
      SC.validator = SC.alwaysValid
    }

increment :: TVar Int -> IO Int
increment counter = atomically $ do
  modifyTVar' counter (+ 1)
  readTVar counter
