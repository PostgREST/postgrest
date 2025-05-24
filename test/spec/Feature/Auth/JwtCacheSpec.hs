{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
module Feature.Auth.JwtCacheSpec

where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import PostgREST.Metrics   (MetricsState (..))
import Prometheus          (getCounter)
import Protolude
import SpecHelper
import Test.Hspec.Wai.JSON (json)

spec :: SpecWith (MetricsState, Application)
spec = describe "Server started with JWT and metrics enabled" $ do

  it "Should not have JWT in cache" $ do
    let auth = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe1"}|]

    expectCounters
      [
        (jwtCacheRequests, (+ 1))
      , (jwtCacheHits,     (+ 0))
      ] $

         request methodGet "/authors_only" [auth] ""

  it "Should have JWT in cache" $ do
    let auth = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe2"}|]

    expectCounters
      [
        (jwtCacheRequests, (+ 2))
      , (jwtCacheHits,     (+ 1))
      ] $

         request methodGet "/authors_only" [auth] "" `shouldRespondWith` 200
      *> request methodGet "/authors_only" [auth] "" `shouldRespondWith` 200

  it "Should evict entries from the JWT cache" $ do
    let jwt1 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe3"}|]
        jwt2 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe4"}|]
        jwt3 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe5"}|]

    expectCounters
      [
        (jwtCacheRequests,  (+ 6))
      , (jwtCacheHits,      (+ 0))
      , (jwtCacheEvictions, (+ 4))
      ] $

         request methodGet "/authors_only" [jwt1] ""
      *> request methodGet "/authors_only" [jwt2] ""
      *> request methodGet "/authors_only" [jwt3] ""
      *> request methodGet "/authors_only" [jwt1] ""
      *> request methodGet "/authors_only" [jwt2] ""
      *> request methodGet "/authors_only" [jwt3] ""

  it "Should not evict entries from the JWT cache in FIFO order" $ do
    let jwt1 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe6"}|]
        jwt2 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe7"}|]
        jwt3 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe8"}|]

    expectCounters
      [
        (jwtCacheRequests,  (+ 6))
      , (jwtCacheHits,      (+ 3))
      , (jwtCacheEvictions, (+ 1))
      ] $

         request methodGet "/authors_only" [jwt1] ""
      *> request methodGet "/authors_only" [jwt2] ""
      -- this one should hit the cache
      *> request methodGet "/authors_only" [jwt1] ""
      -- this one should trigger eviction of jwt2 (not FIFO)
      *> request methodGet "/authors_only" [jwt3] ""
      -- these two should hit the cache
      *> request methodGet "/authors_only" [jwt1] ""
      *> request methodGet "/authors_only" [jwt3] ""

  where
      counterToInt f metrics = round @Double @Int <$> getCounter (f metrics)
      expectCounters = stateCheck . fmap (\(f, g) -> StateCheck (counterToInt f) (flip shouldBe . g))
      genToken = authHeaderJWT . generateJWT

-- should be moved to helpers???
data StateCheck st = forall a. (Show a, Eq a) => StateCheck (st -> WaiSession st a) (a -> a -> Expectation)

stateCheck :: (Traversable t) => t (StateCheck st) -> WaiSession st a -> WaiSession st ()
stateCheck checks act = do
  metrics <- getState
  expectations <- traverse (\(StateCheck f expect) -> f metrics >>= createExpectation (f metrics) . expect) checks
  void act
  sequenceA_ expectations
  where
    createExpectation metrics expect = pure $ metrics >>= liftIO . expect

