{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
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

expectMetrics :: (Traversable t, Show (t b), Eq (t b)) => t (t2 -> WaiSession t2 b, b -> b) -> WaiSession t2 a -> WaiSession t2 ()
expectMetrics expectations act = do
  metrics <- getState
  expected <- traverse (\(f, toExpected) -> toExpected <$> f metrics) expectations
  void act
  result <- traverse (($ metrics) . fst) expectations
  liftIO $ result `shouldBe` expected

spec :: SpecWith (MetricsState, Application)
spec = describe "Server started with JWT and metrics enabled" $ do

  let counterToInt f metrics = round @Double @Int <$> getCounter (f metrics)
      expectCounters = expectMetrics . fmap (first counterToInt)
      genToken = authHeaderJWT . generateJWT

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

    expectMetrics
      [
        (counterToInt jwtCacheRequests,  (+ 6))
      , (counterToInt jwtCacheHits,      (+ 3))
      , (counterToInt jwtCacheEvictions, (+ 1))
      ] $

         request methodGet "/authors_only" [jwt1] ""
      *> request methodGet "/authors_only" [jwt2] ""
      -- this one should hit the cache
      *> request methodGet "/authors_only" [jwt1] ""
      -- this one should trigger eviction of jwt2
      *> request methodGet "/authors_only" [jwt3] ""
      -- thiese two should hit the cache
      *> request methodGet "/authors_only" [jwt1] ""
      *> request methodGet "/authors_only" [jwt3] ""
