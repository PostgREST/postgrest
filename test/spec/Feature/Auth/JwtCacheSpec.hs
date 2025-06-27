{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
module Feature.Auth.JwtCacheSpec

where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec         (Expectation, SpecWith, describe, it,
                           shouldBe)
import Test.Hspec.Wai

import Data.String                     (String)
import PostgREST.Metrics               (MetricsState (..))
import Prometheus                      (getCounter)
import Protolude
import SpecHelper
import Test.Hspec.Expectations.Contrib (annotate)
import Test.Hspec.Wai.JSON             (json)

spec :: SpecWith (MetricsState, Application)
spec = describe "Server started with JWT and metrics enabled" $ do
  it "Should not have JWT in cache" $ do
    let auth = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe1"}|]

    expectCounters
      [
        requests (+ 1)
      , hits     (+ 0)
      ] $

         request methodGet "/authors_only" [auth] ""

  it "Should have JWT in cache" $ do
    let auth = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe2"}|]

    expectCounters
      [
        requests (+ 2)
      , hits     (+ 1)
      ] $

         request methodGet "/authors_only" [auth] "" `shouldRespondWith` 200
      *> request methodGet "/authors_only" [auth] "" `shouldRespondWith` 200

  it "Should not cache invalid JWTs" $ do
    let auth = authHeaderJWT "some random bytes"

    expectCounters
      [
        requests (+ 2)
      , hits     (+ 0)
      ] $

         request methodGet "/authors_only" [auth] "" `shouldRespondWith` 401
      *> request methodGet "/authors_only" [auth] "" `shouldRespondWith` 401

  it "Should cache expired JWTs" $ do
    let auth = genToken [json|{"exp": 1, "role": "postgrest_test_author", "id": "jdoe2"}|]

    expectCounters
      [
        requests (+ 2)
      , hits     (+ 1)
      ] $

         request methodGet "/authors_only" [auth] "" `shouldRespondWith` 401
      *> request methodGet "/authors_only" [auth] "" `shouldRespondWith` 401

  it "Should evict entries from the JWT cache (jwt cache max is 2)" $ do
    let jwt1 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe3"}|]
        jwt2 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe4"}|]
        jwt3 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe5"}|]

    expectCounters
      [
        requests  (+ 6)
      , hits      (+ 0)
      , evictions (+ 4)
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
        requests  (+ 6)
      , hits      (+ 3)
      , evictions (+ 1)
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

  -- This one makes sure we test the scenario when finger
  -- has to move through the whole list first and pass the head
  -- The test case was added based on coverage report
  -- showing this scenario was not covered by previous tests
  it "Should evict entries even though all were hit" $ do
    let jwt1 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe9"}|]
        jwt2 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe10"}|]
        jwt3 = genToken [json|{"exp": 9999999999, "role": "postgrest_test_author", "id": "jdoe11"}|]

    expectCounters
      [
        requests  (+ 7)
      , hits      (+ 4)
      , evictions (+ 1)
      ] $

         request methodGet "/authors_only" [jwt1] ""
      *> request methodGet "/authors_only" [jwt2] ""
      -- these two should hit the cache
      *> request methodGet "/authors_only" [jwt1] ""
      *> request methodGet "/authors_only" [jwt2] ""
      -- this one should trigger eviction of jwt1
      *> request methodGet "/authors_only" [jwt3] ""
      -- these two should hit the cache
      *> request methodGet "/authors_only" [jwt2] ""
      *> request methodGet "/authors_only" [jwt3] ""

  where
      counterToInt = second (fmap (round @Double @Int) . getCounter)
      expectCounters = stateCheck . fmap (\(g, h) -> StateCheck (counterToInt . g) (flip shouldBe . h))
      genToken = authHeaderJWT . generateJWT
      requests = (,) (getF @"jwtCacheRequests")
      hits = (,) (getF @"jwtCacheHits")
      evictions = (,) (getF @"jwtCacheEvictions")


-- should be moved to helpers???
getF :: forall s r a. (KnownSymbol s, HasField s r a) => r -> (String, a)
getF r = (symbolVal (Proxy @s), getField @s r)

data StateCheck st = forall a. (Show a, Eq a) => StateCheck (st -> (String, WaiSession st a)) (a -> a -> Expectation)

stateCheck :: (Traversable t) => t (StateCheck st) -> WaiSession st a -> WaiSession st ()
stateCheck checks act = do
  metrics <- getState
  expectations <- traverse (\(StateCheck g expect) -> let (msg, m) = g metrics in m >>= createExpectation msg m . expect) checks
  void act
  sequenceA_ expectations
  where
    createExpectation msg metrics expect = pure $ metrics >>= liftIO . annotate msg . expect
