module Feature.RangeSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders,simpleStatus))

import Hasql as H
import Hasql.Postgres as P

import SpecHelper
import PostgREST.Types (DbStructure(..))

spec :: DbStructure -> H.Pool P.Postgres -> Spec
spec struct pool = beforeAll resetDb
  . around (withApp cfgDefault struct pool) $
  describe "GET /items" $ do

    context "without range headers" $ do
      context "with response under server size limit" $
        it "returns whole range with status 200" $
          get "/items" `shouldRespondWith` 200

      context "when I don't want the count" $ do
        it "returns range Content-Range with /*" $
          request methodGet "/menagerie"
                  [("Prefer", "count=none")] ""
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Just "[]"
            , matchStatus  = 200
            , matchHeaders = ["Content-Range" <:> "*/*"]
            }

        it "returns range Content-Range with range/*" $
          request methodGet "/items?order=id"
                  [("Prefer", "count=none")] ""
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Just [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]
            , matchStatus  = 200
            , matchHeaders = ["Content-Range" <:> "0-14/*"]
            }

        it "returns range Content-Range with range/* even using other filters" $
          request methodGet "/items?id=eq.1&order=id"
                  [("Prefer", "count=none")] ""
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Just [json| [{"id":1}] |]
            , matchStatus  = 200
            , matchHeaders = ["Content-Range" <:> "0-0/*"]
            }

    context "with range headers" $ do

      context "of acceptable range" $ do
        it "succeeds with partial content" $ do
          r <- request methodGet  "/items"
                       (rangeHdrs $ ByteRangeFromTo 0 1) ""
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-1/15"
            simpleStatus r `shouldBe` partialContent206

        it "understands open-ended ranges" $
          request methodGet "/items"
                  (rangeHdrs $ ByteRangeFrom 0) ""
            `shouldRespondWith` 200

        it "returns an empty body when there are no results" $
          request methodGet "/menagerie"
                  (rangeHdrs $ ByteRangeFromTo 0 1) ""
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Just "[]"
            , matchStatus  = 200
            , matchHeaders = ["Content-Range" <:> "*/0"]
            }

        it "allows one-item requests" $ do
          r <- request methodGet  "/items"
                       (rangeHdrs $ ByteRangeFromTo 0 0) ""
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-0/15"
            simpleStatus r `shouldBe` partialContent206

        it "handles ranges beyond collection length via truncation" $ do
          r <- request methodGet  "/items"
                       (rangeHdrs $ ByteRangeFromTo 10 100) ""
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "10-14/15"
            simpleStatus r `shouldBe` partialContent206

      context "of invalid range" $ do
        it "fails with 416 for offside range" $
          request methodGet  "/items"
                  (rangeHdrs $ ByteRangeFromTo 1 0) ""
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Nothing
            , matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/15"]
            }

        it "refuses a range with nonzero start when there are no items" $
          request methodGet "/menagerie"
                  (rangeHdrs $ ByteRangeFromTo 1 2) ""
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Nothing
            , matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/0"]
            }

        it "refuses a range requesting start past last item" $
          request methodGet "/items"
                  (rangeHdrs $ ByteRangeFromTo 100 199) ""
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Nothing
            , matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/15"]
            }

    context "with range query parameters" $ do
      context "limit" $ do
        it "returns a limited set" $ do
          r <- get "/items?limit=5"
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-4/15"
            simpleStatus r `shouldBe` partialContent206

        it "can get a single item" $ do
          r <- get "/items?limit=1"
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-0/15"
            simpleStatus r `shouldBe` partialContent206

        it "will error with a limit of 0" $
          get "/items?limit=0"
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Nothing
            , matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/15"]
            }

        it "will fetch everything if too large" $ do
          r <- get "/items?limit=100"
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-14/15"
            simpleStatus r `shouldBe` ok200

      context "offset" $ do
        it "will return results not starting at 0" $ do
          r <- get "/items?offset=5"
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "5-14/15"
            simpleStatus r `shouldBe` partialContent206

        it "errors when offset is too large" $
          get "/items?offset=50"
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Nothing
            , matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/15"]
            }

      context "limit and offset" $ do
        it "will return a specific range" $ do
          r <- get "/items?limit=2&offset=5"
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "5-6/15"
            simpleStatus r `shouldBe` partialContent206

        it "will return everything with a large limit" $ do
          r <- get "/items?limit=200&offset=5"
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "5-14/15"
            simpleStatus r `shouldBe` partialContent206

        it "errors when offset is too large" $
          get "/items?limit=1&offset=50"
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Nothing
            , matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/15"]
            }

      context "page" $ do
        it "will use the limit to get the offset" $ do
          r <- get "/items?limit=2&page=2"
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "4-5/15"
            simpleStatus r `shouldBe` partialContent206

        it "will be overriden by offset" $ do
          r <- get "/items?limit=2&offset=5&page=2"
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "5-6/15"
            simpleStatus r `shouldBe` partialContent206
