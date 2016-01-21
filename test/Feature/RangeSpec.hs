module Feature.RangeSpec where

import Data.Pool
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders,simpleStatus))
import qualified Hasql.Connection  as H

import SpecHelper
import PostgREST.Types (DbStructure(..))

spec :: DbStructure -> Pool H.Connection -> Spec
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
            `shouldRespondWith` 416

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
