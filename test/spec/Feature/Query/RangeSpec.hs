module Feature.Query.RangeSpec where

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (simpleHeaders, simpleStatus))

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = do
  describe "GET /rpc/getitemrange" $ do
    context "with range headers" $ do
      context "of acceptable range" $ do
        it "succeeds with partial content" $ do
          r <- request methodGet  "/rpc/getitemrange?min=0&max=15"
                       (rangeHdrs $ ByteRangeFromTo 0 1) mempty
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-1/*"
            simpleStatus r `shouldBe` ok200

        it "understands open-ended ranges" $
          request methodGet "/rpc/getitemrange?min=0&max=15"
                  (rangeHdrs $ ByteRangeFrom 0) mempty
            `shouldRespondWith` 200

        it "returns an empty body when there are no results" $
          request methodGet "/rpc/getitemrange?min=2&max=2"
                  (rangeHdrs $ ByteRangeFromTo 0 1) mempty
            `shouldRespondWith` "[]"
            { matchStatus  = 200
            , matchHeaders = ["Content-Range" <:> "*/*"]
            }

        it "allows one-item requests" $ do
          r <- request methodGet  "/rpc/getitemrange?min=0&max=15"
                       (rangeHdrs $ ByteRangeFromTo 0 0) mempty
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-0/*"
            simpleStatus r `shouldBe` ok200

        it "handles ranges beyond collection length via truncation" $ do
          r <- request methodGet  "/rpc/getitemrange?min=0&max=15"
                       (rangeHdrs $ ByteRangeFromTo 10 100) mempty
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "10-14/*"
            simpleStatus r `shouldBe` ok200

      context "of invalid range" $ do
        it "fails with 416 for offside range" $
          request methodGet  "/rpc/getitemrange?min=2&max=2"
                  (rangeHdrs $ ByteRangeFromTo 1 0) mempty
            `shouldRespondWith`
              [json| {
                "message":"Requested range not satisfiable",
                "code":"PGRST103",
                "details":"The lower boundary must be lower than or equal to the upper boundary in the Range header.",
                "hint":null
              }|]
            { matchStatus = 416 }

        it "refuses a range with nonzero start when there are no items" $
          request methodGet "/rpc/getitemrange?min=2&max=2"
                  (rangeHdrsWithCount $ ByteRangeFromTo 1 2) mempty
            `shouldRespondWith`
              [json| {
                "message":"Requested range not satisfiable",
                "code":"PGRST103",
                "details":"An offset of 1 was requested, but there are only 0 rows.",
                "hint":null
              }|]
            { matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/0"]
            }

        it "refuses a range requesting start past last item" $
          request methodGet "/rpc/getitemrange?min=0&max=15"
                  (rangeHdrsWithCount $ ByteRangeFromTo 100 199) mempty
            `shouldRespondWith`
              [json| {
                "message":"Requested range not satisfiable",
                "code":"PGRST103",
                "details":"An offset of 100 was requested, but there are only 15 rows.",
                "hint":null
              }|]
            { matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/15"]
            }

  describe "GET /items" $ do
    context "with range headers" $ do
      context "of acceptable range" $ do
        it "succeeds with partial content" $ do
          r <- request methodGet  "/items"
                       (rangeHdrs $ ByteRangeFromTo 0 1) ""
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-1/*"
            simpleStatus r `shouldBe` ok200

        it "understands open-ended ranges" $
          request methodGet "/items"
                  (rangeHdrs $ ByteRangeFrom 0) ""
            `shouldRespondWith` 200

        it "returns an empty body when there are no results" $
          request methodGet "/menagerie"
                  (rangeHdrs $ ByteRangeFromTo 0 1) ""
            `shouldRespondWith` "[]"
            { matchStatus  = 200
            , matchHeaders = ["Content-Range" <:> "*/*"]
            }

        it "allows one-item requests" $ do
          r <- request methodGet  "/items"
                       (rangeHdrs $ ByteRangeFromTo 0 0) ""
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "0-0/*"
            simpleStatus r `shouldBe` ok200

        it "handles ranges beyond collection length via truncation" $ do
          r <- request methodGet  "/items"
                       (rangeHdrs $ ByteRangeFromTo 10 100) ""
          liftIO $ do
            simpleHeaders r `shouldSatisfy`
              matchHeader "Content-Range" "10-14/*"
            simpleStatus r `shouldBe` ok200

      context "of invalid range" $ do
        it "fails with 416 for offside range" $
          request methodGet  "/items"
                  (rangeHdrs $ ByteRangeFromTo 1 0) ""
            `shouldRespondWith`
              [json| {
                "message":"Requested range not satisfiable",
                "code":"PGRST103",
                "details":"The lower boundary must be lower than or equal to the upper boundary in the Range header.",
                "hint":null
              }|]
            { matchStatus = 416 }

        it "refuses a range with nonzero start when there are no items" $
          request methodGet "/menagerie"
                  (rangeHdrsWithCount $ ByteRangeFromTo 1 2) ""
            `shouldRespondWith`
              [json| {
                "message":"Requested range not satisfiable",
                "code":"PGRST103",
                "details":"An offset of 1 was requested, but there are only 0 rows.",
                "hint":null
              }|]
            { matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/0"]
            }

        it "refuses a range requesting start past last item" $
          request methodGet "/items"
                  (rangeHdrsWithCount $ ByteRangeFromTo 100 199) ""
            `shouldRespondWith`
              [json| {
                "message":"Requested range not satisfiable",
                "code":"PGRST103",
                "details":"An offset of 100 was requested, but there are only 15 rows.",
                "hint":null
              }|]
            { matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/15"]
            }

    context "when count=planned" $ do
      it "obtains a filtered range" $ do
        request methodGet "/items?select=id&id=gt.8"
            (("Prefer", "count=planned") : rangeHdrs (ByteRangeFromTo 0 6))
            ""
          `shouldRespondWith`
            [json|[{"id":9}, {"id":10}, {"id":11}, {"id":12}, {"id":13}, {"id":14}, {"id":15}]|]
            { matchStatus  = 206
            , matchHeaders = ["Content-Range" <:> "0-6/8"]
            }

        request methodGet "/child_entities?select=id&id=gt.3"
            (("Prefer", "count=planned") : rangeHdrs (ByteRangeFromTo 0 2))
            ""
          `shouldRespondWith`
            [json|[{"id":4}, {"id":5}, {"id":6}]|]
            { matchStatus  = 206
            , matchHeaders = ["Content-Range" <:> "0-2/4"]
            }

        request methodGet "/getallprojects_view?select=id&id=lt.3"
            (("Prefer", "count=planned") : rangeHdrs (ByteRangeFromTo 0 1))
            ""
          `shouldRespondWith`
            [json|[{"id":1}, {"id":2}]|]
            { matchStatus  = 206
            , matchHeaders = ["Content-Range" <:> "0-1/673"]
            }
