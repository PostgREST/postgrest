module Feature.Query.LimitOffsetSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = do
  describe "GET /rpc/getitemrange" $ do
    context "without range headers" $ do
      context "with response under server size limit" $
        it "returns whole range with status 200" $
          get "/rpc/getitemrange?min=0&max=15" `shouldRespondWith` 200

      context "offset exceeding total rows" $ do
        it "returns nothing when offset greater than 0 for when result is empty" $
          request methodGet "/rpc/getitemrange?offset=1&min=2&max=2"
                  [] mempty
            `shouldRespondWith`
              [json|[]|]
            { matchStatus = 200 }

        it "returns nothing when offset exceeds total rows" $
          request methodGet "/rpc/getitemrange?offset=100&min=0&max=15"
                  [] mempty
            `shouldRespondWith`
              [json|[]|]
            { matchStatus  = 200 }

  describe "GET /items" $ do
    context "without range headers" $ do
      context "with response under server size limit" $
        it "returns whole range with status 200" $
          get "/items" `shouldRespondWith` 200

      context "count with an empty body" $ do
        it "returns empty body with Content-Range */0" $
          request methodGet "/items?id=eq.0"
            [("Prefer", "count=exact")] ""
            `shouldRespondWith`
              [json|[]|]
              { matchStatus = 200 }

      context "when I don't want the count" $ do
        it "does not return Content-Range" $
          request methodGet "/menagerie"
              [] ""
            `shouldRespondWith`
              [json|[]|]
              { matchStatus = 200 }

        it "does not return Content-Range" $
          request methodGet "/items?order=id"
                  [] ""
            `shouldRespondWith` [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]
            { matchStatus = 200 }

        it "does not return Content-Range even using other filters" $
          request methodGet "/items?id=eq.1&order=id"
                  [] ""
            `shouldRespondWith` [json| [{"id":1}] |]
            { matchStatus  = 200 }

    context "with limit/offset parameters" $ do
      it "no parameters return everything" $
        get "/items?select=id&order=id.asc"
          `shouldRespondWith`
          [json|[{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}]|]
          { matchStatus = 200 }
      it "top level limit with parameter" $
        get "/items?select=id&order=id.asc&limit=3"
          `shouldRespondWith` [json|[{"id":1},{"id":2},{"id":3}]|]
          { matchStatus  = 200 }
      it "headers override get parameters" $
        request methodGet  "/items?select=id&order=id.asc&limit=3"
                     (rangeHdrs $ ByteRangeFromTo 0 1) ""
          `shouldRespondWith` [json|[{"id":1},{"id":2}]|]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "0-1/*"]
          }

      it "limit works on all levels" $
        get "/clients?select=id,projects(id,tasks(id))&order=id.asc&limit=1&projects.order=id.asc&projects.limit=2&projects.tasks.order=id.asc&projects.tasks.limit=1"
          `shouldRespondWith`
          [json|[{"id":1,"projects":[{"id":1,"tasks":[{"id":1}]},{"id":2,"tasks":[{"id":3}]}]}]|]
          { matchStatus = 200 }

      it "limit and offset works on first level" $ do
        get "/items?select=id&order=id.asc&limit=3&offset=2"
          `shouldRespondWith` [json|[{"id":3},{"id":4},{"id":5}]|]
          { matchStatus = 200 }
        request methodHead "/items?select=id&order=id.asc&limit=3&offset=2"
            []
            mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 200
            , matchHeaders = [ matchContentTypeJson ]
            }

      context "succeeds if offset equals 0 as a no-op" $ do
        it  "no items" $ do
          get "/items?offset=0&id=eq.0"
            `shouldRespondWith`
              [json|[]|]
              { matchStatus = 200 }

          request methodGet "/items?offset=0&id=eq.0"
            [] ""
            `shouldRespondWith`
              [json|[]|]
              { matchStatus = 200 }

        it  "one or more items" $
          get "/items?select=id&offset=0&order=id"
            `shouldRespondWith`
              [json|[{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}]|]
              { matchStatus = 200 }

      it "fails with pg error if offset is negative" $
        get "/items?select=id&offset=-4&order=id"
          `shouldRespondWith`
            [json|{"code":"2201X","details":null,"hint":null,"message":"OFFSET must not be negative"} |]
            { matchStatus  = 400
            , matchHeaders = [matchContentTypeJson] }

      it "succeeds and returns an empty array if limit equals 0" $
        get "/items?select=id&limit=0"
          `shouldRespondWith` [json|[]|]
            { matchStatus  = 200 }

      it "fails if limit is negative" $
        get "/items?select=id&limit=-1"
          `shouldRespondWith`
            [json|{"code":"2201W","details":null,"hint":null,"message":"LIMIT must not be negative"} |]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }
