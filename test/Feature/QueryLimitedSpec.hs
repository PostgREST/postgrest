module Feature.QueryLimitedSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "Requesting many items with server limits(max-rows) enabled" $ do
    it "restricts results" $
      get "/items?order=id"
        `shouldRespondWith`
          [json| [{"id":1},{"id":2}] |]
          { matchHeaders = ["Content-Range" <:> "0-1/*"] }

    it "respects additional client limiting" $ do
      request methodGet  "/items"
          (rangeHdrs $ ByteRangeFromTo 0 0)
          ""
        `shouldRespondWith`
          [json| [{"id":1}] |]
          { matchHeaders = ["Content-Range" <:> "0-0/*"] }

    it "works on all levels" $
      get "/users?select=id,tasks(id)&order=id.asc&tasks.order=id.asc"
        `shouldRespondWith`
          [json|[{"id":1,"tasks":[{"id":1},{"id":2}]},{"id":2,"tasks":[{"id":5},{"id":6}]}]|]
          { matchHeaders = ["Content-Range" <:> "0-1/*"] }

    it "succeeds in getting parent embeds despite the limit, see #647" $
      get "/tasks?select=id,project:projects(id)&id=gt.5"
        `shouldRespondWith`
          [json|[{"id":6,"project":{"id":3}},{"id":7,"project":{"id":4}}]|]
          { matchHeaders = ["Content-Range" <:> "0-1/*"] }

    it "can offset the parent embed, being consistent with the other embed types" $
      get "/tasks?select=id,project:projects(id)&id=gt.5&project.offset=1"
        `shouldRespondWith`
          [json|[{"id":6,"project":null}, {"id":7,"project":null}]|]
          { matchHeaders = ["Content-Range" <:> "0-1/*"] }

    context "count=estimated" $ do
      it "uses the query planner guess when query rows > maxRows" $
        request methodHead "/getallprojects_view" [("Prefer", "count=estimated")] ""
          `shouldRespondWith`
            ""
            { matchStatus  = 206
            , matchHeaders = ["Content-Range" <:> "0-1/2019"]
            }

      it "gives exact count when query rows <= maxRows" $
        request methodHead "/getallprojects_view?id=lt.3" [("Prefer", "count=estimated")] ""
          `shouldRespondWith`
            ""
            { matchHeaders = ["Content-Range" <:> "0-1/2"] }

      it "only uses the query planner guess if it's indeed greater than the exact count" $
        request methodHead "/get_projects_above_view" [("Prefer", "count=estimated")] ""
          `shouldRespondWith`
            ""
            { matchStatus  = 206
            , matchHeaders = ["Content-Range" <:> "0-1/3"]
            }
