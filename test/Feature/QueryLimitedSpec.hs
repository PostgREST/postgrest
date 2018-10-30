module Feature.QueryLimitedSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders, simpleStatus))
import SpecHelper
import Network.Wai (Application)

import Protolude hiding (get)

spec :: SpecWith Application
spec =
  describe "Requesting many items with server limits enabled" $ do
    it "restricts results" $
      get "/items"
        `shouldRespondWith` [json| [{"id":1},{"id":2}] |]
        { matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/*"]
        }

    it "respects additional client limiting" $ do
      r <- request methodGet  "/items"
                   (rangeHdrs $ ByteRangeFromTo 0 0) ""
      liftIO $ do
        simpleHeaders r `shouldSatisfy`
          matchHeader "Content-Range" "0-0/*"
        simpleStatus r `shouldBe` ok200

    it "limit works on all levels" $
      get "/users?select=id,tasks(id)&order=id.asc&tasks.order=id.asc"
        `shouldRespondWith` [json|[{"id":1,"tasks":[{"id":1},{"id":2}]},{"id":2,"tasks":[{"id":5},{"id":6}]}]|]
        { matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/*"]
        }

    it "limit is not applied to parent embeds" $
      get "/tasks?select=id,project(id)&id=gt.5"
        `shouldRespondWith` [json|[{"id":6,"project":{"id":3}},{"id":7,"project":{"id":4}}]|]
        { matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/*"]
        }
