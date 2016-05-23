module Feature.QueryLimitedSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders, simpleStatus))
import Text.Heredoc
import SpecHelper
import Network.Wai (Application)

spec :: SpecWith Application
spec =
  describe "Requesting many items with server limits enabled" $ do
    it "restricts results" $
      get "/items"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":2}] |]
        , matchStatus  = 206
        , matchHeaders = ["Content-Range" <:> "0-1/15"]
        }

    it "respects additional client limiting" $ do
      r <- request methodGet  "/items"
                   (rangeHdrs $ ByteRangeFromTo 0 0) ""
      liftIO $ do
        simpleHeaders r `shouldSatisfy`
          matchHeader "Content-Range" "0-0/15"
        simpleStatus r `shouldBe` partialContent206

    it "limit works on all levels" $
      get "/users?select=id,tasks{id}&order=id.asc&tasks.order=id.asc"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [str|[{"id":1,"tasks":[{"id":1},{"id":2}]},{"id":2,"tasks":[{"id":5},{"id":6}]}]|]
        , matchStatus  = 206
        , matchHeaders = ["Content-Range" <:> "0-1/3"]
        }
