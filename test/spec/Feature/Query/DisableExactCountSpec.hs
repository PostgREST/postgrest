module Feature.Query.DisableExactCountSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "tests for config db-exact-count-enable" $ do

    context "should not return exact count" $ do
      it "returns * in content-range instead of exact count" $ do
        request methodHead  "/getallprojects_view"
          [("Prefer", "count=exact")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = [ matchContentTypeJson
                           , "Content-Range" <:> "0-4/*"]
          }

