module Feature.SingularSpec where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai (Application)

import Protolude hiding (get)


spec :: SpecWith Application
spec = do
  describe "Requesting singular json object" $ do
    let _singular = ("Accept"::Text, "application/vnd.pgrst.object+json"::Text)

    context "with GET request" $
      it "fails for zero rows" $
        get "/items?id=gt.0&id=lt.0"
          `shouldRespondWith` 406
