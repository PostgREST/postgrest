module Feature.DeleteSpec where

import Test.Hspec
import Test.Hspec.Wai
import SpecHelper

import Network.HTTP.Types

spec :: Spec
spec = beforeAll (clearTable "items" >> createItems 30) . afterAll_ (clearTable "items")
  . around withApp $
  describe "Deleting" $ do
    context "existing record" $ do
      it "succeeds with 204 and deletion count" $
        request methodDelete "/items?id=eq.1" [] ""
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Nothing
          , matchStatus  = 204
          , matchHeaders = ["Content-Range" <:> "*/1"]
          }

      it "actually clears items out of the db" $ do
        _ <- request methodDelete "/items?id=lt.15" [] ""
        get "/items?id=lte.15"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just "[{\"id\":15}]"
          , matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "0-0/1"]
          }

    context "known route, unknown record" $
      it "fails with 404" $
        request methodDelete "/items?id=eq.101" [] "" `shouldRespondWith` 404

    context "totally unknown route" $
      it "fails with 404" $
        request methodDelete "/foozle?id=eq.101" [] "" `shouldRespondWith` 404
