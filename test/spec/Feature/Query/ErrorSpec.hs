module Feature.Query.ErrorSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude hiding (get)

spec :: SpecWith ((), Application)
spec = do
  describe "Non existent api schema" $ do
    it "succeeds when requesting root path" $
      get "/" `shouldRespondWith` 200

    it "gives 404 when requesting a nonexistent table in this nonexistent schema" $
      get "/nonexistent_table" `shouldRespondWith` 404

  describe "Non existent URL" $ do
    it "gives 404 on a single nested route" $
      get "/projects/nested" `shouldRespondWith` 404

    it "gives 404 on a double nested route" $
      get "/projects/nested/double" `shouldRespondWith` 404

  describe "Unsupported HTTP methods" $ do
    it "should return 405 for CONNECT method" $
      request methodConnect "/"
          []
          ""
        `shouldRespondWith`
          [json|
            {"hint": null,
             "details": null,
             "code": "PGRST117",
             "message":"Unsupported HTTP method: CONNECT"}|]
          { matchStatus = 405 }

    it "should return 405 for TRACE method" $
      request methodTrace "/"
          []
          ""
        `shouldRespondWith`
          [json|
            {"hint": null,
             "details": null,
             "code": "PGRST117",
             "message":"Unsupported HTTP method: TRACE"}|]
          { matchStatus = 405 }

    it "should return 405 for OTHER method" $
      request "OTHER" "/"
          []
          ""
        `shouldRespondWith`
          [json|
            {"hint": null,
             "details": null,
             "code": "PGRST117",
             "message":"Unsupported HTTP method: OTHER"}|]
          { matchStatus = 405 }
