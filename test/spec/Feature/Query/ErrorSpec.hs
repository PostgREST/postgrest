module Feature.Query.ErrorSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude hiding (get)

nonExistentSchema :: SpecWith ((), Application)
nonExistentSchema = do
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

pgErrorCodeMapping :: SpecWith ((), Application)
pgErrorCodeMapping = do
  describe "PostreSQL error code mappings" $ do
    it "should return 500 for cardinality_violation" $
      get "/bad_subquery" `shouldRespondWith` 500

    it "should return 500 for statement too complex" $
      request methodPost "/infinite_inserts"
        []
        [json|{"id": 3, "name": "qwer"}|]
        `shouldRespondWith`
        [json|
          {"code": "54001",
           "details": null,
           "hint": "Increase the configuration parameter \"max_stack_depth\" (currently 2048kB), after ensuring the platform's stack depth limit is adequate.",
            "message": "stack depth limit exceeded"}|]
        { matchStatus = 500 }
