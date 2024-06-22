module Feature.Query.ErrorSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude hiding (get)

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
