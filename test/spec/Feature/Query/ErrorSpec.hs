module Feature.Query.ErrorSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

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

    context "includes the proxy-status header on the response" $ do
      it "works with ApiRequest error" $
        get "/invalid/nested/paths"
          `shouldRespondWith`
          [json| {"code":"PGRST125","details":null,"hint":null,"message":"Invalid path specified in request URL"} |]
          { matchStatus  = 404
          , matchHeaders = ["Proxy-Status" <:> "PostgREST; error=PGRST125"]
          }

      it "works with SchemaCache error" $
        get "/non_existent_table"
          `shouldRespondWith`
          [json| {"code":"PGRST205","details":null,"hint":"Perhaps you meant the table 'test.json_table'","message":"Could not find the table 'test.non_existent_table' in the schema cache"} |]
          { matchStatus  = 404
          , matchHeaders = ["Proxy-Status" <:> "PostgREST; error=PGRST205"]
          }

      it "works with Jwt error" $ do
        let auth = authHeaderJWT "ey9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
        request methodGet "/authors_only" [auth] ""
          `shouldRespondWith`
          [json| {"message":"Expected 3 parts in JWT; got 2","code":"PGRST301","hint":null,"details":null} |]
          { matchStatus = 401
          , matchHeaders = ["Proxy-Status" <:> "PostgREST; error=PGRST301"]
          }

      it "works with raise sqlstate custom error" $
        get "/rpc/raise_pt402"
          `shouldRespondWith`
          [json| {"code":"PT402","details":"Quota exceeded","hint":"Upgrade your plan","message":"Payment Required"} |]
          { matchStatus  = 402
          , matchHeaders = ["Proxy-Status" <:> "PostgREST; error=PT402"]
          }

      it "works with sqlstate PGRST custom error" $
        get "/rpc/raise_sqlstate_test1"
          `shouldRespondWith`
          [json| {"code":"123","details":"DEF","hint":"XYZ","message":"ABC"} |]
          { matchStatus  = 332
          , matchHeaders = ["Proxy-Status" <:> "PostgREST; error=123"]
          }
