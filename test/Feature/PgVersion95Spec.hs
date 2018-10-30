module Feature.PgVersion95Spec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import SpecHelper
import Network.Wai (Application)

import Protolude hiding (get)

spec :: SpecWith Application
spec = describe "features supported on PostgreSQL 9.5" $
  context "json array negative index" $ do
    it "can select with negative indexes" $ do
      get "/json_arr?select=data->>-1::int&id=in.(1,2)" `shouldRespondWith`
        [json| [{"data":3}, {"data":6}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data->0->>-2::int&id=in.(3,4)" `shouldRespondWith`
        [json| [{"data":8}, {"data":7}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data->-2->>a&id=in.(5,6)" `shouldRespondWith`
        [json| [{"a":"A"}, {"a":"[1,2,3]"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "can filter with negative indexes" $ do
      get "/json_arr?select=data&data->>-3=eq.1" `shouldRespondWith`
        [json| [{"data":[1, 2, 3]}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data&data->-1->>-3=eq.11" `shouldRespondWith`
        [json| [{"data":[[9, 8, 7], [11, 12, 13]]}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data&data->-1->>b=eq.B" `shouldRespondWith`
        [json| [{"data":[{"a": "A"}, {"b": "B"}]}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data&data->-1->b->>-1=eq.5" `shouldRespondWith`
        [json| [{"data":[{"a": [1,2,3]}, {"b": [4,5]}]}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "should fail on badly formed negatives" $ do
      get "/json_arr?select=data->>-78xy" `shouldRespondWith`
        [json|
          {"details": "unexpected 'x' expecting digit, \"->\", \"::\" or end of input",
           "message": "\"failed to parse select parameter (data->>-78xy)\" (line 1, column 11)"} |]
        { matchStatus = 400, matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data->>--34" `shouldRespondWith`
        [json|
          {"details": "unexpected \"-\" expecting digit",
           "message": "\"failed to parse select parameter (data->>--34)\" (line 1, column 9)"} |]
        { matchStatus = 400, matchHeaders = [matchContentTypeJson] }
      get "/json_arr?select=data->>-xy-4" `shouldRespondWith`
        [json|
          {"details":"unexpected \"x\" expecting digit",
           "message":"\"failed to parse select parameter (data->>-xy-4)\" (line 1, column 9)"} |]
        { matchStatus = 400, matchHeaders = [matchContentTypeJson] }
