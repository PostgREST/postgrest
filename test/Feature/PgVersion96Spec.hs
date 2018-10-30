module Feature.PgVersion96Spec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import SpecHelper
import Network.Wai (Application)

import Protolude hiding (get)

spec :: SpecWith Application
spec =
  describe "features supported on PostgreSQL 9.6" $ do
    context "GUC headers" $ do
      it "succeeds setting the headers" $ do
        get "/rpc/get_projects_and_guc_headers?id=eq.2&select=id"
          `shouldRespondWith` [json|[{"id": 2}]|]
          {matchHeaders = [
              matchContentTypeJson,
              "X-Test"   <:> "key1=val1; someValue; key2=val2",
              "X-Test-2" <:> "key1=val1"]}
        get "/rpc/get_int_and_guc_headers?num=1"
          `shouldRespondWith` [json|1|]
          {matchHeaders = [
              matchContentTypeJson,
              "X-Test"   <:> "key1=val1; someValue; key2=val2",
              "X-Test-2" <:> "key1=val1"]}
        post "/rpc/get_int_and_guc_headers" [json|{"num": 1}|]
          `shouldRespondWith` [json|1|]
          {matchHeaders = [
              matchContentTypeJson,
              "X-Test"   <:> "key1=val1; someValue; key2=val2",
              "X-Test-2" <:> "key1=val1"]}

      it "fails when setting headers with wrong json structure" $ do
        get "/rpc/bad_guc_headers_1" `shouldRespondWith` 500
        get "/rpc/bad_guc_headers_2" `shouldRespondWith` 500
        get "/rpc/bad_guc_headers_3" `shouldRespondWith` 500
        post "/rpc/bad_guc_headers_1" [json|{}|] `shouldRespondWith` 500

      it "can set the same http header twice" $
        get "/rpc/set_cookie_twice"
          `shouldRespondWith` "null"
          {matchHeaders = [
              matchContentTypeJson,
              "Set-Cookie" <:> "sessionid=38afes7a8; HttpOnly; Path=/",
              "Set-Cookie" <:> "id=a3fWa; Expires=Wed, 21 Oct 2015 07:28:00 GMT; Secure; HttpOnly"]}

    context "Use of the phraseto_tsquery function" $ do
      it "finds matches" $
        get "/tsearch?text_search_vector=phfts.The%20Fat%20Cats" `shouldRespondWith`
          [json| [{"text_search_vector": "'ate':3 'cat':2 'fat':1 'rat':4" }] |]
          { matchHeaders = [matchContentTypeJson] }

      it "finds matches with different dictionaries" $
        get "/tsearch?text_search_vector=phfts(german).Art%20Spass" `shouldRespondWith`
          [json| [{"text_search_vector": "'art':4 'spass':5 'unmog':7" }] |]
          { matchHeaders = [matchContentTypeJson] }

      it "can be negated with not operator" $
        get "/tsearch?text_search_vector=not.phfts(english).The%20Fat%20Cats" `shouldRespondWith`
          [json| [
            {"text_search_vector": "'fun':5 'imposs':9 'kind':3"},
            {"text_search_vector": "'also':2 'fun':3 'possibl':8"},
            {"text_search_vector": "'amus':5 'fair':7 'impossibl':9 'peu':4"},
            {"text_search_vector": "'art':4 'spass':5 'unmog':7"}]|]
          { matchHeaders = [matchContentTypeJson] }

      it "can be used with or query param" $
        get "/tsearch?or=(text_search_vector.phfts(german).Art%20Spass, text_search_vector.phfts(french).amusant, text_search_vector.fts(english).impossible)" `shouldRespondWith`
          [json|[
            {"text_search_vector": "'fun':5 'imposs':9 'kind':3" },
            {"text_search_vector": "'amus':5 'fair':7 'impossibl':9 'peu':4" },
            {"text_search_vector": "'art':4 'spass':5 'unmog':7"}
          ]|] { matchHeaders = [matchContentTypeJson] }

      it "should work when used with GET RPC" $
        get "/rpc/get_tsearch?text_search_vector=phfts(english).impossible" `shouldRespondWith`
          [json|[{"text_search_vector":"'fun':5 'imposs':9 'kind':3"}]|]
          { matchHeaders = [matchContentTypeJson] }
