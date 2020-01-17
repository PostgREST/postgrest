module Feature.PgVersion96Spec where

import Network.HTTP.Types
import Network.Wai        (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith Application
spec =
  describe "features supported on PostgreSQL 9.6" $ do
    context "GUC headers on function calls" $ do
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
        get "/rpc/bad_guc_headers_1"
          `shouldRespondWith`
          [json|{"message":"response.headers guc must be a JSON array composed of objects with a single key and a string value"}|]
          { matchStatus  = 500
          , matchHeaders = [ matchContentTypeJson ]
          }
        get "/rpc/bad_guc_headers_2"
          `shouldRespondWith`
          [json|{"message":"response.headers guc must be a JSON array composed of objects with a single key and a string value"}|]
          { matchStatus  = 500
          , matchHeaders = [ matchContentTypeJson ]
          }
        get "/rpc/bad_guc_headers_3"
          `shouldRespondWith`
          [json|{"message":"response.headers guc must be a JSON array composed of objects with a single key and a string value"}|]
          { matchStatus  = 500
          , matchHeaders = [ matchContentTypeJson ]
          }
        post "/rpc/bad_guc_headers_1" [json|{}|]
          `shouldRespondWith`
          [json|{"message":"response.headers guc must be a JSON array composed of objects with a single key and a string value"}|]
          { matchStatus  = 500
          , matchHeaders = [ matchContentTypeJson ]
          }

      it "can set the same http header twice" $
        get "/rpc/set_cookie_twice"
          `shouldRespondWith` "null"
          {matchHeaders = [
              matchContentTypeJson,
              "Set-Cookie" <:> "sessionid=38afes7a8; HttpOnly; Path=/",
              "Set-Cookie" <:> "id=a3fWa; Expires=Wed, 21 Oct 2015 07:28:00 GMT; Secure; HttpOnly"]}

    context "GUC headers on all other methods via pre-request" $ do
      it "succeeds setting the headers on GET and HEAD" $ do
        request methodGet "/items?id=eq.1" [("User-Agent", "MSIE 6.0")] mempty
          `shouldRespondWith` [json|[{"id": 1}]|]
          {matchHeaders = [
              matchContentTypeJson,
              "Cache-Control" <:> "no-cache, no-store, must-revalidate"]}

        request methodHead "/items?id=eq.1" [("User-Agent", "MSIE 7.0")] mempty
          `shouldRespondWith` ""
          {matchHeaders = ["Cache-Control" <:> "no-cache, no-store, must-revalidate"]}

        request methodHead "/projects" [("Accept", "text/csv")] mempty
          `shouldRespondWith` ""
          {matchHeaders = ["Content-Disposition" <:> "attachment; filename=projects.csv"]}

      it "succeeds setting the headers on POST" $
        request methodPost "/items" [] [json|[{"id": 11111}]|]
          `shouldRespondWith` ""
          { matchStatus = 201
          , matchHeaders = ["X-Custom-Header" <:> "mykey=myval"]
          }

      it "succeeds setting the headers on PATCH" $
        request methodPatch "/items?id=eq.11111" [] [json|[{"id": 11111}]|]
          `shouldRespondWith` ""
          { matchStatus = 204
          , matchHeaders = ["X-Custom-Header" <:> "mykey=myval"]
          }

      it "succeeds setting the headers on PUT" $
        request methodPut "/items?id=eq.11111" [] [json|[{"id": 11111}]|]
          `shouldRespondWith` ""
          { matchStatus = 204
          , matchHeaders = ["X-Custom-Header" <:> "mykey=myval"]
          }

      it "succeeds setting the headers on DELETE" $
        request methodDelete "/items?id=eq.11111" [] mempty
          `shouldRespondWith` ""
          { matchStatus = 204
          , matchHeaders = ["X-Custom-Header" <:> "mykey=myval"]
          }

    context "Override provided headers by using GUC headers" $ do
      it "can override the Content-Type header" $ do
        request methodHead "/clients?id=eq.1" [] mempty
          `shouldRespondWith` ""
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/geo+json"]
          }
        request methodHead "/rpc/getallprojects" [] mempty
          `shouldRespondWith` ""
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/geo+json"]
          }

      it "can override the Location header" $
        request methodPost "/stuff" [] [json|[{"id": 1, "name": "stuff 1"}]|]
          `shouldRespondWith` ""
          { matchStatus = 201
          , matchHeaders = ["Location" <:> "/stuff?id=eq.1&overriden=true"]
          }

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
