module Feature.QuerySpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders))

import SpecHelper

spec :: Spec
spec =
  beforeAll (clearTable "items" >> createItems 15)
   . beforeAll (clearTable "nullable_integer" >> createNullInteger)
   . beforeAll (
       clearTable "no_pk" >>
       createNulls 2 >>
       createLikableStrings >>
       createJsonData)
   . afterAll_ (clearTable "items" >> clearTable "no_pk" >> clearTable "simple_pk")
   . around withApp $ do

  describe "Querying a table with a column called count" $
    it "should not confuse count column with pg_catalog.count aggregate" $
      get "/has_count_column" `shouldRespondWith` 200

  describe "Querying a nonexistent table" $
    it "causes a 404" $
      get "/faketable" `shouldRespondWith` 404

  describe "Filtering response" $ do
    it "matches with equality" $
      get "/items?id=eq.5"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":5}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-0/1"]
        }

    it "matches items IN" $
      get "/items?id=in.1,3,5"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":3},{"id":5}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "matches nulls in varchar and numeric fields alike" $ do
      get "/no_pk?a=is.null" `shouldRespondWith`
        [json| [{"a": null, "b": null}] |]

      get "/nullable_integer?a=is.null" `shouldRespondWith` "[{\"a\":null}]"

    it "matches with like" $ do
      get "/simple_pk?k=like.*yx" `shouldRespondWith`
        "[{\"k\":\"xyyx\",\"extra\":\"u\"}]"
      get "/simple_pk?k=like.xy*" `shouldRespondWith`
        "[{\"k\":\"xyyx\",\"extra\":\"u\"}]"
      get "/simple_pk?k=like.*YY*" `shouldRespondWith`
        "[{\"k\":\"xYYx\",\"extra\":\"v\"}]"

    it "matches with ilike" $ do
      get "/simple_pk?k=ilike.xy*&order=extra.asc" `shouldRespondWith`
        "[{\"k\":\"xyyx\",\"extra\":\"u\"},{\"k\":\"xYYx\",\"extra\":\"v\"}]"
      get "/simple_pk?k=ilike.*YY*&order=extra.asc" `shouldRespondWith`
        "[{\"k\":\"xyyx\",\"extra\":\"u\"},{\"k\":\"xYYx\",\"extra\":\"v\"}]"

    it "matches with tsearch @@" $
      get "/tsearch?text_search_vector=@@.foo" `shouldRespondWith`
        "[{\"text_search_vector\":\"'bar':2 'foo':1\"}]"

    it "matches with computed column" $
      get "/items?always_true=eq.true" `shouldRespondWith`
        "[{\"id\":1},{\"id\":2},{\"id\":3},{\"id\":4},{\"id\":5},{\"id\":6},{\"id\":7},{\"id\":8},{\"id\":9},{\"id\":10},{\"id\":11},{\"id\":12},{\"id\":13},{\"id\":14},{\"id\":15}]"

  describe "ordering response" $ do
    it "by a column asc" $
      get "/items?id=lte.2&order=id.asc"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":2}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/2"]
        }
    it "by a column desc" $
      get "/items?id=lte.2&order=id.desc"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":2},{"id":1}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/2"]
        }

    it "by a column asc with nulls last" $
      get "/no_pk?order=a.asc.nullslast"
        `shouldRespondWith` ResponseMatcher {
          matchBody = Just [json| [{"a":"1","b":"0"},
                              {"a":"2","b":"0"},
                              {"a":null,"b":null}] |]
        , matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "by a column desc with nulls first" $
      get "/no_pk?order=a.desc.nullsfirst"
        `shouldRespondWith` ResponseMatcher {
          matchBody = Just [json| [{"a":null,"b":null},
                              {"a":"2","b":"0"},
                              {"a":"1","b":"0"}] |]
        , matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "by a column desc with nulls last" $
      get "/no_pk?order=a.desc.nullslast"
        `shouldRespondWith` ResponseMatcher {
          matchBody = Just [json| [{"a":"2","b":"0"},
                              {"a":"1","b":"0"},
                              {"a":null,"b":null}] |]
        , matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "without other constraints" $
      get "/items?order=asc.id" `shouldRespondWith` 200

  describe "Accept headers" $
    it "should respond with CSV to 'text/csv' request" $
      request methodGet "/simple_pk"
              (acceptHdrs "text/csv") ""
        `shouldRespondWith` ResponseMatcher {
          matchBody = Just "k,extra\rxyyx,u\rxYYx,v"
        , matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "text/csv"]
        }

  describe "Canonical location" $ do
    it "Sets Content-Location with alphabetized params" $
      get "/no_pk?b=eq.1&a=eq.1"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just "[]"
        , matchStatus  = 200
        , matchHeaders = ["Content-Location" <:> "/no_pk?a=eq.1&b=eq.1"]
        }

    it "Omits question mark when there are no params" $ do
      r <- get "/simple_pk"
      liftIO $ do
        let respHeaders = simpleHeaders r
        respHeaders `shouldSatisfy` matchHeader
          "Content-Location" "/simple_pk"

  describe "jsonb" $
    it "can filter by properties inside json column" $ do
      get "/json?data->foo->>bar=eq.baz" `shouldRespondWith`
        [json| [{"data": {"foo": {"bar": "baz"}}}] |]
      get "/json?data->foo->>bar=eq.fake" `shouldRespondWith`
        [json| [] |]
