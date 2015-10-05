module Feature.QuerySpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders))

import SpecHelper

spec :: Spec
spec =
  beforeAll (clearTable "items" >> createItems 15)
   . beforeAll (clearTable "complex_items" >> createComplexItems)
   . beforeAll (clearTable "nullable_integer" >> createNullInteger)
   . beforeAll (
       clearTable "no_pk" >>
       createNulls 2 >>
       createLikableStrings >>
       createJsonData)
   . afterAll_ (clearTable "items" >> clearTable "complex_items" >> clearTable "no_pk" >> clearTable "simple_pk")
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

    it "matches with equality using not operator" $
      get "/items?id=not.eq.5"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-13/14"]
        }

    it "matches with more than one condition using not operator" $
      get "/simple_pk?k=like.*yx&extra=not.eq.u" `shouldRespondWith` "[]"

    it "matches with inequality using not operator" $ do
      get "/items?id=not.lt.14&order=id.asc"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":14},{"id":15}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/2"]
        }
      get "/items?id=not.gt.2&order=id.asc"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":2}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/2"]
        }

    it "matches items IN" $
      get "/items?id=in.1,3,5"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":3},{"id":5}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "matches items NOT IN" $
      get "/items?id=notin.2,4,6,7,8,9,10,11,12,13,14,15"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":3},{"id":5}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "matches items NOT IN using not operator" $
      get "/items?id=not.in.2,4,6,7,8,9,10,11,12,13,14,15"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":3},{"id":5}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "matches nulls using not operator" $
      get "/no_pk?a=not.is.null" `shouldRespondWith`
        [json| [{"a":"1","b":"0"},{"a":"2","b":"0"}] |]

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

    it "matches with like using not operator" $
      get "/simple_pk?k=not.like.*yx" `shouldRespondWith`
        "[{\"k\":\"xYYx\",\"extra\":\"v\"}]"

    it "matches with ilike" $ do
      get "/simple_pk?k=ilike.xy*&order=extra.asc" `shouldRespondWith`
        "[{\"k\":\"xyyx\",\"extra\":\"u\"},{\"k\":\"xYYx\",\"extra\":\"v\"}]"
      get "/simple_pk?k=ilike.*YY*&order=extra.asc" `shouldRespondWith`
        "[{\"k\":\"xyyx\",\"extra\":\"u\"},{\"k\":\"xYYx\",\"extra\":\"v\"}]"

    it "matches with ilike using not operator" $
      get "/simple_pk?k=not.ilike.xy*&order=extra.asc" `shouldRespondWith` "[]"

    it "matches with tsearch @@" $
      get "/tsearch?text_search_vector=@@.foo" `shouldRespondWith`
        [json| [{"text_search_vector":"'bar':2 'foo':1"}] |]

    it "matches with tsearch @@ using not operator" $
      get "/tsearch?text_search_vector=not.@@.foo" `shouldRespondWith`
        [json| [{"text_search_vector":"'baz':1 'qux':2"}] |]

    it "matches with computed column" $
      get "/items?always_true=eq.true" `shouldRespondWith`
        [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]

    it "matches filtering nested items" $
      get "/clients?select=id,projects(id,tasks(id,name))&projects.tasks.name=like.Design*" `shouldRespondWith`
        "[{\"id\":1,\"projects\":[{\"id\":1,\"tasks\":[{\"id\":1,\"name\":\"Design w7\"}]},{\"id\":2,\"tasks\":[{\"id\":3,\"name\":\"Design w10\"}]}]},{\"id\":2,\"projects\":[{\"id\":3,\"tasks\":[{\"id\":5,\"name\":\"Design IOS\"}]},{\"id\":4,\"tasks\":[{\"id\":7,\"name\":\"Design OSX\"}]}]}]"

  describe "Shaping response with select parameter" $ do

    it "selectStar works in absense of parameter" $
      get "/complex_items?id=eq.3" `shouldRespondWith`
        "[{\"id\":3,\"name\":\"Three\",\"settings\":{\"foo\":{\"int\":1,\"bar\":\"baz\"}}}]"

    it "one simple column" $
      get "/complex_items?select=id" `shouldRespondWith`
        [json| [{"id":1},{"id":2},{"id":3}] |]

    it "one simple column with casting (text)" $
      get "/complex_items?select=id::text" `shouldRespondWith`
        [json| [{"id":"1"},{"id":"2"},{"id":"3"}] |]

    it "json column" $
      get "/complex_items?id=eq.1&select=settings" `shouldRespondWith`
        [json| [{"settings":{"foo":{"int":1,"bar":"baz"}}}] |]

    it "json subfield one level with casting (json)" $
      get "/complex_items?id=eq.1&select=settings->>foo::json" `shouldRespondWith`
        [json| [{"foo":{"int":1,"bar":"baz"}}] |] -- the value of foo here is of type "text"

    it "fails on bad casting (data of the wrong format)" $
      get "/complex_items?select=settings->foo->>bar::integer"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| {"hint":null,"details":null,"code":"22P02","message":"invalid input syntax for integer: \"baz\""} |]
        , matchStatus  = 400
        , matchHeaders = []
        }

    it "fails on bad casting (wrong cast type)" $
      get "/complex_items?select=id::fakecolumntype"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| {"hint":null,"details":null,"code":"42704","message":"type \"fakecolumntype\" does not exist"} |]
        , matchStatus  = 400
        , matchHeaders = []
        }


    it "json subfield two levels (string)" $
      get "/complex_items?id=eq.1&select=settings->foo->>bar" `shouldRespondWith`
        [json| [{"bar":"baz"}] |]


    it "json subfield two levels with casting (int)" $
      get "/complex_items?id=eq.1&select=settings->foo->>int::integer" `shouldRespondWith`
        [json| [{"int":1}] |] -- the value in the db is an int, but here we expect a string for now

    it "requesting parents and children" $
      get "/projects?id=eq.1&select=id, name, clients(*), tasks(id, name)" `shouldRespondWith`
        "[{\"id\":1,\"name\":\"Windows 7\",\"clients\":{\"id\":1,\"name\":\"Microsoft\"},\"tasks\":[{\"id\":1,\"name\":\"Design w7\"},{\"id\":2,\"name\":\"Code w7\"}]}]"

    it "requesting children 2 levels" $
      get "/clients?id=eq.1&select=id,projects(id,tasks(id))" `shouldRespondWith`
        "[{\"id\":1,\"projects\":[{\"id\":1,\"tasks\":[{\"id\":1},{\"id\":2}]},{\"id\":2,\"tasks\":[{\"id\":3},{\"id\":4}]}]}]"

    it "requesting many<->many relation" $
      get "/tasks?select=id,users(id)" `shouldRespondWith`
        "[{\"id\":1,\"users\":[{\"id\":1},{\"id\":3}]},{\"id\":2,\"users\":[{\"id\":1}]},{\"id\":3,\"users\":[{\"id\":1}]},{\"id\":4,\"users\":[{\"id\":1}]},{\"id\":5,\"users\":[{\"id\":2},{\"id\":3}]},{\"id\":6,\"users\":[{\"id\":2}]},{\"id\":7,\"users\":[{\"id\":2}]},{\"id\":8,\"users\":null}]"

    it "requesting parents and children on views" $
      get "/projects_view?id=eq.1&select=id, name, clients(*), tasks(id, name)" `shouldRespondWith`
        "[{\"id\":1,\"name\":\"Windows 7\",\"clients\":{\"id\":1,\"name\":\"Microsoft\"},\"tasks\":[{\"id\":1,\"name\":\"Design w7\"},{\"id\":2,\"name\":\"Code w7\"}]}]"

    it "requesting children with composite key" $ do
      pendingWith "have to resolve issue #302"
      get "/users_tasks?user_id=eq.2&task_id=eq.6&select=*, comments(content)" `shouldRespondWith`
        [json| [{"user_id":2,"task_id":6,"comments":[{"content": "Needs to be delivered ASAP"}]}] |]


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
      get "/items?order=id.asc" `shouldRespondWith` 200

  describe "Accept headers" $ do
    it "should respond an unknown accept type with 415" $
      request methodGet "/simple_pk"
              (acceptHdrs "text/unknowntype") ""
        `shouldRespondWith` 415

    it "should respond correctly to */* in accept header" $
      request methodGet "/simple_pk"
              (acceptHdrs "*/*") ""
        `shouldRespondWith` 200

    it "should respond correctly to multiple types in accept header" $
      request methodGet "/simple_pk"
              (acceptHdrs "text/unknowntype, text/csv") ""
        `shouldRespondWith` 200

    it "should respond with CSV to 'text/csv' request" $
      request methodGet "/simple_pk"
              (acceptHdrs "text/csv; version=1") ""
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just "k,extra\rxyyx,u\rxYYx,v"
        , matchStatus  = 200
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

  describe "jsonb" $ do
    it "can filter by properties inside json column" $ do
      get "/json?data->foo->>bar=eq.baz" `shouldRespondWith`
        [json| [{"data": {"foo": {"bar": "baz"}}}] |]
      get "/json?data->foo->>bar=eq.fake" `shouldRespondWith`
        [json| [] |]
    it "can filter by properties inside json column using not" $
      get "/json?data->foo->>bar=not.eq.baz" `shouldRespondWith`
        [json| [] |]

  describe "remote procedure call" $ do
    context "a proc that returns a set" . before_ (clearTable "items" >> createItems 10) .
      after_ (clearTable "items") $
      it "returns proper json" $
        post "/rpc/getitemrange" [json| { "min": 2, "max": 4 } |] `shouldRespondWith`
          [json| [ {"id": 3}, {"id":4} ] |]

    context "a proc that returns plain text" $
      it "returns proper json" $
        post "/rpc/sayhello" [json| { "name": "world" } |] `shouldRespondWith`
          [json| [{"sayhello":"Hello, world"}] |]
