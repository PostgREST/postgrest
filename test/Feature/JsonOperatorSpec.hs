module Feature.JsonOperatorSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types

import SpecHelper
import Network.Wai (Application)

import Protolude hiding (get)

spec :: SpecWith Application
spec = describe "json and jsonb operators" $ do
  context "Shaping response with select parameter" $ do
    it "obtains a json subfield one level with casting" $
      get "/complex_items?id=eq.1&select=settings->>foo::json" `shouldRespondWith`
        [json| [{"foo":{"int":1,"bar":"baz"}}] |] -- the value of foo here is of type "text"
        { matchHeaders = [matchContentTypeJson] }

    it "renames json subfield one level with casting" $
      get "/complex_items?id=eq.1&select=myFoo:settings->>foo::json" `shouldRespondWith`
        [json| [{"myFoo":{"int":1,"bar":"baz"}}] |] -- the value of foo here is of type "text"
        { matchHeaders = [matchContentTypeJson] }

    it "fails on bad casting (data of the wrong format)" $
      get "/complex_items?select=settings->foo->>bar::integer"
        `shouldRespondWith` [json| {"hint":null,"details":null,"code":"22P02","message":"invalid input syntax for integer: \"baz\""} |]
        { matchStatus  = 400 , matchHeaders = [] }

    it "obtains a json subfield two levels (string)" $
      get "/complex_items?id=eq.1&select=settings->foo->>bar" `shouldRespondWith`
        [json| [{"bar":"baz"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "renames json subfield two levels (string)" $
      get "/complex_items?id=eq.1&select=myBar:settings->foo->>bar" `shouldRespondWith`
        [json| [{"myBar":"baz"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "obtains a json subfield two levels with casting (int)" $
      get "/complex_items?id=eq.1&select=settings->foo->>int::integer" `shouldRespondWith`
        [json| [{"int":1}] |] -- the value in the db is an int, but here we expect a string for now
        { matchHeaders = [matchContentTypeJson] }

    it "renames json subfield two levels with casting (int)" $
      get "/complex_items?id=eq.1&select=myInt:settings->foo->>int::integer" `shouldRespondWith`
        [json| [{"myInt":1}] |] -- the value in the db is an int, but here we expect a string for now
        { matchHeaders = [matchContentTypeJson] }

  context "filtering response" $ do
    it "can filter by properties inside json column" $ do
      get "/json?data->foo->>bar=eq.baz" `shouldRespondWith`
        [json| [{"data": {"id": 1, "foo": {"bar": "baz"}}}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/json?data->foo->>bar=eq.fake" `shouldRespondWith`
        [json| [] |]
        { matchHeaders = [matchContentTypeJson] }

    it "can filter by properties inside json column using not" $
      get "/json?data->foo->>bar=not.eq.baz" `shouldRespondWith`
        [json| [] |]
        { matchHeaders = [matchContentTypeJson] }

    it "can filter by properties inside json column using ->>" $
      get "/json?data->>id=eq.1" `shouldRespondWith`
        [json| [{"data": {"id": 1, "foo": {"bar": "baz"}}}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "can be filtered with and/or" $
      get "/grandchild_entities?or=(jsonb_col->a->>b.eq.foo, jsonb_col->>b.eq.bar)&select=id" `shouldRespondWith`
        [json|[{id: 4}, {id: 5}]|] { matchStatus = 200, matchHeaders = [matchContentTypeJson] }

  context "ordering response" $ do
    it "orders by a json column property asc" $
      get "/json?order=data->>id.asc" `shouldRespondWith`
        [json| [{"data": {"id": 0}}, {"data": {"id": 1, "foo": {"bar": "baz"}}}, {"data": {"id": 3}}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "orders by a json column with two level property nulls first" $
      get "/json?order=data->foo->>bar.nullsfirst" `shouldRespondWith`
        [json| [{"data": {"id": 3}}, {"data": {"id": 0}}, {"data": {"id": 1, "foo": {"bar": "baz"}}}] |]
        { matchHeaders = [matchContentTypeJson] }

  context "Patching record, in a nonempty table" $ do
    it "can set a json column to escaped value" $ do
      _ <- post "/json" [json| { data: {"escaped":"bar"} } |]
      request methodPatch "/json?data->>escaped=eq.bar"
                   [("Prefer", "return=representation")]
                   [json| { "data": { "escaped":" \"bar" } } |]
        `shouldRespondWith` [json| [{ "data": { "escaped":" \"bar" } }] |]
        { matchStatus  = 200 , matchHeaders = [] }
