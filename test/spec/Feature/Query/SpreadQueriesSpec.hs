module Feature.Query.SpreadQueriesSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "spread embeds" $ do
    it "works on a many-to-one relationship" $ do
      get "/projects?select=id,..clients(client_name:name)" `shouldRespondWith`
        [json|[
          {"id":1,"client_name":"Microsoft"},
          {"id":2,"client_name":"Microsoft"},
          {"id":3,"client_name":"Apple"},
          {"id":4,"client_name":"Apple"},
          {"id":5,"client_name":null}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }
      get "/grandchild_entities?select=name,..child_entities(parent_name:name,..entities(grandparent_name:name))&limit=3" `shouldRespondWith`
        [json|[
          {"name":"grandchild entity 1","parent_name":"child entity 1","grandparent_name":"entity 1"},
          {"name":"grandchild entity 2","parent_name":"child entity 1","grandparent_name":"entity 1"},
          {"name":"grandchild entity 3","parent_name":"child entity 2","grandparent_name":"entity 1"}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }
      get "/videogames?select=name,..computed_designers(designer_name:name)" `shouldRespondWith`
        [json|[
          {"name":"Civilization I","designer_name":"Sid Meier"},
          {"name":"Civilization II","designer_name":"Sid Meier"},
          {"name":"Final Fantasy I","designer_name":"Hironobu Sakaguchi"},
          {"name":"Final Fantasy II","designer_name":"Hironobu Sakaguchi"}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "works inside a normal embed" $
      get "/grandchild_entities?select=name,child_entity:child_entities(name,..entities(parent_name:name))&limit=1" `shouldRespondWith`
        [json|[
          {"name":"grandchild entity 1","child_entity":{"name":"child entity 1","parent_name":"entity 1"}}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "works on a one-to-one relationship" $
      get "/country?select=name,..capital(capital:name)" `shouldRespondWith`
        [json|[
          {"name":"Afghanistan","capital":"Kabul"},
          {"name":"Algeria","capital":"Algiers"}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "fails when is not a to-one relationship" $ do
      get "/clients?select=*,..projects(*)" `shouldRespondWith`
        [json|{
          "code":"PGRST119",
          "details":"'clients' and 'projects' do not form a many-to-one or one-to-one relationship",
          "hint":null,
          "message":"A spread operation on 'projects' is not possible"
        }|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }
      get "/designers?select=*,..computed_videogames(*)" `shouldRespondWith`
        [json|{
          "code":"PGRST119",
          "details":"'designers' and 'computed_videogames' do not form a many-to-one or one-to-one relationship",
          "hint":null,
          "message":"A spread operation on 'computed_videogames' is not possible"
        }|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }
