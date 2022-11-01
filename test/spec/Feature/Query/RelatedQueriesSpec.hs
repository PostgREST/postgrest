module Feature.Query.RelatedQueriesSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "related orders" $ do
    it "works on a many-to-one relationship" $ do
      get "/projects?select=id,clients(name)&order=clients(name).nullsfirst" `shouldRespondWith`
        [json|[
          {"id":5,"clients":null},
          {"id":3,"clients":{"name":"Apple"}},
          {"id":4,"clients":{"name":"Apple"}},
          {"id":1,"clients":{"name":"Microsoft"}},
          {"id":2,"clients":{"name":"Microsoft"}} ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }
      get "/projects?select=id,client:clients(name)&order=client(name).asc" `shouldRespondWith`
        [json|[
          {"id":3,"client":{"name":"Apple"}},
          {"id":4,"client":{"name":"Apple"}},
          {"id":1,"client":{"name":"Microsoft"}},
          {"id":2,"client":{"name":"Microsoft"}},
          {"id":5,"client":null} ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }
      get "/videogames?select=id,computed_designers(id)&order=computed_designers(id).desc" `shouldRespondWith`
        [json|[
          {"id":3,"computed_designers":{"id":2}},
          {"id":4,"computed_designers":{"id":2}},
          {"id":1,"computed_designers":{"id":1}},
          {"id":2,"computed_designers":{"id":1}}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "works on a one-to-one relationship and jsonb column" $ do
      get "/trash?select=id,trash_details(id,jsonb_col)&order=trash_details(jsonb_col->key).asc" `shouldRespondWith`
        [json|[
          {"id":2,"trash_details":{"id":2,"jsonb_col":{"key": 6}}},
          {"id":3,"trash_details":{"id":3,"jsonb_col":{"key": 8}}},
          {"id":1,"trash_details":{"id":1,"jsonb_col":{"key": 10}}}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }
      get "/trash?select=id,trash_details(id,jsonb_col)&order=trash_details(jsonb_col->key).desc" `shouldRespondWith`
        [json|[
          {"id":1,"trash_details":{"id":1,"jsonb_col":{"key": 10}}},
          {"id":3,"trash_details":{"id":3,"jsonb_col":{"key": 8}}},
          {"id":2,"trash_details":{"id":2,"jsonb_col":{"key": 6}}}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "works on an embedded resource" $ do
      get "/users?select=name,tasks(id,name,projects(id,name))&tasks.order=projects(id).desc&limit=1" `shouldRespondWith`
        [json| [{
          "name":"Angela Martin",
          "tasks":[
            {"id": 3, "name":"Design w10","projects":{"id":2,"name":"Windows 10"}},
            {"id": 4, "name":"Code w10","projects":{"id":2,"name":"Windows 10"}},
            {"id": 1, "name":"Design w7","projects":{"id":1,"name":"Windows 7"}},
            {"id": 2, "name":"Code w7","projects":{"id":1,"name":"Windows 7"}}
          ]
        }]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }
      get "/users?select=name,tasks(id,name,projects(id,name))&tasks.order=projects(id).desc,name&limit=1" `shouldRespondWith`
        [json| [{
          "name":"Angela Martin",
          "tasks":[
            {"id": 4, "name":"Code w10","projects":{"id":2,"name":"Windows 10"}},
            {"id": 3, "name":"Design w10","projects":{"id":2,"name":"Windows 10"}},
            {"id": 2, "name":"Code w7","projects":{"id":1,"name":"Windows 7"}},
            {"id": 1, "name":"Design w7","projects":{"id":1,"name":"Windows 7"}}
          ]
        }]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }
      get "/users?select=name,tasks(id,name,projects(id,name))&tasks.order=projects(id).asc&limit=1" `shouldRespondWith`
        [json|[{
          "name":"Angela Martin",
          "tasks":[
            {"id":1,"name":"Design w7","projects":{"id":1,"name":"Windows 7"}},
            {"id":2,"name":"Code w7","projects":{"id":1,"name":"Windows 7"}},
            {"id":3,"name":"Design w10","projects":{"id":2,"name":"Windows 10"}},
            {"id":4,"name":"Code w10","projects":{"id":2,"name":"Windows 10"}}
          ]
        }]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "fails when is not a to-one relationship" $ do
      get "/clients?select=*,projects(*)&order=projects(id)" `shouldRespondWith`
        [json|{"code":"PGRST118","details":null,"hint":null,"message":"'clients' and 'projects' do not form a many-to-one or one-to-one relationship"}|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }
      get "/clients?select=*,pros:projects(*)&order=pros(id)" `shouldRespondWith`
        [json|{"code":"PGRST118","details":null,"hint":null,"message":"'clients' and 'pros' do not form a many-to-one or one-to-one relationship"}|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }
      get "/designers?select=id,computed_videogames(id)&order=computed_videogames(id).desc" `shouldRespondWith`
        [json|{"code":"PGRST118","details":null,"hint":null,"message":"'designers' and 'computed_videogames' do not form a many-to-one or one-to-one relationship"}|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

    it "fails when the resource is not embedded" $
      get "/projects?select=id,clients(name)&order=clientsx(name).nullsfirst" `shouldRespondWith`
        [json|{
          "code":"PGRST108",
          "details":null,
          "hint":"Verify that 'clientsx' is included in the 'select' query parameter.",
          "message":"'clientsx' is not an embedded resource in this request"
        }|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }
