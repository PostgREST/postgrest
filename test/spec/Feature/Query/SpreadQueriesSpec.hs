module Feature.Query.SpreadQueriesSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

aggDisabledSpec :: SpecWith ((), Application)
aggDisabledSpec =
  describe "spread embeds" $ do
    it "works on a many-to-one relationship" $ do
      get "/projects?select=id,...clients(client_name:name)" `shouldRespondWith`
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
      get "/grandchild_entities?select=name,...child_entities(parent_name:name,...entities(grandparent_name:name))&limit=3" `shouldRespondWith`
        [json|[
          {"name":"grandchild entity 1","parent_name":"child entity 1","grandparent_name":"entity 1"},
          {"name":"grandchild entity 2","parent_name":"child entity 1","grandparent_name":"entity 1"},
          {"name":"grandchild entity 3","parent_name":"child entity 2","grandparent_name":"entity 1"}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }
      get "/videogames?select=name,...computed_designers(designer_name:name)" `shouldRespondWith`
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
      get "/grandchild_entities?select=name,child_entity:child_entities(name,...entities(parent_name:name))&limit=1" `shouldRespondWith`
        [json|[
          {"name":"grandchild entity 1","child_entity":{"name":"child entity 1","parent_name":"entity 1"}}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "works on a one-to-one relationship" $
      get "/country?select=name,...capital(capital:name)" `shouldRespondWith`
        [json|[
          {"name":"Afghanistan","capital":"Kabul"},
          {"name":"Algeria","capital":"Algiers"}
        ]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "fails when it's a one-to-many relationship and aggregates are disabled" $ do
      get "/clients?select=*,...projects(*)" `shouldRespondWith`
        [json|{
          "hint":null,
          "details":null,
          "code":"PGRST123",
          "message":"Use of aggregate functions is not allowed"
        }|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }
      get "/designers?select=*,...computed_videogames(*)" `shouldRespondWith`
        [json|{
          "hint":null,
          "details":null,
          "code":"PGRST123",
          "message":"Use of aggregate functions is not allowed"
        }|]
        { matchStatus = 400
        , matchHeaders = [matchContentTypeJson]
        }

    it "fails when it's a many-to-many relationship and aggregates are disabled" $ do
      get "/supervisors?select=*,...processes(*)" `shouldRespondWith`
        [json|{
          "hint":null,
          "details":null,
          "code":"PGRST123",
          "message":"Use of aggregate functions is not allowed"
        }|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

    it "can include or exclude attributes of the junction on a m2m" $ do
      get "/users?select=*,tasks:users_tasks(*,...tasks(*))&limit=1" `shouldRespondWith`
        [json|[{
          "id":1,"name":"Angela Martin",
          "tasks": [
            {"user_id":1,"task_id":1,"id":1,"name":"Design w7","project_id":1},
            {"user_id":1,"task_id":2,"id":2,"name":"Code w7","project_id":1},
            {"user_id":1,"task_id":3,"id":3,"name":"Design w10","project_id":2},
            {"user_id":1,"task_id":4,"id":4,"name":"Code w10","project_id":2}
          ]
        }]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }
      get "/users?select=*,tasks:users_tasks(...tasks(*))&limit=1" `shouldRespondWith`
        [json|[{
          "id":1,"name":"Angela Martin",
          "tasks":[
            {"id":1,"name":"Design w7","project_id":1},
            {"id":2,"name":"Code w7","project_id":1},
            {"id":3,"name":"Design w10","project_id":2},
            {"id":4,"name":"Code w10","project_id":2}
          ]
        }]|]
        { matchStatus  = 200
        , matchHeaders = [matchContentTypeJson]
        }

aggEnabledSpec :: SpecWith ((), Application)
aggEnabledSpec =
  describe "spread embeds" $ do
    context "one-to-many relationships as array aggregates" $ do
      it "should aggregate a single spread column" $ do
        get "/factories?select=factory:name,...processes(name)&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","name":["Process B1", "Process B2"]},
            {"factory":"Factory A","name":["Process A1", "Process A2"]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
        get "/factories?select=factory:name,...processes(processes:name)&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","processes":["Process B1", "Process B2"]},
            {"factory":"Factory A","processes":["Process A1", "Process A2"]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate many spread columns" $ do
        get "/factories?select=factory:name,...processes(name,category_id)&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","name":["Process B1", "Process B2"],"category_id":[1, 1]},
            {"factory":"Factory A","name":["Process A1", "Process A2"],"category_id":[1, 2]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
        get "/factories?select=factory:name,...processes(processes:name,categories:category_id)&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","processes":["Process B1", "Process B2"],"categories":[1, 1]},
            {"factory":"Factory A","processes":["Process A1", "Process A2"],"categories":[1, 2]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should return an empty array when no elements are found" $
        get "/factories?select=factory:name,...processes(processes:name)&processes=is.null" `shouldRespondWith`
          [json|[
            {"factory":"Factory D","processes":[]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should work when selecting all columns, aggregating each one of them" $
        get "/factories?select=factory:name,...processes(*)&id=lte.2&order=name" `shouldRespondWith`
          [json|[
            {"factory":"Factory A","id":[1,2],"name":["Process A1","Process A2"],"factory_id":[1,1],"category_id":[1,2]},
            {"factory":"Factory B","id":[3,4],"name":["Process B1","Process B2"],"factory_id":[2,2],"category_id":[1,1]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate spread columns from a nested one-to-one relationship" $
        get "/factories?select=factory:name,...processes(process:name,...process_costs(process_costs:cost))&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","process":["Process B1", "Process B2"],"process_costs":[180.00, 70.00]},
            {"factory":"Factory A","process":["Process A1", "Process A2"],"process_costs":[150.00, 200.00]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate spread columns from a nested many-to-one relationship" $
        get "/factories?select=factory:name,...processes(process:name,...process_categories(categories:name))&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","process":["Process B1", "Process B2"],"categories":["Batch", "Batch"]},
            {"factory":"Factory A","process":["Process A1", "Process A2"],"categories":["Batch", "Mass"]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate spread columns from a nested one-to-many relationship" $
        get "/factories?select=factory:name,...processes(process:name,...process_supervisor(supervisor_ids:supervisor_id))&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","process":["Process B1", "Process B1", "Process B2", "Process B2"],"supervisor_ids":[3, 4, 1, 2]},
            {"factory":"Factory A","process":["Process A1", "Process A2"],"supervisor_ids":[1, 2]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate spread columns from a nested many-to-many relationship" $ do
        get "/factories?select=factory:name,...processes(process:name,...supervisors(supervisors:name))&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","process":["Process B1", "Process B1", "Process B2", "Process B2"],"supervisors":["Peter", "Sarah", "Mary", "John"]},
            {"factory":"Factory A","process":["Process A1", "Process A2"],"supervisors":["Mary", "John"]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate a nested non-spread one-to-one relationship into an array of objects" $ do
        get "/factories?select=factory:name,...processes(process:name,process_costs(cost))&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","process":["Process B1", "Process B2"],"process_costs":[{"cost": 180.00}, {"cost": 70.00}]},
            {"factory":"Factory A","process":["Process A1", "Process A2"],"process_costs":[{"cost": 150.00}, {"cost": 200.00}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate a nested non-spread many-to-one relationship into an array of objects" $
        get "/factories?select=factory:name,...processes(process:name,process_categories(name))&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","process":["Process B1", "Process B2"],"process_categories":[{"name": "Batch"}, {"name": "Batch"}]},
            {"factory":"Factory A","process":["Process A1", "Process A2"],"process_categories":[{"name": "Batch"}, {"name": "Mass"}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate a nested non-spread one-to-many relationship into an array of objects" $
        get "/factories?select=factory:name,...processes(process:name,process_supervisor(supervisor_id))&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","process":["Process B1","Process B1","Process B2","Process B2"],"process_supervisor":[{"supervisor_id": 3},{"supervisor_id": 4},{"supervisor_id": 1},{"supervisor_id": 2}]},
            {"factory":"Factory A","process":["Process A1","Process A2"],"process_supervisor":[{"supervisor_id": 1},{"supervisor_id": 2}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate a nested non-spread many-to-many relationship into an array of objects" $
        get "/factories?select=factory:name,...processes(process:name,supervisors(name))&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory B","process":["Process B1", "Process B1", "Process B2", "Process B2"],"supervisors":[{"name": "Peter"}, {"name": "Sarah"}, {"name": "Mary"}, {"name": "John"}]},
            {"factory":"Factory A","process":["Process A1", "Process A2"],"supervisors":[{"name": "Mary"}, {"name": "John"}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should work when selecting all columns in a nested to-one resource, aggregating each one of them" $
        get "/factories?select=factory:name,...processes(*,...process_costs(*))&id=lte.2" `shouldRespondWith`
          [json|[
            {"factory":"Factory A","id":[1,2],"name":["Process A1","Process A2"],"factory_id":[1,1],"category_id":[1,2],"process_id":[1,2],"cost":[150.00,200.00]},
            {"factory":"Factory B","id":[3,4],"name":["Process B1","Process B2"],"factory_id":[2,2],"category_id":[1,1],"process_id":[3,4],"cost":[180.00,70.00]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }

    context "many-to-many relationships as array aggregates" $ do
      it "should aggregate a single spread column" $ do
        get "/operators?select=operator:name,...processes(name)&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","name":["Process A1","Process A2","Process B2"]},
            {"operator":"Louis","name":["Process A1","Process A2"]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
        get "/operators?select=operator:name,...processes(processes:name)&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","processes":["Process A1","Process A2","Process B2"]},
            {"operator":"Louis","processes":["Process A1","Process A2"]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate many spread columns" $ do
        get "/operators?select=operator:name,...processes(name,category_id)&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","name":["Process A1","Process A2","Process B2"],"category_id":[1,2,1]},
            {"operator":"Louis","name":["Process A1","Process A2"],"category_id":[1,2]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
        get "/operators?select=operator:name,...processes(processes:name,categories:category_id)&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","processes":["Process A1","Process A2","Process B2"],"categories":[1,2,1]},
            {"operator":"Louis","processes":["Process A1","Process A2"],"categories":[1,2]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should return an empty array when no elements are found" $
        get "/operators?select=operator:name,...processes(processes:name)&processes=is.null" `shouldRespondWith`
          [json|[
            {"operator":"Liz","processes":[]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should work when selecting all columns, aggregating each one of them" $
        get "/operators?select=operator:name,...processes(*)&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","id":[1,2,4],"name":["Process A1","Process A2","Process B2"],"factory_id":[1,1,2],"category_id":[1,2,1]},
            {"operator":"Louis","id":[1,2],"name":["Process A1","Process A2"],"factory_id":[1,1],"category_id":[1,2]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate spread columns from a nested one-to-one relationship" $
        get "/operators?select=operator:name,...processes(process:name,...process_costs(process_costs:cost))&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","process":["Process A1","Process A2","Process B2"],"process_costs":[150.00,200.00,70.00]},
            {"operator":"Louis","process":["Process A1","Process A2"],"process_costs":[150.00,200.00]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate spread columns from a nested many-to-one relationship" $
        get "/operators?select=operator:name,...processes(process:name,...process_categories(categories:name))&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","process":["Process A1","Process A2","Process B2"],"categories":["Batch","Mass","Batch"]},
            {"operator":"Louis","process":["Process A1","Process A2"],"categories":["Batch","Mass"]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate spread columns from a nested one-to-many relationship" $
        get "/operators?select=operator:name,...processes(process:name,...process_supervisor(supervisor_ids:supervisor_id))&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","process":["Process A1","Process A2","Process B2","Process B2"],"supervisor_ids":[1,2,1,2]},
            {"operator":"Louis","process":["Process A1","Process A2"],"supervisor_ids":[1,2]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate spread columns from a nested many-to-many relationship" $ do
        get "/operators?select=operator:name,...processes(process:name,...supervisors(supervisors:name))&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","process":["Process A1","Process A2","Process B2","Process B2"],"supervisors":["Mary","John","Mary","John"]},
            {"operator":"Louis","process":["Process A1","Process A2"],"supervisors":["Mary","John"]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate a nested non-spread one-to-one relationship into an array of objects" $ do
        get "/operators?select=operator:name,...processes(process:name,process_costs(cost))&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","process":["Process A1","Process A2","Process B2"],"process_costs":[{"cost": 150.00},{"cost": 200.00},{"cost": 70.00}]},
            {"operator":"Louis","process":["Process A1","Process A2"],"process_costs":[{"cost": 150.00},{"cost": 200.00}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate a nested non-spread many-to-one relationship into an array of objects" $
        get "/operators?select=operator:name,...processes(process:name,process_categories(name))&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","process":["Process A1","Process A2","Process B2"],"process_categories":[{"name": "Batch"},{"name": "Mass"},{"name": "Batch"}]},
            {"operator":"Louis","process":["Process A1","Process A2"],"process_categories":[{"name": "Batch"},{"name": "Mass"}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate a nested non-spread one-to-many relationship into an array of objects" $
        get "/operators?select=operator:name,...processes(process:name,process_supervisor(supervisor_id))&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","process":["Process A1","Process A2","Process B2","Process B2"],"process_supervisor":[{"supervisor_id": 1},{"supervisor_id": 2},{"supervisor_id": 1},{"supervisor_id": 2}]},
            {"operator":"Louis","process":["Process A1","Process A2"],"process_supervisor":[{"supervisor_id": 1},{"supervisor_id": 2}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate a nested non-spread many-to-many relationship into an array of objects" $
        get "/operators?select=operator:name,...processes(process:name,supervisors(name))&id=lte.2" `shouldRespondWith`
          [json|[
            {"operator":"Anne","process":["Process A1","Process A2","Process B2","Process B2"],"supervisors":[{"name": "Mary"},{"name": "John"},{"name": "Mary"},{"name": "John"}]},
            {"operator":"Louis","process":["Process A1","Process A2"],"supervisors":[{"name": "Mary"},{"name": "John"}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should not aggregate to-one relationships when they're nested inside a non-spread relationship, even if the latter is nested in a to-many spread" $
        get "/supervisors?select=name,...process_supervisor(processes(name,...process_costs(cost)))&id=lte.2" `shouldRespondWith`
          [json|[
            {"name":"Mary","processes":[{"cost": 150.00, "name": "Process A1"}, {"cost": 70.00, "name": "Process B2"}]},
            {"name":"John","processes":[{"cost": 200.00, "name": "Process A2"}, {"cost": 70.00, "name": "Process B2"}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
      it "should aggregate to-many relationships when they're nested inside a non-spread relationship" $
        get "/supervisors?select=name,...process_supervisor(processes(name,...operators(operators:name)))&id=lte.2" `shouldRespondWith`
          [json|[
            {"name":"John","processes":[{"name": "Process A2", "operators": ["Anne", "Louis", "Jeff"]}, {"name": "Process B2", "operators": ["Anne", "Jeff"]}]},
            {"name":"Mary","processes":[{"name": "Process A1", "operators": ["Anne", "Louis"]}, {"name": "Process B2", "operators": ["Anne", "Jeff"]}]}
          ]|]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
          }
