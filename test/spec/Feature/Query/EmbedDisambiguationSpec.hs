module Feature.Query.EmbedDisambiguationSpec where

import Network.Wai (Application)

import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "resource embedding disambiguation" $ do
    context "ambiguous requests that give 300 Multiple Choices" $ do
      it "errs when there's a table and view that point to the same fk" $
        get "/message?select=id,body,sender(name,sent)" `shouldRespondWith`
          [json|
            {
              "details": [
                {
                    "cardinality": "many-to-one",
                    "relationship": "message_sender_fkey using message(sender) and person(id)",
                    "embedding": "message with person"
                },
                {
                    "cardinality": "many-to-one",
                    "relationship": "message_sender_fkey using message(sender) and person_detail(id)",
                    "embedding": "message with person_detail"
                }
              ],
              "hint": "Try changing 'sender' to one of the following: 'person!message_sender_fkey', 'person_detail!message_sender_fkey'. Find the desired relationship in the 'details' key.",
              "message": "Could not embed because more than one relationship was found for 'message' and 'sender'",
              "code": "PGRST201"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

      it "errs when there's a table and view that point to the same fk (composite pk)" $
        get "/activities?select=fst_shift(*)" `shouldRespondWith`
          [json|
            {
              "details": [
                {
                    "cardinality": "one-to-many",
                    "relationship": "fst_shift using activities(id, schedule_id) and unit_workdays(fst_shift_activity_id, fst_shift_schedule_id)",
                    "embedding": "activities with unit_workdays"
                },
                {
                    "cardinality": "one-to-many",
                    "relationship": "fst_shift using activities(id, schedule_id) and unit_workdays_fst_shift(fst_shift_activity_id, fst_shift_schedule_id)",
                    "embedding": "activities with unit_workdays_fst_shift"
                }
              ],
              "hint": "Try changing 'fst_shift' to one of the following: 'unit_workdays!fst_shift', 'unit_workdays_fst_shift!fst_shift'. Find the desired relationship in the 'details' key.",
              "message": "Could not embed because more than one relationship was found for 'activities' and 'fst_shift'",
              "code": "PGRST201"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

      it "errs when there are o2m and m2m cardinalities to the target table" $
        get "/sites?select=*,big_projects(*)" `shouldRespondWith`
          [json|
            {
              "details": [
                {
                  "cardinality": "many-to-one",
                  "relationship": "main_project using sites(main_project_id) and big_projects(big_project_id)",
                  "embedding": "sites with big_projects"
                },
                {
                  "cardinality": "many-to-many",
                  "relationship": "jobs using jobs_site_id_fkey(site_id) and jobs_big_project_id_fkey(big_project_id)",
                  "embedding": "sites with big_projects"
                },
                {
                  "cardinality": "many-to-many",
                  "relationship": "main_jobs using jobs_site_id_fkey(site_id) and jobs_big_project_id_fkey(big_project_id)",
                  "embedding": "sites with big_projects"
                }
              ],
              "hint": "Try changing 'big_projects' to one of the following: 'big_projects!main_project', 'big_projects!jobs', 'big_projects!main_jobs'. Find the desired relationship in the 'details' key.",
              "message": "Could not embed because more than one relationship was found for 'sites' and 'big_projects'",
              "code": "PGRST201"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

      it "errs on an ambiguous embed that has a circular reference" $
        get "/agents?select=*,departments(*)" `shouldRespondWith`
          [json|
            {
              "details": [
                {
                    "cardinality": "one-to-many",
                    "relationship": "departments_head_id_fkey using agents(id) and departments(head_id)",
                    "embedding": "agents with departments"
                },
                {
                    "cardinality": "many-to-one",
                    "relationship": "agents_department_id_fkey using agents(department_id) and departments(id)",
                    "embedding": "agents with departments"
                }
              ],
              "hint": "Try changing 'departments' to one of the following: 'departments!departments_head_id_fkey', 'departments!agents_department_id_fkey'. Find the desired relationship in the 'details' key.",
              "message": "Could not embed because more than one relationship was found for 'agents' and 'departments'",
              "code": "PGRST201"
            }
           |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

      it "errs when there are more than two fks on a junction table(currently impossible to disambiguate, only choice is to split the table)" $
        -- We have 4 possibilities for doing the junction JOIN here.
        -- This could be solved by specifying two additional fks, like whatev_projects!fk1!fk2(*)
        -- If the need arises this capability can be added later without causing a breaking change
        get "/whatev_sites?select=*,whatev_projects(*)" `shouldRespondWith`
          [json|
            {
              "details": [
                {
                  "cardinality": "many-to-many",
                  "relationship": "whatev_jobs using whatev_jobs_site_id_1_fkey(site_id_1) and whatev_jobs_project_id_1_fkey(project_id_1)",
                  "embedding": "whatev_sites with whatev_projects"
                },
                {
                  "cardinality": "many-to-many",
                  "relationship": "whatev_jobs using whatev_jobs_site_id_1_fkey(site_id_1) and whatev_jobs_project_id_2_fkey(project_id_2)",
                  "embedding": "whatev_sites with whatev_projects"
                },
                {
                  "cardinality": "many-to-many",
                  "relationship": "whatev_jobs using whatev_jobs_site_id_2_fkey(site_id_2) and whatev_jobs_project_id_1_fkey(project_id_1)",
                  "embedding": "whatev_sites with whatev_projects"
                },
                {
                  "cardinality": "many-to-many",
                  "relationship": "whatev_jobs using whatev_jobs_site_id_2_fkey(site_id_2) and whatev_jobs_project_id_2_fkey(project_id_2)",
                  "embedding": "whatev_sites with whatev_projects"
                }
              ],
              "hint": "Try changing 'whatev_projects' to one of the following: 'whatev_projects!whatev_jobs', 'whatev_projects!whatev_jobs', 'whatev_projects!whatev_jobs', 'whatev_projects!whatev_jobs'. Find the desired relationship in the 'details' key.",
              "message": "Could not embed because more than one relationship was found for 'whatev_sites' and 'whatev_projects'",
              "code": "PGRST201"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

    context "disambiguating requests with embed hints" $ do

      context "using FK to specify the relationship" $ do
        it "can embed by FK name" $
          get "/projects?id=in.(1,3)&select=id,name,client(id,name)" `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7","client":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client":{"id":2,"name":"Apple"}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can embed by FK name and select the FK column at the same time" $
          get "/projects?id=in.(1,3)&select=id,name,client_id,client(id,name)" `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7","client_id":1,"client":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":2,"client":{"id":2,"name":"Apple"}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can embed parent with view!fk and grandparent by using fk" $
          get "/tasks?id=eq.1&select=id,name,projects_view!project(id,name,client(id,name))" `shouldRespondWith`
            [json|[{"id":1,"name":"Design w7","projects_view":{"id":1,"name":"Windows 7","client":{"id":1,"name":"Microsoft"}}}]|]

        it "can embed by using a composite FK name" $
          get "/unit_workdays?select=unit_id,day,fst_shift(car_id,schedule(name)),snd_shift(camera_id,schedule(name))" `shouldRespondWith`
            [json| [
              {
                "day": "2019-12-02",
                "fst_shift": {
                    "car_id": "CAR-349",
                    "schedule": {
                        "name": "morning"
                    }
                },
                "snd_shift": {
                    "camera_id": "CAM-123",
                    "schedule": {
                        "name": "night"
                    }
                },
                "unit_id": 1
              }
            ] |]
            { matchHeaders = [matchContentTypeJson] }

        it "embeds by using two fks pointing to the same table" $
          get "/orders?id=eq.1&select=id, name, billing(address), shipping(address)" `shouldRespondWith`
            [json|[{"id":1,"name":"order 1","billing":{"address": "address 1"},"shipping":{"address": "address 2"}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "fails if the fk is not known" $
          get "/message?select=id,sender:person!space(name)&id=lt.4" `shouldRespondWith`
            [json|{
              "hint":"Verify that 'message' and 'person' exist in the schema 'test' and that there is a foreign key relationship between them. If a new relationship was created, try reloading the schema cache.",
              "message":"Could not find a relationship between 'message' and 'person' in the schema cache",
              "code": "PGRST200",
              "details": null}|]
            { matchStatus = 400
            , matchHeaders = [matchContentTypeJson] }

        it "can request a parent with fk" $
          get "/comments?select=content,user(name)" `shouldRespondWith`
            [json|[ { "content": "Needs to be delivered ASAP", "user": { "name": "Angela Martin" } } ]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can request two parents with fks" $
          get "/articleStars?select=createdAt,article(id),user(name)&limit=1"
            `shouldRespondWith`
              [json|[{"createdAt":"2015-12-08T04:22:57.472738","article":{"id": 1},"user":{"name": "Angela Martin"}}]|]

        it "can specify a view!fk" $
          get "/message?select=id,body,sender:person_detail!message_sender_fkey(name,sent),recipient:person_detail!message_recipient_fkey(name,received)&id=lt.4" `shouldRespondWith`
            [json|
              [{"id":1,"body":"Hello Jane","sender":{"name":"John","sent":2},"recipient":{"name":"Jane","received":2}},
               {"id":2,"body":"Hi John","sender":{"name":"Jane","sent":1},"recipient":{"name":"John","received":1}},
               {"id":3,"body":"How are you doing?","sender":{"name":"John","sent":2},"recipient":{"name":"Jane","received":2}}] |]
            { matchHeaders = [matchContentTypeJson] }

        it "can specify a table!fk hint and request children 2 levels" $
          get "/clients?id=eq.1&select=id,projects:projects!client(id,tasks(id))" `shouldRespondWith`
            [json|[{"id":1,"projects":[{"id":1,"tasks":[{"id":1},{"id":2}]},{"id":2,"tasks":[{"id":3},{"id":4}]}]}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can disambiguate with the fk in case of an o2m and m2m relationship to the same table" $
          get "/sites?select=name,main_project(name)&site_id=eq.1" `shouldRespondWith`
            [json| [ { "name": "site 1", "main_project": { "name": "big project 1" } } ] |]
            { matchHeaders = [matchContentTypeJson] }

      context "using the column name of the FK to specify the relationship" $ do
        it "can embed by column" $
          get "/projects?id=in.(1,3)&select=id,name,client_id(id,name)" `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7","client_id":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":{"id":2,"name":"Apple"}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can embed by column and select the column at the same time, if aliased" $
          get "/projects?id=in.(1,3)&select=id,name,client_id,client:client_id(id,name)" `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7","client_id":1,"client":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":2,"client":{"id":2,"name":"Apple"}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can embed parent by using view!column and grandparent by using the column" $
          get "/tasks?id=eq.1&select=id,name,project:projects_view!project_id(id,name,client:client_id(id,name))" `shouldRespondWith`
            [json|[{"id":1,"name":"Design w7","project":{"id":1,"name":"Windows 7","client":{"id":1,"name":"Microsoft"}}}]|]

        it "can specify table!column" $
          get "/message?select=id,body,sender:person!sender(name),recipient:person!recipient(name)&id=lt.4" `shouldRespondWith`
            [json|
              [{"id":1,"body":"Hello Jane","sender":{"name":"John"},"recipient":{"name":"Jane"}},
               {"id":2,"body":"Hi John","sender":{"name":"Jane"},"recipient":{"name":"John"}},
               {"id":3,"body":"How are you doing?","sender":{"name":"John"},"recipient":{"name":"Jane"}}] |]
            { matchHeaders = [matchContentTypeJson] }

        it "will embed using a column that has uppercase chars" $
          get "/ghostBusters?select=escapeId(*)" `shouldRespondWith`
            [json| [{"escapeId":{"so6meIdColumn":1}},{"escapeId":{"so6meIdColumn":3}},{"escapeId":{"so6meIdColumn":5}}] |]
            { matchHeaders = [matchContentTypeJson] }

        it "embeds by using two columns pointing to the same table" $
          get "/orders?id=eq.1&select=id, name, billing_address_id(id), shipping_address_id(id)" `shouldRespondWith`
            [json|[{"id":1,"name":"order 1","billing_address_id":{"id":1},"shipping_address_id":{"id":2}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can disambiguate with the column in case of an o2m and m2m relationship to the same table" $
          get "/sites?select=name,main_project_id(name)&site_id=eq.1" `shouldRespondWith`
            [json| [ { "name": "site 1", "main_project_id": { "name": "big project 1" } } ] |]
            { matchHeaders = [matchContentTypeJson] }

      context "using the junction to disambiguate the request" $
        it "can specify the junction of an m2m relationship" $ do
          get "/sites?select=*,big_projects!jobs(name)&site_id=in.(1,2)" `shouldRespondWith`
            [json|
              [
                {
                  "big_projects": [
                    {
                      "name": "big project 1"
                    }
                  ],
                  "main_project_id": 1,
                  "name": "site 1",
                  "site_id": 1
                },
                {
                  "big_projects": [
                    {
                      "name": "big project 1"
                    },
                    {
                      "name": "big project 2"
                    }
                  ],
                  "main_project_id": null,
                  "name": "site 2",
                  "site_id": 2
                }
              ]
            |]
          get "/sites?select=*,big_projects!main_jobs(name)&site_id=in.(1,2)" `shouldRespondWith`
            [json|
              [
                {
                  "big_projects": [
                    {
                      "name": "big project 1"
                    }
                  ],
                  "main_project_id": 1,
                  "name": "site 1",
                  "site_id": 1
                },
                {
                  "big_projects": [],
                  "main_project_id": null,
                  "name": "site 2",
                  "site_id": 2
                }
              ]
            |]
            { matchHeaders = [matchContentTypeJson] }

      context "using a FK column and a FK to specify the relationship" $
        it "embeds by using a column and a fk pointing to the same table" $
          get "/orders?id=eq.1&select=id, name, billing_address_id(id), shipping(id)" `shouldRespondWith`
            [json|[{"id":1,"name":"order 1","billing_address_id":{"id":1},"shipping":{"id":2}}]|]
            { matchHeaders = [matchContentTypeJson] }

    context "tables with self reference foreign keys" $ do
      context "one self reference foreign key" $ do
        it "embeds parents recursively" $
          get "/family_tree?id=in.(3,4)&select=id,parent(id,name,parent(*))" `shouldRespondWith`
            [json|[
              { "id": "3", "parent": { "id": "1", "name": "Parental Unit", "parent": null } },
              { "id": "4", "parent": { "id": "2", "name": "Kid One", "parent": { "id": "1", "name": "Parental Unit", "parent": null } } }
            ]|]
            { matchHeaders = [matchContentTypeJson] }

        it "embeds children recursively" $
          get "/family_tree?id=eq.1&select=id,name, children:family_tree!parent(id,name,children:family_tree!parent(id,name))" `shouldRespondWith`
            [json|[{
              "id": "1", "name": "Parental Unit", "children": [
                { "id": "2", "name": "Kid One", "children": [ { "id": "4", "name": "Grandkid One" } ] },
                { "id": "3", "name": "Kid Two", "children": [ { "id": "5", "name": "Grandkid Two" } ] }
              ]
            }]|] { matchHeaders = [matchContentTypeJson] }

        it "embeds parent and then embeds children" $
          get "/family_tree?id=eq.2&select=id,name,parent(id,name,children:family_tree!parent(id,name))" `shouldRespondWith`
            [json|[{
              "id": "2", "name": "Kid One", "parent": {
                "id": "1", "name": "Parental Unit", "children": [ { "id": "2", "name": "Kid One" }, { "id": "3", "name": "Kid Two"} ]
              }
            }]|] { matchHeaders = [matchContentTypeJson] }

        it "embeds parent and then embeds children on a view" $
          get "/job?select=id,parent_id(*),children:job!parent_id(id,parent_id)" `shouldRespondWith`
            [json|[
              {
                "id": 1,
                "parent_id": null,
                "children": [ { "id": 2, "parent_id": 1 } ]
              },
              {
                "id": 2,
                "parent_id": { "id": 1, "parent_id": null },
                "children": []
              }
            ]|] { matchHeaders = [matchContentTypeJson] }

      context "two self reference foreign keys" $ do
        it "embeds parents" $
          get "/organizations?select=id,name,referee(id,name),auditor(id,name)&id=eq.3" `shouldRespondWith`
            [json|[{
              "id": 3, "name": "Acme",
              "referee": {
                "id": 1,
                "name": "Referee Org"
              },
              "auditor": {
                "id": 2,
                "name": "Auditor Org"
              }
            }]|] { matchHeaders = [matchContentTypeJson] }

        it "embeds children" $ do
          get "/organizations?select=id,name,refereeds:organizations!referee(id,name)&id=eq.1" `shouldRespondWith`
            [json|[{
              "id": 1, "name": "Referee Org",
              "refereeds": [
                {
                  "id": 3,
                  "name": "Acme"
                },
                {
                  "id": 4,
                  "name": "Umbrella"
                }
              ]
            }]|] { matchHeaders = [matchContentTypeJson] }
          get "/organizations?select=id,name,auditees:organizations!auditor(id,name)&id=eq.2" `shouldRespondWith`
            [json|[{
              "id": 2, "name": "Auditor Org",
              "auditees": [
                {
                  "id": 3,
                  "name": "Acme"
                },
                {
                  "id": 4,
                  "name": "Umbrella"
                }
              ]
            }]|] { matchHeaders = [matchContentTypeJson] }

        it "embeds other relations(manager) besides the self reference" $ do
          get "/organizations?select=name,manager(name),referee(name,manager(name),auditor(name,manager(name))),auditor(name,manager(name),referee(name,manager(name)))&id=eq.5" `shouldRespondWith`
            [json|[{
              "name":"Cyberdyne",
              "manager":{"name":"Cyberdyne Manager"},
              "referee":{
                "name":"Acme",
                "manager":{"name":"Acme Manager"},
                "auditor":{
                  "name":"Auditor Org",
                  "manager":{"name":"Auditor Manager"}}},
              "auditor":{
                "name":"Umbrella",
                "manager":{"name":"Umbrella Manager"},
                "referee":{
                  "name":"Referee Org",
                  "manager":{"name":"Referee Manager"}}}
            }]|] { matchHeaders = [matchContentTypeJson] }

          get "/organizations?select=name,manager(name),auditees:organizations!auditor(name,manager(name),refereeds:organizations!referee(name,manager(name)))&id=eq.2" `shouldRespondWith`
            [json|[{
              "name":"Auditor Org",
              "manager":{"name":"Auditor Manager"},
              "auditees":[
                {"name":"Acme",
                 "manager":{"name":"Acme Manager"},
                 "refereeds":[
                   {"name":"Cyberdyne",
                    "manager":{"name":"Cyberdyne Manager"}},
                   {"name":"Oscorp",
                    "manager":{"name":"Oscorp Manager"}}]},
                {"name":"Umbrella",
                 "manager":{"name":"Umbrella Manager"},
                 "refereeds":[]}]
            }]|] { matchHeaders = [matchContentTypeJson] }

    context "m2m embed when there's a junction in an internal schema" $ do
      -- https://github.com/PostgREST/postgrest/issues/1736
      it "works with no ambiguity when there's an exposed view of the junction" $ do
        get "/screens?select=labels(name)" `shouldRespondWith`
          [json|[{"labels":[{"name":"fruit"}]}, {"labels":[{"name":"vehicles"}]}, {"labels":[{"name":"vehicles"}, {"name":"fruit"}]}]|]
          { matchHeaders = [matchContentTypeJson] }
        get "/actors?select=*,films(*)" `shouldRespondWith`
          [json|[ {"id":1,"name":"john","films":[{"id":12,"title":"douze commandements"}]},
                  {"id":2,"name":"mary","films":[{"id":2001,"title":"odyssée de l'espace"}]}]|]
          { matchHeaders = [matchContentTypeJson] }
      it "doesn't work if the junction is only internal" $
        get "/end_1?select=end_2(*)" `shouldRespondWith`
          [json|{
            "hint":"Verify that 'end_1' and 'end_2' exist in the schema 'test' and that there is a foreign key relationship between them. If a new relationship was created, try reloading the schema cache.",
            "message":"Could not find a relationship between 'end_1' and 'end_2' in the schema cache",
            "code":"PGRST200",
            "details": null}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson] }
      it "shouldn't try to embed if the private junction has an exposed homonym" $
        -- ensures the "invalid reference to FROM-clause entry for table "rollen" error doesn't happen.
        -- Ref: https://github.com/PostgREST/postgrest/issues/1587#issuecomment-734995669
        get "/schauspieler?select=filme(*)" `shouldRespondWith`
          [json|{
            "hint":"Verify that 'schauspieler' and 'filme' exist in the schema 'test' and that there is a foreign key relationship between them. If a new relationship was created, try reloading the schema cache.",
            "message":"Could not find a relationship between 'schauspieler' and 'filme' in the schema cache",
            "code":"PGRST200",
            "details": null}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson] }
