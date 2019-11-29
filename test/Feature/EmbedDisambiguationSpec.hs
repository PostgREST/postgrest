module Feature.EmbedDisambiguationSpec where

import Network.Wai (Application)

import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Text.Heredoc

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith Application
spec =
  describe "resource embedding disambiguation" $ do
    context "ambiguous requests that give 300 Multiple Choices" $ do
      it "errs when there's a table and view that point to the same fk" $
        get "/message?select=id,body,sender(name,sent)" `shouldRespondWith`
          [json|
            {
              "details": [
                {
                    "cardinality": "m2o",
                    "relationship": "sender",
                    "source": "test.message",
                    "target": "test.person"
                },
                {
                    "cardinality": "m2o",
                    "relationship": "sender",
                    "source": "test.message",
                    "target": "test.person_detail"
                }
              ],
              "hint": "By following the 'details' key, disambiguate the request by changing the url to /source?select=relationship(*) or /source?select=target!<relationship|cardinality|junction>(*)",
              "message": "More than one relationship was found for message and sender"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

      it "errs when there's a self reference and no cardinality is specified" $
        get "/web_content?select=*,web_content(*)" `shouldRespondWith`
          [json|
            {
              "details": [
                {
                    "cardinality": "m2o",
                    "relationship": "p_web_id",
                    "source": "test.web_content",
                    "target": "test.web_content"
                },
                {
                    "cardinality": "o2m",
                    "relationship": "p_web_id",
                    "source": "test.web_content",
                    "target": "test.web_content"
                }
              ],
              "hint": "By following the 'details' key, disambiguate the request by changing the url to /source?select=relationship(*) or /source?select=target!<relationship|cardinality|junction>(*)",
              "message": "More than one relationship was found for web_content and web_content"
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
                    "cardinality": "m2o",
                    "relationship": "sites_main_project_id_fkey",
                    "source": "test.sites",
                    "target": "test.big_projects"
                },
                {
                    "cardinality": "m2m",
                    "junction": "test.jobs",
                    "source": "test.sites",
                    "target": "test.big_projects"
                },
                {
                    "cardinality": "m2m",
                    "junction": "test.main_jobs",
                    "source": "test.sites",
                    "target": "test.big_projects"
                }
              ],
              "hint": "By following the 'details' key, disambiguate the request by changing the url to /source?select=relationship(*) or /source?select=target!<relationship|cardinality|junction>(*)",
              "message": "More than one relationship was found for sites and big_projects"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

      it "errs when there are two junctions on an m2m relationship" $
        get "/sites?select=*,big_projects!m2m(*)" `shouldRespondWith`
          [json|
            {
              "details": [
                  {
                      "cardinality": "m2m",
                      "junction": "test.jobs",
                      "source": "test.sites",
                      "target": "test.big_projects"
                  },
                  {
                      "cardinality": "m2m",
                      "junction": "test.main_jobs",
                      "source": "test.sites",
                      "target": "test.big_projects"
                  }
              ],
              "hint": "By following the 'details' key, disambiguate the request by changing the url to /source?select=relationship(*) or /source?select=target!<relationship|cardinality|junction>(*)",
              "message": "More than one relationship was found for sites and big_projects"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

      it "errs when there are two junctions on a m2m cardinality" $
        get "/sites?select=*,big_projects!m2m(*)" `shouldRespondWith`
          [json|
            {
              "details": [
                  {
                      "cardinality": "m2m",
                      "junction": "test.jobs",
                      "source": "test.sites",
                      "target": "test.big_projects"
                  },
                  {
                      "cardinality": "m2m",
                      "junction": "test.main_jobs",
                      "source": "test.sites",
                      "target": "test.big_projects"
                  }
              ],
              "hint": "By following the 'details' key, disambiguate the request by changing the url to /source?select=relationship(*) or /source?select=target!<relationship|cardinality|junction>(*)",
              "message": "More than one relationship was found for sites and big_projects"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

      it "errs on a circular reference" $
        get "/agents?select=*,departments(*)" `shouldRespondWith`
          [json|
            {
              "details": [
                {
                  "cardinality": "m2o",
                  "relationship": "agents_department_id_fkey",
                  "source": "test.agents",
                  "target": "test.departments"
                },
                {
                  "cardinality": "o2m",
                  "relationship": "departments_head_id_fkey",
                  "source": "test.agents",
                  "target": "test.departments"
                }
              ],
              "hint": "By following the 'details' key, disambiguate the request by changing the url to /source?select=relationship(*) or /source?select=target!<relationship|cardinality|junction>(*)",
              "message": "More than one relationship was found for agents and departments"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

      it "errs when there are more than two fks on a junction table(currently impossible to disambiguate, only choice is to split the table)" $
        -- We have 4 possibilities for doing the junction JOIN here, using these columns:
        -- [site_id_1][project_id_1]
        -- [site_id_1][project_id_2]
        -- [site_id_2][project_id_1]
        -- [site_id_2][project_id_2]
        get "/whatev_sites?select=*,whatev_projects!m2m(*)" `shouldRespondWith`
          [json|
            {
              "details": [
                  {
                    "cardinality": "m2m",
                    "junction": "test.whatev_jobs",
                    "source": "test.whatev_sites",
                    "target": "test.whatev_projects"
                  },
                  {
                    "cardinality": "m2m",
                    "junction": "test.whatev_jobs",
                    "source": "test.whatev_sites",
                    "target": "test.whatev_projects"
                  },
                  {
                    "cardinality": "m2m",
                    "junction": "test.whatev_jobs",
                    "source": "test.whatev_sites",
                    "target": "test.whatev_projects"
                  },
                  {
                    "cardinality": "m2m",
                    "junction": "test.whatev_jobs",
                    "source": "test.whatev_sites",
                    "target": "test.whatev_projects"
                  }
              ],
              "hint": "By following the 'details' key, disambiguate the request by changing the url to /source?select=relationship(*) or /source?select=target!<relationship|cardinality|junction>(*)",
              "message": "More than one relationship was found for whatev_sites and whatev_projects"
            }
          |]
          { matchStatus  = 300
          , matchHeaders = [matchContentTypeJson]
          }

    context "disambiguating requests with embed hints" $ do
      it "can specify a cardinality hint" $ do
        get "/sites?select=name,big_projects!m2o(name)&limit=1" `shouldRespondWith`
          [json|
            [
              {
                "big_projects": {
                    "name": "big project 1"
                },
                "name": "site 1"
              }
            ]
          |]
          { matchHeaders = [matchContentTypeJson] }
        get "/agents?select=name,departments!m2o(name)&id=eq.1" `shouldRespondWith`
          [json|
            [
                {
                    "departments": {
                        "name": "dep 1"
                    },
                    "name": "agent 1"
                }
            ]
          |]
          { matchHeaders = [matchContentTypeJson] }

      it "embeds by using two fks pointing to the same table" $
        get "/orders?id=eq.1&select=id, name, billing_address_id(id), shipping_address_id(id)" `shouldRespondWith`
          [str|[{"id":1,"name":"order 1","billing_address_id":{"id":1},"shipping_address_id":{"id":2}}]|]

      it "will embed using an fk that has an uppercase character" $
        get "/ghostBusters?select=escapeId(*)" `shouldRespondWith`
          [json| [{"escapeId":{"so6meIdColumn":1}},{"escapeId":{"so6meIdColumn":3}},{"escapeId":{"so6meIdColumn":5}}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "can specify an fk hint and request children 2 levels" $
        get "/clients?id=eq.1&select=id,projects:projects!client_id(id,tasks(id))" `shouldRespondWith`
          [json|[{"id":1,"projects":[{"id":1,"tasks":[{"id":1},{"id":2}]},{"id":2,"tasks":[{"id":3},{"id":4}]}]}]|]
          { matchHeaders = [matchContentTypeJson] }

      it "can specify two different fk hints to the same table" $
        get "/message?select=id,body,sender:person!sender(name),recipient:person!recipient(name)&id=lt.4" `shouldRespondWith`
          [json|
            [{"id":1,"body":"Hello Jane","sender":{"name":"John"},"recipient":{"name":"Jane"}},
             {"id":2,"body":"Hi John","sender":{"name":"Jane"},"recipient":{"name":"John"}},
             {"id":3,"body":"How are you doing?","sender":{"name":"John"},"recipient":{"name":"Jane"}}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "fails if the fk is not known" $
        get "/message?select=id,sender:person!space(name)&id=lt.4" `shouldRespondWith`
          [json|{"message":"Could not find foreign keys between these entities. No relationship found between message and person"}|]
          { matchStatus = 400
          , matchHeaders = [matchContentTypeJson] }

      it "can specify an fk on a view" $
        get "/message?select=id,body,sender:person_detail!sender(name,sent),recipient:person_detail!recipient(name,received)&id=lt.4" `shouldRespondWith`
          [json|
            [{"id":1,"body":"Hello Jane","sender":{"name":"John","sent":2},"recipient":{"name":"Jane","received":2}},
             {"id":2,"body":"Hi John","sender":{"name":"Jane","sent":1},"recipient":{"name":"John","received":1}},
             {"id":3,"body":"How are you doing?","sender":{"name":"John","sent":2},"recipient":{"name":"Jane","received":2}}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "can specify the junction on an m2m relationship" $ do
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
          { matchHeaders = [matchContentTypeJson] }         { matchHeaders = [matchContentTypeJson] }

      context "using FK to specify the relationship" $ do
        it "can embed by FK name" $
          get "/projects?id=in.(1,3)&select=id,name,client_id(id,name)" `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7","client_id":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":{"id":2,"name":"Apple"}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can embed by FK name and select the FK column at the same time, if aliased" $
          get "/projects?id=in.(1,3)&select=id,name,client_id,client:client_id(id,name)" `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7","client_id":1,"client":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":2,"client":{"id":2,"name":"Apple"}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can embed parent and grandparent by using the FK" $
          get "/tasks?id=eq.1&select=id,name,project:projects!project_id(id,name,client:client_id(id,name))" `shouldRespondWith`
            [str|[{"id":1,"name":"Design w7","project":{"id":1,"name":"Windows 7","client":{"id":1,"name":"Microsoft"}}}]|]

    context "tables with self reference foreign keys" $ do
      context "one self reference foreign key" $ do
        it "embeds parents recursively" $
          get "/family_tree?id=in.(3,4)&select=id,parent:family_tree!m2o(id,name,parent:family_tree!m2o(*))" `shouldRespondWith`
            [json|[
              { "id": "3", "parent": { "id": "1", "name": "Parental Unit", "parent": null } },
              { "id": "4", "parent": { "id": "2", "name": "Kid One", "parent": { "id": "1", "name": "Parental Unit", "parent": null } } }
            ]|]
            { matchHeaders = [matchContentTypeJson] }

        it "embeds childs recursively" $
          get "/family_tree?id=eq.1&select=id,name, childs:family_tree!o2m(id,name,childs:family_tree!o2m(id,name))" `shouldRespondWith`
            [json|[{
              "id": "1", "name": "Parental Unit", "childs": [
                { "id": "2", "name": "Kid One", "childs": [ { "id": "4", "name": "Grandkid One" } ] },
                { "id": "3", "name": "Kid Two", "childs": [ { "id": "5", "name": "Grandkid Two" } ] }
              ]
            }]|] { matchHeaders = [matchContentTypeJson] }

        it "embeds parent and then embeds childs" $
          get "/family_tree?id=eq.2&select=id,name,parent:family_tree!m2o(id,name,childs:family_tree!o2m(id,name))" `shouldRespondWith`
            [json|[{
              "id": "2", "name": "Kid One", "parent": {
                "id": "1", "name": "Parental Unit", "childs": [ { "id": "2", "name": "Kid One" }, { "id": "3", "name": "Kid Two"} ]
              }
            }]|] { matchHeaders = [matchContentTypeJson] }

      context "two self reference foreign keys" $ do
        it "embeds parents" $
          get "/organizations?select=id,name,referee!m2o(id,name),auditor!m2o(id,name)&id=eq.3" `shouldRespondWith`
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

        it "embeds childs" $ do
          get "/organizations?select=id,name,refereeds:referee!o2m(id,name)&id=eq.1" `shouldRespondWith`
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
          get "/organizations?select=id,name,auditees:auditor!o2m(id,name)&id=eq.2" `shouldRespondWith`
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
          get "/organizations?select=name,manager(name),referee!m2o(name,manager(name),auditor!m2o(name,manager(name))),auditor!m2o(name,manager(name),referee!m2o(name,manager(name)))&id=eq.5" `shouldRespondWith`
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

          get "/organizations?select=name,manager(name),auditees:auditor!o2m(name,manager(name),refereeds:referee!o2m(name,manager(name)))&id=eq.2" `shouldRespondWith`
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
