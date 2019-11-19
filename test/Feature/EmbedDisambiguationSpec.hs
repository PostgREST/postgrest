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

    it "gives a 300 Multiple Choices error when the request is ambiguous" $ do
      pendingWith "duck typing removed: see what to do with single fk column embed"
      get "/message?select=id,body,sender(name,sent)" `shouldRespondWith`
        [json|
          {
            "details": [
            {
              "cardinality": "many-to-one",
              "source": "test.message[sender]",
              "target": "test.person[id]"
            },
            {
              "cardinality": "many-to-one",
              "source": "test.message[sender]",
              "target": "test.person_detail[id]"
            }
            ],
            "hint": "Disambiguate by choosing a relationship from the `details` key",
            "message": "More than one relationship was found for message and sender"
          }
        |]
        { matchStatus  = 300
        , matchHeaders = [matchContentTypeJson]
        }

      get "/users?select=*,id(*)" `shouldRespondWith`
        [json|
          {
            "details": [
              {
                "cardinality": "one-to-many",
                "source": "test.users[id]",
                "target": "test.articleStars[userId]"
              },
              {
                "cardinality": "one-to-many",
                "source": "test.users[id]",
                "target": "test.limited_article_stars[user_id]"
              },
              {
                "cardinality": "one-to-many",
                "source": "test.users[id]",
                "target": "test.comments[commenter_id]"
              },
              {
                "cardinality": "one-to-many",
                "source": "test.users[id]",
                "target": "test.users_projects[user_id]"
              },
              {
                "cardinality": "one-to-many",
                "source": "test.users[id]",
                "target": "test.users_tasks[user_id]"
              },
              {
                "cardinality": "many-to-many",
                "junction": "private.article_stars[user_id][article_id]",
                "source": "test.users[id]",
                "target": "test.articles[id]"
              },
              {
                "cardinality": "many-to-many",
                "junction": "test.articleStars[userId][articleId]",
                "source": "test.users[id]",
                "target": "test.articles[id]"
              },
              {
                "cardinality": "many-to-many",
                "junction": "test.limited_article_stars[user_id][article_id]",
                "source": "test.users[id]",
                "target": "test.articles[id]"
              },
              {
                "cardinality": "many-to-many",
                "junction": "test.users_projects[user_id][project_id]",
                "source": "test.users[id]",
                "target": "test.projects[id]"
              },
              {
                "cardinality": "many-to-many",
                "junction": "test.users_projects[user_id][project_id]",
                "source": "test.users[id]",
                "target": "test.materialized_projects[id]"
              },
              {
                "cardinality": "many-to-many",
                "junction": "test.users_projects[user_id][project_id]",
                "source": "test.users[id]",
                "target": "test.projects_view[id]"
              },
              {
                "cardinality": "many-to-many",
                "junction": "test.users_projects[user_id][project_id]",
                "source": "test.users[id]",
                "target": "test.projects_view_alt[t_id]"
              },
              {
                "cardinality": "many-to-many",
                "junction": "test.users_tasks[user_id][task_id]",
                "source": "test.users[id]",
                "target": "test.tasks[id]"
              },
              {
                "cardinality": "many-to-many",
                "junction": "test.users_tasks[user_id][task_id]",
                "source": "test.users[id]",
                "target": "test.filtered_tasks[myId]"
              }
            ],
            "hint": "Disambiguate by choosing a relationship from the `details` key",
            "message": "More than one relationship was found for users and id"
          }
        |]
        { matchStatus  = 300
        , matchHeaders = [matchContentTypeJson]
        }

    it "works when requesting children 2 levels" $
      get "/clients?id=eq.1&select=id,projects:projects!client_id(id,tasks(id))" `shouldRespondWith`
        [json|[{"id":1,"projects":[{"id":1,"tasks":[{"id":1},{"id":2}]},{"id":2,"tasks":[{"id":3},{"id":4}]}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "works with parent relation" $
      get "/message?select=id,body,sender:person!sender(name),recipient:person!recipient(name)&id=lt.4" `shouldRespondWith`
        [json|
          [{"id":1,"body":"Hello Jane","sender":{"name":"John"},"recipient":{"name":"Jane"}},
           {"id":2,"body":"Hi John","sender":{"name":"Jane"},"recipient":{"name":"John"}},
           {"id":3,"body":"How are you doing?","sender":{"name":"John"},"recipient":{"name":"Jane"}}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "fails with an unknown relation" $
      get "/message?select=id,sender:person!space(name)&id=lt.4" `shouldRespondWith`
        [json|{"message":"Could not find foreign keys between these entities, No relation found between message and person"}|]
        { matchStatus = 400
        , matchHeaders = [matchContentTypeJson] }

    it "works with a parent view relation" $
      get "/message?select=id,body,sender:person_detail!sender(name,sent),recipient:person_detail!recipient(name,received)&id=lt.4" `shouldRespondWith`
        [json|
          [{"id":1,"body":"Hello Jane","sender":{"name":"John","sent":2},"recipient":{"name":"Jane","received":2}},
           {"id":2,"body":"Hi John","sender":{"name":"Jane","sent":1},"recipient":{"name":"John","received":1}},
           {"id":3,"body":"How are you doing?","sender":{"name":"John","sent":2},"recipient":{"name":"Jane","received":2}}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "works with many<->many relation" $
      get "/tasks?select=id,users:users!users_tasks(id)" `shouldRespondWith`
        [json|[{"id":1,"users":[{"id":1},{"id":3}]},{"id":2,"users":[{"id":1}]},{"id":3,"users":[{"id":1}]},{"id":4,"users":[{"id":1}]},{"id":5,"users":[{"id":2},{"id":3}]},{"id":6,"users":[{"id":2}]},{"id":7,"users":[{"id":2}]},{"id":8,"users":[]}]|]
        { matchHeaders = [matchContentTypeJson] }

    context "using FK col to specify the relationship" $ do
        it "can embed by FK column name" $ do
          pendingWith "duck typing removed: see what to do with single fk column embed"
          get "/projects?id=in.(1,3)&select=id,name,client_id(id,name)" `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7","client_id":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":{"id":2,"name":"Apple"}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "can embed by FK column name and select the FK value at the same time, if aliased" $ do
          pendingWith "duck typing removed: see what to do with single fk column embed"
          get "/projects?id=in.(1,3)&select=id,name,client_id,client:client_id(id,name)" `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7","client_id":1,"client":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":2,"client":{"id":2,"name":"Apple"}}]|]
            { matchHeaders = [matchContentTypeJson] }

        it "requests parents two levels up" $ do
          pendingWith "duck typing removed: see what to do with single fk column embed"
          get "/tasks?id=eq.1&select=id,name,project:projects!project_id(id,name,client:client_id(id,name))" `shouldRespondWith`
            [str|[{"id":1,"name":"Design w7","project":{"id":1,"name":"Windows 7","client":{"id":1,"name":"Microsoft"}}}]|]


    context "tables with self reference foreign keys" $ do
      context "one self reference foreign key" $ do
        it "embeds parents recursively" $ do
          pendingWith "duck typing removed: see what to do with single fk column embed"
          get "/family_tree?id=in.(3,4)&select=id,parent(id,name,parent(*))" `shouldRespondWith`
            [json|[
              { "id": "3", "parent": { "id": "1", "name": "Parental Unit", "parent": null } },
              { "id": "4", "parent": { "id": "2", "name": "Kid One", "parent": { "id": "1", "name": "Parental Unit", "parent": null } } }
            ]|]
            { matchHeaders = [matchContentTypeJson] }

        it "embeds childs recursively" $
          get "/family_tree?id=eq.1&select=id,name, childs:family_tree!parent(id,name,childs:family_tree!parent(id,name))" `shouldRespondWith`
            [json|[{
              "id": "1", "name": "Parental Unit", "childs": [
                { "id": "2", "name": "Kid One", "childs": [ { "id": "4", "name": "Grandkid One" } ] },
                { "id": "3", "name": "Kid Two", "childs": [ { "id": "5", "name": "Grandkid Two" } ] }
              ]
            }]|] { matchHeaders = [matchContentTypeJson] }

        it "embeds parent and then embeds childs" $ do
          pendingWith "duck typing removed: see what to do with single fk column embed"
          get "/family_tree?id=eq.2&select=id,name,parent(id,name,childs:family_tree!parent(id,name))" `shouldRespondWith`
            [json|[{
              "id": "2", "name": "Kid One", "parent": {
                "id": "1", "name": "Parental Unit", "childs": [ { "id": "2", "name": "Kid One" }, { "id": "3", "name": "Kid Two"} ]
              }
            }]|] { matchHeaders = [matchContentTypeJson] }

      context "two self reference foreign keys" $ do
        it "embeds parents" $ do
          pendingWith "duck typing removed: see what to do with single fk column embed"
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

        it "embeds childs" $ do
          pendingWith "duck typing removed: see what to do with single fk column embed"
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
          pendingWith "duck typing removed: see what to do with single fk column embed"
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

    -- TODO Remove in next major version(7.0)
    describe "old dot '.' symbol, deprecated" $
      it "still works" $ do
        get "/clients?id=eq.1&select=id,projects:projects.client_id(id,tasks(id))" `shouldRespondWith`
          [json|[{"id":1,"projects":[{"id":1,"tasks":[{"id":1},{"id":2}]},{"id":2,"tasks":[{"id":3},{"id":4}]}]}]|]
          { matchHeaders = [matchContentTypeJson] }
        get "/tasks?select=id,users:users.users_tasks(id)" `shouldRespondWith`
          [json|[{"id":1,"users":[{"id":1},{"id":3}]},{"id":2,"users":[{"id":1}]},{"id":3,"users":[{"id":1}]},{"id":4,"users":[{"id":1}]},{"id":5,"users":[{"id":2},{"id":3}]},{"id":6,"users":[{"id":2}]},{"id":7,"users":[{"id":2}]},{"id":8,"users":[]}]|]
          { matchHeaders = [matchContentTypeJson] }
