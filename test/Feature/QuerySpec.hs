module Feature.QuerySpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders))

import SpecHelper
import Text.Heredoc
import Network.Wai (Application)

import Protolude hiding (get)

spec :: SpecWith Application
spec = do

  describe "Querying a table with a column called count" $
    it "should not confuse count column with pg_catalog.count aggregate" $
      get "/has_count_column" `shouldRespondWith` 200

  describe "Querying a table with a column called t" $
    it "should not conflict with internal postgrest table alias" $
      get "/clashing_column?select=t" `shouldRespondWith` 200

  describe "Querying a nonexistent table" $
    it "causes a 404" $
      get "/faketable" `shouldRespondWith` 404

  describe "Filtering response" $ do
    it "matches with equality" $
      get "/items?id=eq.5"
        `shouldRespondWith` [json| [{"id":5}] |]
        { matchHeaders = ["Content-Range" <:> "0-0/*"] }

    it "matches with equality using not operator" $
      get "/items?id=not.eq.5"
        `shouldRespondWith` [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]
        { matchHeaders = ["Content-Range" <:> "0-13/*"] }

    it "matches with more than one condition using not operator" $
      get "/simple_pk?k=like.*yx&extra=not.eq.u" `shouldRespondWith` "[]"

    it "matches with inequality using not operator" $ do
      get "/items?id=not.lt.14&order=id.asc"
        `shouldRespondWith` [json| [{"id":14},{"id":15}] |]
        { matchHeaders = ["Content-Range" <:> "0-1/*"] }
      get "/items?id=not.gt.2&order=id.asc"
        `shouldRespondWith` [json| [{"id":1},{"id":2}] |]
        { matchHeaders = ["Content-Range" <:> "0-1/*"] }

    it "matches items IN" $
      get "/items?id=in.(1,3,5)"
        `shouldRespondWith` [json| [{"id":1},{"id":3},{"id":5}] |]
        { matchHeaders = ["Content-Range" <:> "0-2/*"] }

    it "matches items NOT IN using not operator" $
      get "/items?id=not.in.(2,4,6,7,8,9,10,11,12,13,14,15)"
        `shouldRespondWith` [json| [{"id":1},{"id":3},{"id":5}] |]
        { matchHeaders = ["Content-Range" <:> "0-2/*"] }

    it "matches nulls using not operator" $
      get "/no_pk?a=not.is.null" `shouldRespondWith`
        [json| [{"a":"1","b":"0"},{"a":"2","b":"0"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "matches nulls in varchar and numeric fields alike" $ do
      get "/no_pk?a=is.null" `shouldRespondWith`
        [json| [{"a": null, "b": null}] |]
        { matchHeaders = [matchContentTypeJson] }

      get "/nullable_integer?a=is.null" `shouldRespondWith` [str|[{"a":null}]|]

    it "matches with like" $ do
      get "/simple_pk?k=like.*yx" `shouldRespondWith`
        [str|[{"k":"xyyx","extra":"u"}]|]
      get "/simple_pk?k=like.xy*" `shouldRespondWith`
        [str|[{"k":"xyyx","extra":"u"}]|]
      get "/simple_pk?k=like.*YY*" `shouldRespondWith`
        [str|[{"k":"xYYx","extra":"v"}]|]

    it "matches with like using not operator" $
      get "/simple_pk?k=not.like.*yx" `shouldRespondWith`
        [str|[{"k":"xYYx","extra":"v"}]|]

    it "matches with ilike" $ do
      get "/simple_pk?k=ilike.xy*&order=extra.asc" `shouldRespondWith`
        [json|[{"k":"xyyx","extra":"u"},{"k":"xYYx","extra":"v"}]|]
        { matchHeaders = [matchContentTypeJson] }
      get "/simple_pk?k=ilike.*YY*&order=extra.asc" `shouldRespondWith`
        [json|[{"k":"xyyx","extra":"u"},{"k":"xYYx","extra":"v"}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "matches with ilike using not operator" $
      get "/simple_pk?k=not.ilike.xy*&order=extra.asc" `shouldRespondWith` "[]"

    describe "Full text search operator" $ do
      it "finds matches with to_tsquery" $
        get "/tsearch?text_search_vector=fts.impossible" `shouldRespondWith`
          [json| [{"text_search_vector": "'fun':5 'imposs':9 'kind':3" }] |]
          { matchHeaders = [matchContentTypeJson] }

      it "can use lexeme boolean operators(&=%26, |=%7C, !) in to_tsquery" $ do
        get "/tsearch?text_search_vector=fts.fun%26possible" `shouldRespondWith`
          [json| [ {"text_search_vector": "'also':2 'fun':3 'possibl':8"}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/tsearch?text_search_vector=fts.impossible%7Cpossible" `shouldRespondWith`
          [json| [
          {"text_search_vector": "'fun':5 'imposs':9 'kind':3"},
          {"text_search_vector": "'also':2 'fun':3 'possibl':8"}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/tsearch?text_search_vector=fts.fun%26!possible" `shouldRespondWith`
          [json| [ {"text_search_vector": "'fun':5 'imposs':9 'kind':3"}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "finds matches with plainto_tsquery" $
        get "/tsearch?text_search_vector=plfts.The%20Fat%20Rats" `shouldRespondWith`
          [json| [ {"text_search_vector": "'ate':3 'cat':2 'fat':1 'rat':4" }] |]
          { matchHeaders = [matchContentTypeJson] }

      it "finds matches with different dictionaries" $ do
        get "/tsearch?text_search_vector=fts(french).amusant" `shouldRespondWith`
          [json| [{"text_search_vector": "'amus':5 'fair':7 'impossibl':9 'peu':4" }] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/tsearch?text_search_vector=plfts(french).amusant%20impossible" `shouldRespondWith`
          [json| [{"text_search_vector": "'amus':5 'fair':7 'impossibl':9 'peu':4" }] |]
          { matchHeaders = [matchContentTypeJson] }

      it "can be negated with not operator" $ do
        get "/tsearch?text_search_vector=not.fts.impossible%7Cfat%7Cfun" `shouldRespondWith`
          [json| [
            {"text_search_vector": "'amus':5 'fair':7 'impossibl':9 'peu':4"},
            {"text_search_vector": "'art':4 'spass':5 'unmog':7"}]|]
          { matchHeaders = [matchContentTypeJson] }
        get "/tsearch?text_search_vector=not.fts(english).impossible%7Cfat%7Cfun" `shouldRespondWith`
          [json| [
            {"text_search_vector": "'amus':5 'fair':7 'impossibl':9 'peu':4"},
            {"text_search_vector": "'art':4 'spass':5 'unmog':7"}]|]
          { matchHeaders = [matchContentTypeJson] }
        get "/tsearch?text_search_vector=not.plfts.The%20Fat%20Rats" `shouldRespondWith`
          [json| [
            {"text_search_vector": "'fun':5 'imposs':9 'kind':3"},
            {"text_search_vector": "'also':2 'fun':3 'possibl':8"},
            {"text_search_vector": "'amus':5 'fair':7 'impossibl':9 'peu':4"},
            {"text_search_vector": "'art':4 'spass':5 'unmog':7"}]|]
          { matchHeaders = [matchContentTypeJson] }

    it "matches with computed column" $
      get "/items?always_true=eq.true&order=id.asc" `shouldRespondWith`
        [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "order by computed column" $
      get "/items?order=anti_id.desc" `shouldRespondWith`
        [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "cannot access a computed column that is outside of the config schema" $
      get "/items?always_false=is.false" `shouldRespondWith` 400

    it "matches filtering nested items 2" $
      get "/clients?select=id,projects(id,tasks2(id,name))&projects.tasks.name=like.Design*"
        `shouldRespondWith` [json| {"message":"Could not find foreign keys between these entities, No relation found between projects and tasks2"}|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

    it "matches filtering nested items" $
      get "/clients?select=id,projects(id,tasks(id,name))&projects.tasks.name=like.Design*" `shouldRespondWith`
        [json|[{"id":1,"projects":[{"id":1,"tasks":[{"id":1,"name":"Design w7"}]},{"id":2,"tasks":[{"id":3,"name":"Design w10"}]}]},{"id":2,"projects":[{"id":3,"tasks":[{"id":5,"name":"Design IOS"}]},{"id":4,"tasks":[{"id":7,"name":"Design OSX"}]}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "matches with cs operator" $
      get "/complex_items?select=id&arr_data=cs.{2}" `shouldRespondWith`
        [json|[{"id":2},{"id":3}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "matches with cd operator" $
      get "/complex_items?select=id&arr_data=cd.{1,2,4}" `shouldRespondWith`
        [json|[{"id":1},{"id":2}]|]
        { matchHeaders = [matchContentTypeJson] }

  describe "Shaping response with select parameter" $ do

    it "selectStar works in absense of parameter" $
      get "/complex_items?id=eq.3" `shouldRespondWith`
        [str|[{"id":3,"name":"Three","settings":{"foo":{"int":1,"bar":"baz"}},"arr_data":[1,2,3],"field-with_sep":1}]|]

    it "dash `-` in column names is accepted" $
      get "/complex_items?id=eq.3&select=id,field-with_sep" `shouldRespondWith`
        [str|[{"id":3,"field-with_sep":1}]|]

    it "one simple column" $
      get "/complex_items?select=id" `shouldRespondWith`
        [json| [{"id":1},{"id":2},{"id":3}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "rename simple column" $
      get "/complex_items?id=eq.1&select=myId:id" `shouldRespondWith`
        [json| [{"myId":1}] |]
        { matchHeaders = [matchContentTypeJson] }


    it "one simple column with casting (text)" $
      get "/complex_items?select=id::text" `shouldRespondWith`
        [json| [{"id":"1"},{"id":"2"},{"id":"3"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "rename simple column with casting" $
      get "/complex_items?id=eq.1&select=myId:id::text" `shouldRespondWith`
        [json| [{"myId":"1"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "json column" $
      get "/complex_items?id=eq.1&select=settings" `shouldRespondWith`
        [json| [{"settings":{"foo":{"int":1,"bar":"baz"}}}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "fails on bad casting (wrong cast type)" $
      get "/complex_items?select=id::fakecolumntype"
        `shouldRespondWith` [json| {"hint":null,"details":null,"code":"42704","message":"type \"fakecolumntype\" does not exist"} |]
        { matchStatus  = 400
        , matchHeaders = []
        }

    it "requesting parents and children" $
      get "/projects?id=eq.1&select=id, name, clients(*), tasks(id, name)" `shouldRespondWith`
        [json|[{"id":1,"name":"Windows 7","clients":{"id":1,"name":"Microsoft"},"tasks":[{"id":1,"name":"Design w7"},{"id":2,"name":"Code w7"}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "requesting parent without specifying primary key" $
      get "/projects?select=name,client(name)" `shouldRespondWith`
        [json|[
          {"name":"Windows 7","client":{"name": "Microsoft"}},
          {"name":"Windows 10","client":{"name": "Microsoft"}},
          {"name":"IOS","client":{"name": "Apple"}},
          {"name":"OSX","client":{"name": "Apple"}},
          {"name":"Orphan","client":null}
        ]|]
        { matchHeaders = [matchContentTypeJson] }

    it "requesting parent and renaming primary key" $
      get "/projects?select=name,client(clientId:id,name)" `shouldRespondWith`
        [json|[
          {"name":"Windows 7","client":{"name": "Microsoft", "clientId": 1}},
          {"name":"Windows 10","client":{"name": "Microsoft", "clientId": 1}},
          {"name":"IOS","client":{"name": "Apple", "clientId": 2}},
          {"name":"OSX","client":{"name": "Apple", "clientId": 2}},
          {"name":"Orphan","client":null}
        ]|]
        { matchHeaders = [matchContentTypeJson] }

    it "requesting parent and specifying/renaming one key of the composite primary key" $ do
      get "/comments?select=*,users_tasks(userId:user_id)" `shouldRespondWith`
        [json|[{"id":1,"commenter_id":1,"user_id":2,"task_id":6,"content":"Needs to be delivered ASAP","users_tasks":{"userId": 2}}]|]
        { matchHeaders = [matchContentTypeJson] }
      get "/comments?select=*,users_tasks(taskId:task_id)" `shouldRespondWith`
        [json|[{"id":1,"commenter_id":1,"user_id":2,"task_id":6,"content":"Needs to be delivered ASAP","users_tasks":{"taskId": 6}}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "embed data with two fk pointing to the same table" $
      get "/orders?id=eq.1&select=id, name, billing_address_id(id), shipping_address_id(id)" `shouldRespondWith`
        [str|[{"id":1,"name":"order 1","billing_address_id":{"id":1},"shipping_address_id":{"id":2}}]|]


    it "requesting parents and children while renaming them" $
      get "/projects?id=eq.1&select=myId:id, name, project_client:client_id(*), project_tasks:tasks(id, name)" `shouldRespondWith`
        [json|[{"myId":1,"name":"Windows 7","project_client":{"id":1,"name":"Microsoft"},"project_tasks":[{"id":1,"name":"Design w7"},{"id":2,"name":"Code w7"}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "requesting parents two levels up while using FK to specify the link" $
      get "/tasks?id=eq.1&select=id,name,project:project_id(id,name,client:client_id(id,name))" `shouldRespondWith`
        [str|[{"id":1,"name":"Design w7","project":{"id":1,"name":"Windows 7","client":{"id":1,"name":"Microsoft"}}}]|]

    it "requesting parents two levels up while using FK to specify the link (with rename)" $
      get "/tasks?id=eq.1&select=id,name,project:project_id(id,name,client:client_id(id,name))" `shouldRespondWith`
        [str|[{"id":1,"name":"Design w7","project":{"id":1,"name":"Windows 7","client":{"id":1,"name":"Microsoft"}}}]|]

    it "requesting parents and filtering parent columns" $
      get "/projects?id=eq.1&select=id, name, clients(id)" `shouldRespondWith`
        [str|[{"id":1,"name":"Windows 7","clients":{"id":1}}]|]

    it "rows with missing parents are included" $
      get "/projects?id=in.(1,5)&select=id,clients(id)" `shouldRespondWith`
        [json|[{"id":1,"clients":{"id":1}},{"id":5,"clients":null}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "rows with no children return [] instead of null" $
      get "/projects?id=in.(5)&select=id,tasks(id)" `shouldRespondWith`
        [str|[{"id":5,"tasks":[]}]|]

    it "requesting children 2 levels" $
      get "/clients?id=eq.1&select=id,projects(id,tasks(id))" `shouldRespondWith`
        [json|[{"id":1,"projects":[{"id":1,"tasks":[{"id":1},{"id":2}]},{"id":2,"tasks":[{"id":3},{"id":4}]}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "requesting many<->many relation" $
      get "/tasks?select=id,users(id)" `shouldRespondWith`
        [json|[{"id":1,"users":[{"id":1},{"id":3}]},{"id":2,"users":[{"id":1}]},{"id":3,"users":[{"id":1}]},{"id":4,"users":[{"id":1}]},{"id":5,"users":[{"id":2},{"id":3}]},{"id":6,"users":[{"id":2}]},{"id":7,"users":[{"id":2}]},{"id":8,"users":[]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "requesting many<->many relation with rename" $
      get "/tasks?id=eq.1&select=id,theUsers:users(id)" `shouldRespondWith`
        [json|[{"id":1,"theUsers":[{"id":1},{"id":3}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "requesting many<->many relation reverse" $
      get "/users?select=id,tasks(id)" `shouldRespondWith`
        [json|[{"id":1,"tasks":[{"id":1},{"id":2},{"id":3},{"id":4}]},{"id":2,"tasks":[{"id":5},{"id":6},{"id":7}]},{"id":3,"tasks":[{"id":1},{"id":5}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "requesting children with composite key" $
      get "/users_tasks?user_id=eq.2&task_id=eq.6&select=*, comments(content)" `shouldRespondWith`
        [json|[{"user_id":2,"task_id":6,"comments":[{"content":"Needs to be delivered ASAP"}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "can embed by FK column name" $
      get "/projects?id=in.(1,3)&select=id,name,client_id(id,name)" `shouldRespondWith`
        [json|[{"id":1,"name":"Windows 7","client_id":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":{"id":2,"name":"Apple"}}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "can embed by FK column name and select the FK value at the same time, if aliased" $
      get "/projects?id=in.(1,3)&select=id,name,client_id,client:client_id(id,name)" `shouldRespondWith`
        [json|[{"id":1,"name":"Windows 7","client_id":1,"client":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":2,"client":{"id":2,"name":"Apple"}}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "can select by column name sans id" $
      get "/projects?id=in.(1,3)&select=id,name,client_id,client(id,name)" `shouldRespondWith`
        [json|[{"id":1,"name":"Windows 7","client_id":1,"client":{"id":1,"name":"Microsoft"}},{"id":3,"name":"IOS","client_id":2,"client":{"id":2,"name":"Apple"}}]|]
        { matchHeaders = [matchContentTypeJson] }

    describe "view embedding" $ do
      it "can detect fk relations through views to tables in the public schema" $
        get "/consumers_view?select=*,orders_view(*)" `shouldRespondWith` 200

      it "can request parent without specifying primary key" $
        get "/articleStars?select=createdAt,article(owner),user(name)&limit=1" `shouldRespondWith`
          [json|[{"createdAt":"2015-12-08T04:22:57.472738","article":{"owner": "postgrest_test_authenticator"},"user":{"name": "Angela Martin"}}]|]
          { matchHeaders = [matchContentTypeJson] }

      it "can detect relations in views from exposed schema that are based on tables in private schema and have columns renames" $
        get "/articles?id=eq.1&select=id,articleStars(users(*))" `shouldRespondWith`
          [json|[{"id":1,"articleStars":[{"users":{"id":1,"name":"Angela Martin"}},{"users":{"id":2,"name":"Michael Scott"}},{"users":{"id":3,"name":"Dwight Schrute"}}]}]|]
          { matchHeaders = [matchContentTypeJson] }

      it "works when requesting parents and children on views" $
        get "/projects_view?id=eq.1&select=id, name, clients(*), tasks(id, name)" `shouldRespondWith`
          [json|[{"id":1,"name":"Windows 7","clients":{"id":1,"name":"Microsoft"},"tasks":[{"id":1,"name":"Design w7"},{"id":2,"name":"Code w7"}]}]|]
          { matchHeaders = [matchContentTypeJson] }

      it "works when requesting parents and children on views with renamed keys" $
        get "/projects_view_alt?t_id=eq.1&select=t_id, name, clients(*), tasks(id, name)" `shouldRespondWith`
          [json|[{"t_id":1,"name":"Windows 7","clients":{"id":1,"name":"Microsoft"},"tasks":[{"id":1,"name":"Design w7"},{"id":2,"name":"Code w7"}]}]|]
          { matchHeaders = [matchContentTypeJson] }

      it "detects parent relations when having many views of a private table" $ do
        get "/books?select=title,author(name)&id=eq.5" `shouldRespondWith`
          [json|[ { "title": "Farenheit 451", "author": { "name": "Ray Bradbury" } } ]|]
          { matchHeaders = [matchContentTypeJson] }
        get "/forties_books?select=title,author(name)&limit=1" `shouldRespondWith`
          [json|[ { "title": "1984", "author": { "name": "George Orwell" } } ]|]
          { matchHeaders = [matchContentTypeJson] }
        get "/fifties_books?select=title,author(name)&limit=1" `shouldRespondWith`
          [json|[ { "title": "The Catcher in the Rye", "author": { "name": "J.D. Salinger" } } ]|]
          { matchHeaders = [matchContentTypeJson] }
        get "/sixties_books?select=title,author(name)&limit=1" `shouldRespondWith`
          [json|[ { "title": "To Kill a Mockingbird", "author": { "name": "Harper Lee" } } ]|]
          { matchHeaders = [matchContentTypeJson] }

      it "works with views that have subselects" $
        get "/authors_books_number?select=*,books(title)&id=eq.1" `shouldRespondWith`
          [json|[ {"id":1, "name":"George Orwell","num_in_forties":1,"num_in_fifties":0,"num_in_sixties":0,"num_in_all_decades":1,
                   "books":[{"title":"1984"}]} ]|]
          { matchHeaders = [matchContentTypeJson] }

      it "works with views that have case subselects" $
        get "/authors_have_book_in_decade?select=*,books(title)&id=eq.3" `shouldRespondWith`
          [json|[ {"id":3,"name":"Antoine de Saint-Exupéry","has_book_in_forties":true,"has_book_in_fifties":false,"has_book_in_sixties":false,
                   "books":[{"title":"The Little Prince"}]} ]|]
          { matchHeaders = [matchContentTypeJson] }

      it "works with views that have subselect in the FROM clause" $
        get "/forties_and_fifties_books?select=title,first_publisher,author:authors(name)&id=eq.1" `shouldRespondWith`
          [json|[{"title":"1984","first_publisher":"Secker & Warburg","author":{"name":"George Orwell"}}]|]
          { matchHeaders = [matchContentTypeJson] }

      it "works with views that have CTE" $
        get "/odd_years_publications?select=title,publication_year,first_publisher,author:authors(name)&id=in.(1,2,3)" `shouldRespondWith`
          [json|[
            {"title":"1984","publication_year":1949,"first_publisher":"Secker & Warburg","author":{"name":"George Orwell"}},
            {"title":"The Diary of a Young Girl","publication_year":1947,"first_publisher":"Contact Publishing","author":{"name":"Anne Frank"}},
            {"title":"The Little Prince","publication_year":1947,"first_publisher":"Reynal & Hitchcock","author":{"name":"Antoine de Saint-Exupéry"}} ]|]
          { matchHeaders = [matchContentTypeJson] }

      it "works when having a capitalized table name and camelCase fk column" $
        get "/foos?select=*,bars(*)" `shouldRespondWith` 200

      it "works when embedding a view with a table that has a long compound pk" $ do
        get "/player_view?select=id,contract(purchase_price)&id=in.(1,3,5,7)" `shouldRespondWith`
          [json|
            [{"id":1,"contract":[{"purchase_price":10}]},
             {"id":3,"contract":[{"purchase_price":30}]},
             {"id":5,"contract":[{"purchase_price":50}]},
             {"id":7,"contract":[]}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/contract?select=tournament,player_view(first_name)&limit=3" `shouldRespondWith`
          [json|
            [{"tournament":"tournament_1","player_view":{"first_name":"first_name_1"}},
             {"tournament":"tournament_2","player_view":{"first_name":"first_name_2"}},
             {"tournament":"tournament_3","player_view":{"first_name":"first_name_3"}}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "works when embedding a view with a view that referes to a table that has a long compound pk" $ do
        get "/player_view?select=id,contract_view(purchase_price)&id=in.(1,3,5,7)" `shouldRespondWith`
          [json|
            [{"id":1,"contract_view":[{"purchase_price":10}]},
             {"id":3,"contract_view":[{"purchase_price":30}]},
             {"id":5,"contract_view":[{"purchase_price":50}]},
             {"id":7,"contract_view":[]}] |]
          { matchHeaders = [matchContentTypeJson] }
        get "/contract_view?select=tournament,player_view(first_name)&limit=3" `shouldRespondWith`
          [json|
            [{"tournament":"tournament_1","player_view":{"first_name":"first_name_1"}},
             {"tournament":"tournament_2","player_view":{"first_name":"first_name_2"}},
             {"tournament":"tournament_3","player_view":{"first_name":"first_name_3"}}] |]
          { matchHeaders = [matchContentTypeJson] }

    describe "path fixed" $ do
      it "works when requesting children 2 levels" $
        get "/clients?id=eq.1&select=id,projects:projects.client_id(id,tasks(id))" `shouldRespondWith`
          [json|[{"id":1,"projects":[{"id":1,"tasks":[{"id":1},{"id":2}]},{"id":2,"tasks":[{"id":3},{"id":4}]}]}]|]
          { matchHeaders = [matchContentTypeJson] }

      it "works with parent relation" $
        get "/message?select=id,body,sender:person.sender(name),recipient:person.recipient(name)&id=lt.4" `shouldRespondWith`
          [json|
            [{"id":1,"body":"Hello Jane","sender":{"name":"John"},"recipient":{"name":"Jane"}},
             {"id":2,"body":"Hi John","sender":{"name":"Jane"},"recipient":{"name":"John"}},
             {"id":3,"body":"How are you doing?","sender":{"name":"John"},"recipient":{"name":"Jane"}}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "works with a parent view relation" $
        get "/message?select=id,body,sender:person_detail.sender(name,sent),recipient:person_detail.recipient(name,received)&id=lt.4" `shouldRespondWith`
          [json|
            [{"id":1,"body":"Hello Jane","sender":{"name":"John","sent":2},"recipient":{"name":"Jane","received":2}},
             {"id":2,"body":"Hi John","sender":{"name":"Jane","sent":1},"recipient":{"name":"John","received":1}},
             {"id":3,"body":"How are you doing?","sender":{"name":"John","sent":2},"recipient":{"name":"Jane","received":2}}] |]
          { matchHeaders = [matchContentTypeJson] }

      it "works with many<->many relation" $
        get "/tasks?select=id,users:users.users_tasks(id)" `shouldRespondWith`
          [json|[{"id":1,"users":[{"id":1},{"id":3}]},{"id":2,"users":[{"id":1}]},{"id":3,"users":[{"id":1}]},{"id":4,"users":[{"id":1}]},{"id":5,"users":[{"id":2},{"id":3}]},{"id":6,"users":[{"id":2}]},{"id":7,"users":[{"id":2}]},{"id":8,"users":[]}]|]
          { matchHeaders = [matchContentTypeJson] }

    describe "aliased embeds" $ do
      it "works with child relation" $
        get "/space?select=id,zones:zone(id,name),stores:zone(id,name)&zones.zone_type_id=eq.2&stores.zone_type_id=eq.3" `shouldRespondWith`
          [json|[
            { "id":1,
              "zones": [ {"id":1,"name":"zone 1"}, {"id":2,"name":"zone 2"}],
              "stores": [ {"id":3,"name":"store 3"}, {"id":4,"name":"store 4"}]}
          ]|] { matchHeaders = [matchContentTypeJson] }

      it "works with many to many relation" $
        get "/users?select=id,designTasks:tasks(id,name),codeTasks:tasks(id,name)&designTasks.name=like.*Design*&codeTasks.name=like.*Code*" `shouldRespondWith`
          [json|[
             { "id":1,
               "designTasks":[ { "id":1, "name":"Design w7" }, { "id":3, "name":"Design w10" } ],
               "codeTasks":[ { "id":2, "name":"Code w7" }, { "id":4, "name":"Code w10" } ] },
             { "id":2,
               "designTasks":[ { "id":5, "name":"Design IOS" }, { "id":7, "name":"Design OSX" } ],
               "codeTasks":[ { "id":6, "name":"Code IOS" } ] },
             { "id":3,
               "designTasks":[ { "id":1, "name":"Design w7" }, { "id":5, "name":"Design IOS" } ],
               "codeTasks":[ ] }
          ]|] { matchHeaders = [matchContentTypeJson] }

      it "works with an aliased child plus non aliased child" $
        get "/projects?select=id,name,designTasks:tasks(name,users(id,name))&designTasks.name=like.*Design*&designTasks.users.id=in.(1,2)" `shouldRespondWith`
          [json|[
            {
              "id":1, "name":"Windows 7",
              "designTasks":[ { "name":"Design w7", "users":[ { "id":1, "name":"Angela Martin" } ] } ] },
            {
              "id":2, "name":"Windows 10",
              "designTasks":[ { "name":"Design w10", "users":[ { "id":1, "name":"Angela Martin" } ] } ] },
            {
              "id":3, "name":"IOS",
              "designTasks":[ { "name":"Design IOS", "users":[ { "id":2, "name":"Michael Scott" } ] } ] },
            {
              "id":4, "name":"OSX",
              "designTasks":[ { "name":"Design OSX", "users":[ { "id":2, "name":"Michael Scott" } ] } ] },
            {
              "id":5, "name":"Orphan",
              "designTasks":[ ] }
          ]|] { matchHeaders = [matchContentTypeJson] }

      it "works with two aliased childs embeds plus and/or" $
        get "/entities?select=id,childs:child_entities(id,gChilds:grandchild_entities(id))&childs.and=(id.in.(1,2,3))&childs.gChilds.or=(id.eq.1,id.eq.2)" `shouldRespondWith`
          [json|[
            { "id":1,
              "childs":[
                {"id":1,"gChilds":[{"id":1}, {"id":2}]},
                {"id":2,"gChilds":[]}]},
            { "id":2,
              "childs":[
                {"id":3,"gChilds":[]}]},
            { "id":3,"childs":[]},
            { "id":4,"childs":[]}
          ]|] { matchHeaders = [matchContentTypeJson] }

    describe "tables with self reference foreign keys" $ do
      context "one self reference foreign key" $ do
        it "embeds parents recursively" $
          get "/family_tree?id=in.(3,4)&select=id,parent(id,name,parent(*))" `shouldRespondWith`
            [json|[
              { "id": "3", "parent": { "id": "1", "name": "Parental Unit", "parent": null } },
              { "id": "4", "parent": { "id": "2", "name": "Kid One", "parent": { "id": "1", "name": "Parental Unit", "parent": null } } }
            ]|]
            { matchHeaders = [matchContentTypeJson] }

        it "embeds childs recursively" $
          get "/family_tree?id=eq.1&select=id,name, childs:family_tree.parent(id,name,childs:family_tree.parent(id,name))" `shouldRespondWith`
            [json|[{
              "id": "1", "name": "Parental Unit", "childs": [
                { "id": "2", "name": "Kid One", "childs": [ { "id": "4", "name": "Grandkid One" } ] },
                { "id": "3", "name": "Kid Two", "childs": [ { "id": "5", "name": "Grandkid Two" } ] }
              ]
            }]|] { matchHeaders = [matchContentTypeJson] }

        it "embeds parent and then embeds childs" $
          get "/family_tree?id=eq.2&select=id,name,parent(id,name,childs:family_tree.parent(id,name))" `shouldRespondWith`
            [json|[{
              "id": "2", "name": "Kid One", "parent": {
                "id": "1", "name": "Parental Unit", "childs": [ { "id": "2", "name": "Kid One" }, { "id": "3", "name": "Kid Two"} ]
              }
            }]|] { matchHeaders = [matchContentTypeJson] }

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

        it "embeds childs" $ do
          get "/organizations?select=id,name,refereeds:organizations.referee(id,name)&id=eq.1" `shouldRespondWith`
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
          get "/organizations?select=id,name,auditees:organizations.auditor(id,name)&id=eq.2" `shouldRespondWith`
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

  describe "ordering response" $ do
    it "by a column asc" $
      get "/items?id=lte.2&order=id.asc"
        `shouldRespondWith` [json| [{"id":1},{"id":2}] |]
        { matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/*"]
        }


    it "by a column desc" $
      get "/items?id=lte.2&order=id.desc"
        `shouldRespondWith` [json| [{"id":2},{"id":1}] |]
        { matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/*"]
        }

    it "by a column with nulls first" $
      get "/no_pk?order=a.nullsfirst"
        `shouldRespondWith` [json| [{"a":null,"b":null},
                              {"a":"1","b":"0"},
                              {"a":"2","b":"0"}
                              ] |]
        { matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/*"]
        }

    it "by a column asc with nulls last" $
      get "/no_pk?order=a.asc.nullslast"
        `shouldRespondWith` [json| [{"a":"1","b":"0"},
                              {"a":"2","b":"0"},
                              {"a":null,"b":null}] |]
        { matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/*"]
        }

    it "by a column desc with nulls first" $
      get "/no_pk?order=a.desc.nullsfirst"
        `shouldRespondWith` [json| [{"a":null,"b":null},
                              {"a":"2","b":"0"},
                              {"a":"1","b":"0"}] |]
        { matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/*"]
        }

    it "by a column desc with nulls last" $
      get "/no_pk?order=a.desc.nullslast"
        `shouldRespondWith` [json| [{"a":"2","b":"0"},
                              {"a":"1","b":"0"},
                              {"a":null,"b":null}] |]
        { matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/*"]
        }

    it "by two columns with nulls and direction specified" $
      get "/projects?select=client_id,id,name&order=client_id.desc.nullslast,id.desc"
        `shouldRespondWith` [json|
          [{"client_id":2,"id":4,"name":"OSX"},
           {"client_id":2,"id":3,"name":"IOS"},
           {"client_id":1,"id":2,"name":"Windows 10"},
           {"client_id":1,"id":1,"name":"Windows 7"},
           {"client_id":null,"id":5,"name":"Orphan"}]
        |]
        { matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-4/*"]
        }

    it "by a column with no direction or nulls specified" $
      get "/items?id=lte.2&order=id"
        `shouldRespondWith` [json| [{"id":1},{"id":2}] |]
        { matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/*"]
        }

    it "without other constraints" $
      get "/items?order=id.asc" `shouldRespondWith` 200

    it "ordering embeded entities" $
      get "/projects?id=eq.1&select=id, name, tasks(id, name)&tasks.order=name.asc" `shouldRespondWith`
        [json|[{"id":1,"name":"Windows 7","tasks":[{"id":2,"name":"Code w7"},{"id":1,"name":"Design w7"}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "ordering embeded entities with alias" $
      get "/projects?id=eq.1&select=id, name, the_tasks:tasks(id, name)&tasks.order=name.asc" `shouldRespondWith`
        [json|[{"id":1,"name":"Windows 7","the_tasks":[{"id":2,"name":"Code w7"},{"id":1,"name":"Design w7"}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "ordering embeded entities, two levels" $
      get "/projects?id=eq.1&select=id, name, tasks(id, name, users(id, name))&tasks.order=name.asc&tasks.users.order=name.desc" `shouldRespondWith`
        [json|[{"id":1,"name":"Windows 7","tasks":[{"id":2,"name":"Code w7","users":[{"id":1,"name":"Angela Martin"}]},{"id":1,"name":"Design w7","users":[{"id":3,"name":"Dwight Schrute"},{"id":1,"name":"Angela Martin"}]}]}]|]
        { matchHeaders = [matchContentTypeJson] }

    it "ordering embeded parents does not break things" $
      get "/projects?id=eq.1&select=id, name, clients(id, name)&clients.order=name.asc" `shouldRespondWith`
        [str|[{"id":1,"name":"Windows 7","clients":{"id":1,"name":"Microsoft"}}]|]

    it "ordering embeded parents does not break things when using ducktape names" $
      get "/projects?id=eq.1&select=id, name, client(id, name)&client.order=name.asc" `shouldRespondWith`
        [str|[{"id":1,"name":"Windows 7","client":{"id":1,"name":"Microsoft"}}]|]

    context "order syntax errors" $ do
      it "gives meaningful error messages when asc/desc/nulls{first,last} are misspelled" $ do
        get "/items?order=id.ac" `shouldRespondWith`
          [json|{"details":"unexpected \"c\" expecting \"asc\", \"desc\", \"nullsfirst\" or \"nullslast\"","message":"\"failed to parse order (id.ac)\" (line 1, column 4)"}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }
        get "/items?order=id.descc" `shouldRespondWith`
          [json|{"details":"unexpected 'c' expecting delimiter (.), \",\" or end of input","message":"\"failed to parse order (id.descc)\" (line 1, column 8)"}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }
        get "/items?order=id.nulsfist" `shouldRespondWith`
          [json|{"details":"unexpected \"n\" expecting \"asc\", \"desc\", \"nullsfirst\" or \"nullslast\"","message":"\"failed to parse order (id.nulsfist)\" (line 1, column 4)"}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }
        get "/items?order=id.nullslasttt" `shouldRespondWith`
          [json|{"details":"unexpected 't' expecting \",\" or end of input","message":"\"failed to parse order (id.nullslasttt)\" (line 1, column 13)"}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }
        get "/items?order=id.smth34" `shouldRespondWith`
          [json|{"details":"unexpected \"s\" expecting \"asc\", \"desc\", \"nullsfirst\" or \"nullslast\"","message":"\"failed to parse order (id.smth34)\" (line 1, column 4)"}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }

      it "gives meaningful error messages when nulls{first,last} are misspelled after asc/desc" $ do
        get "/items?order=id.asc.nlsfst" `shouldRespondWith`
          [json|{"details":"unexpected \"l\" expecting \"nullsfirst\" or \"nullslast\"","message":"\"failed to parse order (id.asc.nlsfst)\" (line 1, column 8)"}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }
        get "/items?order=id.asc.nullslasttt" `shouldRespondWith`
          [json|{"details":"unexpected 't' expecting \",\" or end of input","message":"\"failed to parse order (id.asc.nullslasttt)\" (line 1, column 17)"}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }
        get "/items?order=id.asc.smth34" `shouldRespondWith`
          [json|{"details":"unexpected \"s\" expecting \"nullsfirst\" or \"nullslast\"","message":"\"failed to parse order (id.asc.smth34)\" (line 1, column 8)"}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }

  describe "Accept headers" $ do
    it "should respond an unknown accept type with 415" $
      request methodGet "/simple_pk"
              (acceptHdrs "text/unknowntype") ""
        `shouldRespondWith` 415

    it "should respond correctly to */* in accept header" $
      request methodGet "/simple_pk"
              (acceptHdrs "*/*") ""
        `shouldRespondWith` 200

    it "*/* should rescue an unknown type" $
      request methodGet "/simple_pk"
              (acceptHdrs "text/unknowntype, */*") ""
        `shouldRespondWith` 200

    it "specific available preference should override */*" $ do
      r <- request methodGet "/simple_pk"
              (acceptHdrs "text/csv, */*") ""
      liftIO $ do
        let respHeaders = simpleHeaders r
        respHeaders `shouldSatisfy` matchHeader
          "Content-Type" "text/csv; charset=utf-8"

    it "honors client preference even when opposite of server preference" $ do
      r <- request methodGet "/simple_pk"
              (acceptHdrs "text/csv, application/json") ""
      liftIO $ do
        let respHeaders = simpleHeaders r
        respHeaders `shouldSatisfy` matchHeader
          "Content-Type" "text/csv; charset=utf-8"

    it "should respond correctly to multiple types in accept header" $
      request methodGet "/simple_pk"
              (acceptHdrs "text/unknowntype, text/csv") ""
        `shouldRespondWith` 200

    it "should respond with CSV to 'text/csv' request" $
      request methodGet "/simple_pk"
              (acceptHdrs "text/csv; version=1") ""
        `shouldRespondWith` "k,extra\nxyyx,u\nxYYx,v"
        { matchStatus  = 200
        , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8"]
        }

  describe "Canonical location" $ do
    it "Sets Content-Location with alphabetized params" $
      get "/no_pk?b=eq.1&a=eq.1"
        `shouldRespondWith` "[]"
        { matchStatus  = 200
        , matchHeaders = ["Content-Location" <:> "/no_pk?a=eq.1&b=eq.1"]
        }

    it "Omits question mark when there are no params" $ do
      r <- get "/simple_pk"
      liftIO $ do
        let respHeaders = simpleHeaders r
        respHeaders `shouldSatisfy` matchHeader
          "Content-Location" "/simple_pk"


  describe "weird requests" $ do
    it "can query as normal" $ do
      get "/Escap3e;" `shouldRespondWith`
        [json| [{"so6meIdColumn":1},{"so6meIdColumn":2},{"so6meIdColumn":3},{"so6meIdColumn":4},{"so6meIdColumn":5}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/ghostBusters" `shouldRespondWith`
        [json| [{"escapeId":1},{"escapeId":3},{"escapeId":5}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "fails if an operator is not given" $
      get "/ghostBusters?id=0" `shouldRespondWith` [json| {"details":"unexpected \"0\" expecting \"not\" or operator (eq, gt, ...)","message":"\"failed to parse filter (0)\" (line 1, column 1)"} |]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

    it "will embed a collection" $
      get "/Escap3e;?select=ghostBusters(*)" `shouldRespondWith`
        [json| [{"ghostBusters":[{"escapeId":1}]},{"ghostBusters":[]},{"ghostBusters":[{"escapeId":3}]},{"ghostBusters":[]},{"ghostBusters":[{"escapeId":5}]}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "will embed using a column" $
      get "/ghostBusters?select=escapeId(*)" `shouldRespondWith`
        [json| [{"escapeId":{"so6meIdColumn":1}},{"escapeId":{"so6meIdColumn":3}},{"escapeId":{"so6meIdColumn":5}}] |]
        { matchHeaders = [matchContentTypeJson] }

  describe "binary output" $ do
    context "on GET" $ do
      it "can query if a single column is selected" $
        request methodGet "/images_base64?select=img&name=eq.A.png" (acceptHdrs "application/octet-stream") ""
          `shouldRespondWith` "iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEUAAAD/AAAb/40iAAAAP0lEQVQI12NgwAbYG2AE/wEYwQMiZB4ACQkQYZEAIgqAhAGIKLCAEQ8kgMT/P1CCEUwc4IMSzA3sUIIdCHECAGSQEkeOTUyCAAAAAElFTkSuQmCC"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/octet-stream; charset=utf-8"]
          }

      it "fails if a single column is not selected" $ do
        request methodGet "/images?select=img,name&name=eq.A.png" (acceptHdrs "application/octet-stream") ""
          `shouldRespondWith` 406
        request methodGet "/images?select=*&name=eq.A.png" (acceptHdrs "application/octet-stream") ""
          `shouldRespondWith` 406
        request methodGet "/images?name=eq.A.png" (acceptHdrs "application/octet-stream") ""
          `shouldRespondWith` 406

      it "concatenates results if more than one row is returned" $
        request methodGet "/images_base64?select=img&name=in.(A.png,B.png)" (acceptHdrs "application/octet-stream") ""
          `shouldRespondWith` "iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEUAAAD/AAAb/40iAAAAP0lEQVQI12NgwAbYG2AE/wEYwQMiZB4ACQkQYZEAIgqAhAGIKLCAEQ8kgMT/P1CCEUwc4IMSzA3sUIIdCHECAGSQEkeOTUyCAAAAAElFTkSuQmCCiVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEX///8AAP94wDzzAAAAL0lEQVQIW2NgwAb+HwARH0DEDyDxwAZEyGAhLODqHmBRzAcn5GAS///A1IF14AAA5/Adbiiz/0gAAAAASUVORK5CYII="
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/octet-stream; charset=utf-8"]
          }

    context "on RPC" $ do
      context "Proc that returns scalar" $
        it "can query without selecting column" $
          request methodPost "/rpc/ret_base64_bin" (acceptHdrs "application/octet-stream") ""
            `shouldRespondWith` "iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEUAAAD/AAAb/40iAAAAP0lEQVQI12NgwAbYG2AE/wEYwQMiZB4ACQkQYZEAIgqAhAGIKLCAEQ8kgMT/P1CCEUwc4IMSzA3sUIIdCHECAGSQEkeOTUyCAAAAAElFTkSuQmCC"
            { matchStatus = 200
            , matchHeaders = ["Content-Type" <:> "application/octet-stream; charset=utf-8"]
            }

      context "Proc that returns rows" $ do
        it "can query if a single column is selected" $
          request methodPost "/rpc/ret_rows_with_base64_bin?select=img" (acceptHdrs "application/octet-stream") ""
            `shouldRespondWith` "iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEUAAAD/AAAb/40iAAAAP0lEQVQI12NgwAbYG2AE/wEYwQMiZB4ACQkQYZEAIgqAhAGIKLCAEQ8kgMT/P1CCEUwc4IMSzA3sUIIdCHECAGSQEkeOTUyCAAAAAElFTkSuQmCCiVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEX///8AAP94wDzzAAAAL0lEQVQIW2NgwAb+HwARH0DEDyDxwAZEyGAhLODqHmBRzAcn5GAS///A1IF14AAA5/Adbiiz/0gAAAAASUVORK5CYII="
            { matchStatus = 200
            , matchHeaders = ["Content-Type" <:> "application/octet-stream; charset=utf-8"]
            }

        it "fails if a single column is not selected" $
          request methodPost "/rpc/ret_rows_with_base64_bin" (acceptHdrs "application/octet-stream") ""
            `shouldRespondWith` 406

  describe "HTTP request env vars" $ do
    it "custom header is set" $
      request methodPost "/rpc/get_guc_value"
                [("Custom-Header", "test")]
          [json| { "name": "request.header.custom-header" } |]
          `shouldRespondWith`
          [str|"test"|]
          { matchStatus  = 200
          , matchHeaders = [ matchContentTypeJson ]
          }
    it "standard header is set" $
      request methodPost "/rpc/get_guc_value"
                [("Origin", "http://example.com")]
          [json| { "name": "request.header.origin" } |]
          `shouldRespondWith`
          [str|"http://example.com"|]
          { matchStatus  = 200
          , matchHeaders = [ matchContentTypeJson ]
          }
    it "current role is available as GUC claim" $
      request methodPost "/rpc/get_guc_value" []
          [json| { "name": "request.jwt.claim.role" } |]
          `shouldRespondWith`
          [str|"postgrest_test_anonymous"|]
          { matchStatus  = 200
          , matchHeaders = [ matchContentTypeJson ]
          }
    it "single cookie ends up as claims" $
      request methodPost "/rpc/get_guc_value" [("Cookie","acookie=cookievalue")]
        [json| {"name":"request.cookie.acookie"} |]
          `shouldRespondWith`
          [str|"cookievalue"|]
          { matchStatus = 200
          , matchHeaders = []
          }
    it "multiple cookies ends up as claims" $
      request methodPost "/rpc/get_guc_value" [("Cookie","acookie=cookievalue;secondcookie=anothervalue")]
        [json| {"name":"request.cookie.secondcookie"} |]
          `shouldRespondWith`
          [str|"anothervalue"|]
          { matchStatus = 200
          , matchHeaders = []
          }
    it "app settings available" $
      request methodPost "/rpc/get_guc_value" []
        [json| { "name": "app.settings.app_host" } |]
          `shouldRespondWith`
          [str|"localhost"|]
          { matchStatus  = 200
          , matchHeaders = [ matchContentTypeJson ]
          }

  describe "values with quotes in IN and NOT IN" $ do
    it "succeeds when only quoted values are present" $ do
      get "/w_or_wo_comma_names?name=in.(\"Hebdon, John\")" `shouldRespondWith`
        [json| [{"name":"Hebdon, John"}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/w_or_wo_comma_names?name=in.(\"Hebdon, John\",\"Williams, Mary\",\"Smith, Joseph\")" `shouldRespondWith`
        [json| [{"name":"Hebdon, John"},{"name":"Williams, Mary"},{"name":"Smith, Joseph"}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/w_or_wo_comma_names?name=not.in.(\"Hebdon, John\",\"Williams, Mary\",\"Smith, Joseph\")" `shouldRespondWith`
        [json| [{"name":"David White"},{"name":"Larry Thompson"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "succeeds w/ and w/o quoted values" $ do
      get "/w_or_wo_comma_names?name=in.(David White,\"Hebdon, John\")" `shouldRespondWith`
        [json| [{"name":"Hebdon, John"},{"name":"David White"}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/w_or_wo_comma_names?name=not.in.(\"Hebdon, John\",Larry Thompson,\"Smith, Joseph\")" `shouldRespondWith`
        [json| [{"name":"Williams, Mary"},{"name":"David White"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "checks well formed quoted values" $ do
      get "/w_or_wo_comma_names?name=in.(\"\"Hebdon, John\")" `shouldRespondWith`
        [json| [] |] { matchHeaders = [matchContentTypeJson] }
      get "/w_or_wo_comma_names?name=in.(\"\"Hebdon, John\"\"Mary)" `shouldRespondWith`
        [json| [] |] { matchHeaders = [matchContentTypeJson] }
      get "/w_or_wo_comma_names?name=in.(Williams\"Hebdon, John\")" `shouldRespondWith`
        [json| [] |] { matchHeaders = [matchContentTypeJson] }

  describe "IN and NOT IN empty set" $ do
    context "returns an empty result for IN when no value is present" $ do
      it "works for integer" $
        get "/items_with_different_col_types?int_data=in.()" `shouldRespondWith`
          [json| [] |] { matchHeaders = [matchContentTypeJson] }
      it "works for text" $
        get "/items_with_different_col_types?text_data=in.()" `shouldRespondWith`
          [json| [] |] { matchHeaders = [matchContentTypeJson] }
      it "works for bool" $
        get "/items_with_different_col_types?bool_data=in.()" `shouldRespondWith`
          [json| [] |] { matchHeaders = [matchContentTypeJson] }
      it "works for bytea" $
        get "/items_with_different_col_types?bin_data=in.()" `shouldRespondWith`
          [json| [] |] { matchHeaders = [matchContentTypeJson] }
      it "works for char" $
        get "/items_with_different_col_types?char_data=in.()" `shouldRespondWith`
          [json| [] |] { matchHeaders = [matchContentTypeJson] }
      it "works for date" $
        get "/items_with_different_col_types?date_data=in.()" `shouldRespondWith`
          [json| [] |] { matchHeaders = [matchContentTypeJson] }
      it "works for real" $
        get "/items_with_different_col_types?real_data=in.()" `shouldRespondWith`
          [json| [] |] { matchHeaders = [matchContentTypeJson] }
      it "works for time" $
        get "/items_with_different_col_types?time_data=in.()" `shouldRespondWith`
          [json| [] |] { matchHeaders = [matchContentTypeJson] }

    it "returns all results for not.in when no value is present" $
      get "/items_with_different_col_types?int_data=not.in.()&select=int_data" `shouldRespondWith`
        [json| [{int_data: 1}] |] { matchHeaders = [matchContentTypeJson] }

    it "returns an empty result ignoring spaces" $
      get "/items_with_different_col_types?int_data=in.(    )" `shouldRespondWith`
        [json| [] |] { matchHeaders = [matchContentTypeJson] }

    it "only returns an empty result set if the in value is empty" $
      get "/items_with_different_col_types?int_data=in.( ,3,4)" `shouldRespondWith` 400

  describe "Embedding when column name = table name" $ do
    it "works with child embeds" $
      get "/being?select=*,descendant(*)&limit=1" `shouldRespondWith`
        [json|[{"being":1,"descendant":[{"descendant":1,"being":1},{"descendant":2,"being":1},{"descendant":3,"being":1}]}]|]
        { matchHeaders = [matchContentTypeJson] }
    it "works with many to many embeds" $
      get "/being?select=*,part(*)&limit=1" `shouldRespondWith`
        [json|[{"being":1,"part":[{"part":1}]}]|]
        { matchHeaders = [matchContentTypeJson] }

  describe "Foreign table" $ do
    it "can be queried by using regular filters" $
      get "/projects_dump?id=in.(1,2,3)" `shouldRespondWith`
        [json| [{"id":1,"name":"Windows 7","client_id":1}, {"id":2,"name":"Windows 10","client_id":1}, {"id":3,"name":"IOS","client_id":2}]|]
        { matchHeaders = [matchContentTypeJson] }
    it "can be queried with select, order and limit" $
      get "/projects_dump?select=id,name&order=id.desc&limit=3" `shouldRespondWith`
        [json| [{"id":5,"name":"Orphan"}, {"id":4,"name":"OSX"}, {"id":3,"name":"IOS"}] |]
        { matchHeaders = [matchContentTypeJson] }
