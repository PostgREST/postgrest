module Feature.Query.AggregateFunctionsSpec where

import Network.Wai (Application)

import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

allowed :: SpecWith ((), Application)
allowed =
  describe "aggregate functions" $ do
    context "performing a count without specifying a field" $ do
      it "returns the count of all rows when no other fields are selected" $
        get "/entities?select=count()" `shouldRespondWith`
          [json|[{ "count": 4 }]|] { matchHeaders = [matchContentTypeJson] }
      it "allows you to specify an alias for the count" $
        get "/entities?select=cnt:count()" `shouldRespondWith`
          [json|[{ "cnt": 4 }]|] { matchHeaders = [matchContentTypeJson] }
      it "allows you to cast the result of the count" $
        get "/entities?select=count()::text" `shouldRespondWith`
          [json|[{ "count": "4" }]|] { matchHeaders = [matchContentTypeJson] }
      it "returns the count grouped by all provided fields when other fields are selected" $
        get "/projects?select=c:count(),client_id&order=client_id.desc" `shouldRespondWith`
          [json|[{ "c": 1, "client_id": null }, { "c": 2, "client_id": 2 }, { "c": 2, "client_id": 1}]|] { matchHeaders = [matchContentTypeJson] }

    context "performing a count by using it as a column (backwards compat)" $ do
      it "returns the count of all rows when no other fields are selected" $
        get "/entities?select=count" `shouldRespondWith`
          [json|[{ "count": 4 }]|] { matchHeaders = [matchContentTypeJson] }
      it "returns the embedded count of another resource" $
        get "/clients?select=name,projects(count)'" `shouldRespondWith`
          [json|[{"name":"Microsoft","projects":[{"count": 2}]}, {"name":"Apple","projects":[{"count": 2}]}]|] { matchHeaders = [matchContentTypeJson] }

    context "performing an aggregation on one or more fields" $ do
      it "supports sum()" $
        get "/project_invoices?select=invoice_total.sum()" `shouldRespondWith`
          [json|[{"sum":8800}]|] { matchHeaders = [matchContentTypeJson] }
      it "supports avg()" $
        get "/project_invoices?select=invoice_total.avg()" `shouldRespondWith`
          [json|[{"avg":1100.0000000000000000}]|] { matchHeaders = [matchContentTypeJson] }
      it "supports min()" $
        get "/project_invoices?select=invoice_total.min()" `shouldRespondWith`
          [json|[{ "min": 100 }]|] { matchHeaders = [matchContentTypeJson] }
      it "supports max()" $
        get "/project_invoices?select=invoice_total.max()" `shouldRespondWith`
          [json|[{ "max": 4000 }]|] { matchHeaders = [matchContentTypeJson] }
      it "supports count()" $
        get "/project_invoices?select=invoice_total.count()" `shouldRespondWith`
          [json|[{ "count": 8 }]|] { matchHeaders = [matchContentTypeJson] }
      it "groups by any fields selected that do not have an aggregate applied" $
        get "/project_invoices?select=invoice_total.sum(),invoice_total.max(),invoice_total.min(),project_id&order=project_id.desc" `shouldRespondWith`
          [json|[
            {"sum":4100,"max":4000,"min":100,"project_id":4},
            {"sum":3200,"max":2000,"min":1200,"project_id":3},
            {"sum":1200,"max":700,"min":500,"project_id":2},
            {"sum":300,"max":200,"min":100,"project_id":1} ]|]
          { matchHeaders = [matchContentTypeJson] }
      it "supports the use of aliases on fields that will be used in the group by" $
        get "/project_invoices?select=invoice_total.sum(),invoice_total.max(),invoice_total.min(),pid:project_id&order=project_id.desc" `shouldRespondWith`
          [json|[
            {"sum":4100,"max":4000,"min":100,"pid":4},
            {"sum":3200,"max":2000,"min":1200,"pid":3},
            {"sum":1200,"max":700,"min":500,"pid":2},
            {"sum":300,"max":200,"min":100,"pid":1}]|]
          { matchHeaders = [matchContentTypeJson] }
      it "allows you to specify an alias for the aggregate" $
        get "/project_invoices?select=total_charged:invoice_total.sum(),project_id&order=project_id.desc" `shouldRespondWith`
          [json|[
             {"total_charged":4100,"project_id":4},
             {"total_charged":3200,"project_id":3},
             {"total_charged":1200,"project_id":2},
             {"total_charged":300,"project_id":1}]|] { matchHeaders = [matchContentTypeJson] }
      it "allows you to cast the result of the aggregate" $
        get "/project_invoices?select=total_charged:invoice_total.sum()::text,project_id&order=project_id.desc" `shouldRespondWith`
          [json|[
             {"total_charged":"4100","project_id":4},
             {"total_charged":"3200","project_id":3},
             {"total_charged":"1200","project_id":2},
             {"total_charged":"300","project_id":1}]|] { matchHeaders = [matchContentTypeJson] }
      it "allows you to cast the input argument of the aggregate" $
        get "/trash_details?select=jsonb_col->>key::integer.sum()" `shouldRespondWith`
          [json|[{"sum": 24}]|] { matchHeaders = [matchContentTypeJson] }
      it "allows the combination of an alias, a before cast, and an after cast" $
        get "/trash_details?select=s:jsonb_col->>key::integer.sum()::text" `shouldRespondWith`
          [json|[{"s": "24"}]|] { matchHeaders = [matchContentTypeJson] }
      it "supports use of aggregates on RPC functions that return table values" $
        get "/rpc/getallprojects?select=id.max()" `shouldRespondWith`
          [json|[{"max": 5}]|] { matchHeaders = [matchContentTypeJson] }
      it "allows the use of an JSON-embedded relationship column as part of the group by" $
        get "/project_invoices?select=project_id,total:invoice_total.sum(),projects(name)&order=project_id" `shouldRespondWith`
          [json|[
            {"project_id": 1, "total": 300,  "projects": {"name": "Windows 7"}},
            {"project_id": 2, "total": 1200, "projects": {"name": "Windows 10"}},
            {"project_id": 3, "total": 3200, "projects": {"name": "IOS"}},
            {"project_id": 4, "total": 4100, "projects": {"name": "OSX"}}]|] { matchHeaders = [matchContentTypeJson] }
    context "performing aggregations that involve JSON-embedded relationships" $ do
      it "supports sum()" $
        get "/projects?select=name,project_invoices(invoice_total.sum())" `shouldRespondWith`
          [json|[
            {"name":"Windows 7","project_invoices":[{"sum": 300}]},
            {"name":"Windows 10","project_invoices":[{"sum": 1200}]},
            {"name":"IOS","project_invoices":[{"sum": 3200}]},
            {"name":"OSX","project_invoices":[{"sum": 4100}]},
            {"name":"Orphan","project_invoices":[{"sum": null}]}]|]
          { matchHeaders = [matchContentTypeJson] }
      it "supports max()" $
        get "/projects?select=name,project_invoices(invoice_total.max())" `shouldRespondWith`
          [json|[{"name":"Windows 7","project_invoices":[{"max": 200}]},
            {"name":"Windows 10","project_invoices":[{"max": 700}]},
            {"name":"IOS","project_invoices":[{"max": 2000}]},
            {"name":"OSX","project_invoices":[{"max": 4000}]},
            {"name":"Orphan","project_invoices":[{"max": null}]}]|]
          { matchHeaders = [matchContentTypeJson] }
      it "supports avg()" $
        get "/projects?select=name,project_invoices(invoice_total.avg())" `shouldRespondWith`
          [json|[{"name":"Windows 7","project_invoices":[{"avg": 150.0000000000000000}]},
            {"name":"Windows 10","project_invoices":[{"avg": 600.0000000000000000}]},
            {"name":"IOS","project_invoices":[{"avg": 1600.0000000000000000}]},
            {"name":"OSX","project_invoices":[{"avg": 2050.0000000000000000}]},
            {"name":"Orphan","project_invoices":[{"avg": null}]}]|]
          { matchHeaders = [matchContentTypeJson] }
      it "supports min()" $
        get "/projects?select=name,project_invoices(invoice_total.min())" `shouldRespondWith`
          [json|[{"name":"Windows 7","project_invoices":[{"min": 100}]},
            {"name":"Windows 10","project_invoices":[{"min": 500}]},
            {"name":"IOS","project_invoices":[{"min": 1200}]},
            {"name":"OSX","project_invoices":[{"min": 100}]},
            {"name":"Orphan","project_invoices":[{"min": null}]}]|]
          { matchHeaders = [matchContentTypeJson] }
      it "supports all at once" $
        get "/projects?select=name,project_invoices(invoice_total.max(),invoice_total.min(),invoice_total.avg(),invoice_total.sum(),invoice_total.count())" `shouldRespondWith`
          [json|[
            {"name":"Windows 7","project_invoices":[{"avg": 150.0000000000000000, "max": 200, "min": 100, "sum": 300, "count": 2}]},
            {"name":"Windows 10","project_invoices":[{"avg": 600.0000000000000000, "max": 700, "min": 500, "sum": 1200, "count": 2}]},
            {"name":"IOS","project_invoices":[{"avg": 1600.0000000000000000, "max": 2000, "min": 1200, "sum": 3200, "count": 2}]},
            {"name":"OSX","project_invoices":[{"avg": 2050.0000000000000000, "max": 4000, "min": 100, "sum": 4100, "count": 2}]},
            {"name":"Orphan","project_invoices":[{"avg": null, "max": null, "min": null, "sum": null, "count": 0}]}]|]
          { matchHeaders = [matchContentTypeJson] }

    context "performing aggregations on spreaded fields from an embedded resource" $ do
      it "supports the use of aggregates on spreaded fields" $ do
        get "/budget_expenses?select=total_expenses:expense_amount.sum(),...budget_categories(budget_owner,total_budget:budget_amount.sum())&order=budget_categories(budget_owner)" `shouldRespondWith`
          [json|[
            {"total_expenses": 600.52,"budget_owner": "Brian Smith",  "total_budget": 2000.42},
            {"total_expenses": 100.22, "budget_owner": "Jane Clarkson","total_budget": 7000.41},
            {"total_expenses": 900.27, "budget_owner": "Sally Hughes", "total_budget": 500.23}]|]
          { matchHeaders = [matchContentTypeJson] }
      it "supports the use of aggregates on spreaded fields when only aggregates are supplied" $ do
        get "/budget_expenses?select=...budget_categories(total_budget:budget_amount.sum())" `shouldRespondWith`
          [json|[{"total_budget": 9501.06}]|]
          { matchHeaders = [matchContentTypeJson] }

disallowed :: SpecWith ((), Application)
disallowed =
  describe "attempting to use an aggregate when aggregate functions are disallowed" $ do
    it "prevents the use of aggregates" $
      get "/project_invoices?select=invoice_total.sum()" `shouldRespondWith`
        [json|{
          "hint":null,
          "details":null,
          "code":"PGRST123",
          "message":"Use of aggregate functions is not allowed"
        }|]
        { matchStatus = 400
        , matchHeaders = [matchContentTypeJson] }
