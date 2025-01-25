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
      context "to-one spread relationships" $ do
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
        it "supports aggregates from a spread relationships grouped by spreaded fields from other relationships" $ do
          get "/processes?select=...process_costs(cost.sum()),...process_categories(name)" `shouldRespondWith`
            [json|[
              {"sum": 400.00, "name": "Batch"},
              {"sum": 350.00, "name": "Mass"}]|]
            { matchHeaders = [matchContentTypeJson] }
          get "/processes?select=...process_costs(cost_sum:cost.sum()),...process_categories(category:name)" `shouldRespondWith`
            [json|[
              {"cost_sum": 400.00, "category": "Batch"},
              {"cost_sum": 350.00, "category": "Mass"}]|]
            { matchHeaders = [matchContentTypeJson] }
        it "supports aggregates on spreaded fields from nested relationships" $ do
          get "/process_supervisor?select=...processes(factory_id,...process_costs(cost.sum()))" `shouldRespondWith`
            [json|[
              {"factory_id": 3, "sum": 110.00},
              {"factory_id": 2, "sum": 500.00},
              {"factory_id": 1, "sum": 350.00}]|]
            { matchHeaders = [matchContentTypeJson] }
          get "/process_supervisor?select=...processes(factory_id,...process_costs(cost_sum:cost.sum()))" `shouldRespondWith`
            [json|[
              {"factory_id": 3, "cost_sum": 110.00},
              {"factory_id": 2, "cost_sum": 500.00},
              {"factory_id": 1, "cost_sum": 350.00}]|]
            { matchHeaders = [matchContentTypeJson] }
        it "supports aggregates on spreaded fields from nested relationships, grouped by a regular nested relationship" $ do
          get "/process_supervisor?select=...processes(factories(name),...process_costs(cost.sum()))" `shouldRespondWith`
            [json|[
              {"factories": {"name": "Factory A"}, "sum": 350.00},
              {"factories": {"name": "Factory B"}, "sum": 500.00},
              {"factories": {"name": "Factory C"}, "sum": 110.00}]|]
            { matchHeaders = [matchContentTypeJson] }
          get "/process_supervisor?select=...processes(factory:factories(name),...process_costs(cost_sum:cost.sum()))" `shouldRespondWith`
            [json|[
              {"factory": {"name": "Factory A"}, "cost_sum": 350.00},
              {"factory": {"name": "Factory B"}, "cost_sum": 500.00},
              {"factory": {"name": "Factory C"}, "cost_sum": 110.00}]|]
            { matchHeaders = [matchContentTypeJson] }
        it "supports aggregates on spreaded fields from nested relationships, grouped by spreaded fields from other nested relationships" $ do
          get "/process_supervisor?select=supervisor_id,...processes(...process_costs(cost.sum()),...process_categories(name))&order=supervisor_id" `shouldRespondWith`
            [json|[
              {"supervisor_id": 1, "sum": 220.00, "name": "Batch"},
              {"supervisor_id": 2, "sum": 70.00, "name": "Batch"},
              {"supervisor_id": 2, "sum": 200.00, "name": "Mass"},
              {"supervisor_id": 3, "sum": 180.00, "name": "Batch"},
              {"supervisor_id": 3, "sum": 110.00, "name": "Mass"},
              {"supervisor_id": 4, "sum": 180.00, "name": "Batch"}]|]
            { matchHeaders = [matchContentTypeJson] }
          get "/process_supervisor?select=supervisor_id,...processes(...process_costs(cost_sum:cost.sum()),...process_categories(category:name))&order=supervisor_id" `shouldRespondWith`
            [json|[
              {"supervisor_id": 1, "cost_sum": 220.00, "category": "Batch"},
              {"supervisor_id": 2, "cost_sum": 70.00, "category": "Batch"},
              {"supervisor_id": 2, "cost_sum": 200.00, "category": "Mass"},
              {"supervisor_id": 3, "cost_sum": 180.00, "category": "Batch"},
              {"supervisor_id": 3, "cost_sum": 110.00, "category": "Mass"},
              {"supervisor_id": 4, "cost_sum": 180.00, "category": "Batch"}]|]
            { matchHeaders = [matchContentTypeJson] }
        it "supports aggregates on spreaded fields from nested relationships, grouped by spreaded fields from other nested relationships, using a nested relationship as top parent" $ do
          get "/supervisors?select=name,process_supervisor(...processes(...process_costs(cost.sum()),...process_categories(name)))" `shouldRespondWith`
            [json|[
              {"name": "Mary", "process_supervisor": [{"name": "Batch", "sum": 220.00}]},
              {"name": "John", "process_supervisor": [{"name": "Batch", "sum": 70.00}, {"name": "Mass", "sum": 200.00}]},
              {"name": "Peter", "process_supervisor": [{"name": "Batch", "sum": 180.00}, {"name": "Mass", "sum": 110.00}]},
              {"name": "Sarah", "process_supervisor": [{"name": "Batch", "sum": 180.00}]},
              {"name": "Jane", "process_supervisor": []}]|]
            { matchHeaders = [matchContentTypeJson] }
          get "/supervisors?select=name,process_supervisor(...processes(...process_costs(cost_sum:cost.sum()),...process_categories(category:name)))" `shouldRespondWith`
            [json|[
              {"name": "Mary", "process_supervisor": [{"category": "Batch", "cost_sum": 220.00}]},
              {"name": "John", "process_supervisor": [{"category": "Batch", "cost_sum": 70.00}, {"category": "Mass", "cost_sum": 200.00}]},
              {"name": "Peter", "process_supervisor": [{"category": "Batch", "cost_sum": 180.00}, {"category": "Mass", "cost_sum": 110.00}]},
              {"name": "Sarah", "process_supervisor": [{"category": "Batch", "cost_sum": 180.00}]},
              {"name": "Jane", "process_supervisor": []}]|]
            { matchHeaders = [matchContentTypeJson] }

        context "supports count() aggregate without specifying a field" $ do
          it "works by itself in the embedded resource" $ do
            get "/process_supervisor?select=supervisor_id,...processes(count())&order=supervisor_id" `shouldRespondWith`
              [json|[
                {"supervisor_id": 1, "count": 2},
                {"supervisor_id": 2, "count": 2},
                {"supervisor_id": 3, "count": 3},
                {"supervisor_id": 4, "count": 1}]|]
              { matchHeaders = [matchContentTypeJson] }
            get "/process_supervisor?select=supervisor_id,...processes(processes_count:count())&order=supervisor_id" `shouldRespondWith`
              [json|[
                {"supervisor_id": 1, "processes_count": 2},
                {"supervisor_id": 2, "processes_count": 2},
                {"supervisor_id": 3, "processes_count": 3},
                {"supervisor_id": 4, "processes_count": 1}]|]
              { matchHeaders = [matchContentTypeJson] }
          it "works alongside other columns in the embedded resource" $ do
            get "/process_supervisor?select=...supervisors(id,count())&order=supervisors(id)" `shouldRespondWith`
              [json|[
                {"id": 1, "count": 2},
                {"id": 2, "count": 2},
                {"id": 3, "count": 3},
                {"id": 4, "count": 1}]|]
              { matchHeaders = [matchContentTypeJson] }
            get "/process_supervisor?select=...supervisors(supervisor:id,supervisor_count:count())&order=supervisors(supervisor)" `shouldRespondWith`
              [json|[
                {"supervisor": 1, "supervisor_count": 2},
                {"supervisor": 2, "supervisor_count": 2},
                {"supervisor": 3, "supervisor_count": 3},
                {"supervisor": 4, "supervisor_count": 1}]|]
              { matchHeaders = [matchContentTypeJson] }
          it "works on nested resources" $ do
            get "/process_supervisor?select=supervisor_id,...processes(...process_costs(count()))&order=supervisor_id" `shouldRespondWith`
              [json|[
                {"supervisor_id": 1, "count": 2},
                {"supervisor_id": 2, "count": 2},
                {"supervisor_id": 3, "count": 3},
                {"supervisor_id": 4, "count": 1}]|]
              { matchHeaders = [matchContentTypeJson] }
            get "/process_supervisor?select=supervisor:supervisor_id,...processes(...process_costs(process_costs_count:count()))&order=supervisor_id" `shouldRespondWith`
              [json|[
                {"supervisor": 1, "process_costs_count": 2},
                {"supervisor": 2, "process_costs_count": 2},
                {"supervisor": 3, "process_costs_count": 3},
                {"supervisor": 4, "process_costs_count": 1}]|]
              { matchHeaders = [matchContentTypeJson] }
          it "works on nested resources grouped by spreaded fields" $ do
            get "/process_supervisor?select=...processes(factory_id,...process_costs(count()))&order=processes(factory_id)" `shouldRespondWith`
              [json|[
                {"factory_id": 1, "count": 2},
                {"factory_id": 2, "count": 4},
                {"factory_id": 3, "count": 2}]|]
              { matchHeaders = [matchContentTypeJson] }
            get "/process_supervisor?select=...processes(factory:factory_id,...process_costs(process_costs_count:count()))&order=processes(factory)" `shouldRespondWith`
              [json|[
                {"factory": 1, "process_costs_count": 2},
                {"factory": 2, "process_costs_count": 4},
                {"factory": 3, "process_costs_count": 2}]|]
              { matchHeaders = [matchContentTypeJson] }
          it "works on different levels of the nested resources at the same time" $
            get "/process_supervisor?select=...processes(factory:factory_id,processes_count:count(),...process_costs(process_costs_count:count()))&order=processes(factory)" `shouldRespondWith`
              [json|[
                {"factory": 1, "processes_count": 2, "process_costs_count": 2},
                {"factory": 2, "processes_count": 4, "process_costs_count": 4},
                {"factory": 3, "processes_count": 2, "process_costs_count": 2}]|]
              { matchHeaders = [matchContentTypeJson] }

      context "to-many spread relationships" $ do
        it "supports the use of aggregates" $ do
          get "/factories?select=name,...factory_buildings(type,size.sum())" `shouldRespondWith`
            [json|[
              {"name":"Factory A","type":["A"],"sum":[350]},
              {"name":"Factory B","type":["B", "C"],"sum":[50, 120]},
              {"name":"Factory C","type":["B"],"sum":[240]},
              {"name":"Factory D","type":["A"],"sum":[310]}]|]
            { matchHeaders = [matchContentTypeJson] }
        it "supports the use of aggregates without grouping by any fields" $ do
          get "/factories?select=name,...factory_buildings(size.sum())" `shouldRespondWith`
            [json|[
              {"name":"Factory A","sum":350},
              {"name":"Factory B","sum":170},
              {"name":"Factory C","sum":240},
              {"name":"Factory D","sum":310}]|]
            { matchHeaders = [matchContentTypeJson] }
        it "supports many aggregates at the same time" $ do
          get "/factories?select=name,...factory_buildings(size.min(),size.max(),size.sum())" `shouldRespondWith`
            [json|[
              {"name":"Factory A","min":150,"max":200,"sum":350},
              {"name":"Factory B","min":50,"max":120,"sum":170},
              {"name":"Factory C","min":240,"max":240,"sum":240},
              {"name":"Factory D","min":310,"max":310,"sum":310}]|]
            { matchHeaders = [matchContentTypeJson] }
        it "supports aggregates inside nested to-one spread relationships" $ do
          get "/supervisors?select=name,...processes(...process_costs(cost.sum()))&order=name" `shouldRespondWith`
            [json|[
              {"name":"Jane","sum":null},
              {"name":"John","sum":270.00},
              {"name":"Mary","sum":220.00},
              {"name":"Peter","sum":290.00},
              {"name":"Sarah","sum":180.00}]|]
            { matchHeaders = [matchContentTypeJson] }
          get "/supervisors?select=supervisor:name,...processes(...process_costs(cost_sum:cost.sum()))&order=name" `shouldRespondWith`
            [json|[
              {"supervisor":"Jane","cost_sum":null},
              {"supervisor":"John","cost_sum":270.00},
              {"supervisor":"Mary","cost_sum":220.00},
              {"supervisor":"Peter","cost_sum":290.00},
              {"supervisor":"Sarah","cost_sum":180.00}]|]
            { matchHeaders = [matchContentTypeJson] }
        it "supports aggregates alongside the aggregates nested in to-one spread relationships" $ do
          get "/supervisors?select=name,...processes(id.count(),...process_costs(cost.sum()))&order=name" `shouldRespondWith`
            [json|[
              {"name":"Jane","count":0,"sum":null},
              {"name":"John","count":2,"sum":270.00},
              {"name":"Mary","count":2,"sum":220.00},
              {"name":"Peter","count":3,"sum":290.00},
              {"name":"Sarah","count":1,"sum":180.00}]|]
            { matchHeaders = [matchContentTypeJson] }
          get "/supervisors?select=supervisor:name,...processes(process_count:count(),...process_costs(cost_sum:cost.sum()))&order=name" `shouldRespondWith`
            [json|[
              {"supervisor":"Jane","process_count":0,"cost_sum":null},
              {"supervisor":"John","process_count":2,"cost_sum":270.00},
              {"supervisor":"Mary","process_count":2,"cost_sum":220.00},
              {"supervisor":"Peter","process_count":3,"cost_sum":290.00},
              {"supervisor":"Sarah","process_count":1,"cost_sum":180.00}]|]
            { matchHeaders = [matchContentTypeJson] }
        it "supports aggregates on nested relationships" $ do
          get "/operators?select=name,...processes(id,...factories(...factory_buildings(size.sum())))&order=name" `shouldRespondWith`
            [json|[
              {"name":"Alfred","id":[6, 7],"sum":[240, 240]},
              {"name":"Anne","id":[1, 2, 4],"sum":[350, 350, 170]},
              {"name":"Jeff","id":[2, 3, 4, 6],"sum":[350, 170, 170, 240]},
              {"name":"Liz","id":[],"sum":[]},
              {"name":"Louis","id":[1, 2],"sum":[350, 350]}]|]
            { matchHeaders = [matchContentTypeJson] }
          get "/operators?select=name,...processes(process_id:id,...factories(...factory_buildings(factory_building_size_sum:size.sum())))&order=name" `shouldRespondWith`
            [json|[
              {"name":"Alfred","process_id":[6, 7],"factory_building_size_sum":[240, 240]},
              {"name":"Anne","process_id":[1, 2, 4],"factory_building_size_sum":[350, 350, 170]},
              {"name":"Jeff","process_id":[2, 3, 4, 6],"factory_building_size_sum":[350, 170, 170, 240]},
              {"name":"Liz","process_id":[],"factory_building_size_sum":[]},
              {"name":"Louis","process_id":[1, 2],"factory_building_size_sum":[350, 350]}]|]
            { matchHeaders = [matchContentTypeJson] }
          get "/operators?select=name,...processes(process_id:id,...factories(...factory_buildings(factory_building_size_sum:size.sum())))&processes.order=id.desc&order=name" `shouldRespondWith`
            [json|[
              {"name":"Alfred","process_id":[7, 6],"factory_building_size_sum":[240, 240]},
              {"name":"Anne","process_id":[4, 2, 1],"factory_building_size_sum":[170, 350, 350]},
              {"name":"Jeff","process_id":[6, 4, 3, 2],"factory_building_size_sum":[240, 170, 170, 350]},
              {"name":"Liz","process_id":[],"factory_building_size_sum":[]},
              {"name":"Louis","process_id":[2, 1],"factory_building_size_sum":[350, 350]}]|]
            { matchHeaders = [matchContentTypeJson] }

        context "supports count() aggregate without specifying a field" $ do
          context "one-to-many" $ do
            it "works by itself in the embedded resource" $ do
              get "/factories?select=name,...processes(count())&order=name" `shouldRespondWith`
                [json|[
                  {"name":"Factory A","count":2},
                  {"name":"Factory B","count":2},
                  {"name":"Factory C","count":4},
                  {"name":"Factory D","count":0}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/factories?select=factory:name,...processes(processes_count:count())&order=name" `shouldRespondWith`
                [json|[
                  {"factory":"Factory A","processes_count":2},
                  {"factory":"Factory B","processes_count":2},
                  {"factory":"Factory C","processes_count":4},
                  {"factory":"Factory D","processes_count":0}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works alongside other columns in the embedded resource" $ do
              get "/factories?select=name,...processes(category_id,count())&order=name" `shouldRespondWith`
                [json|[
                  {"name":"Factory A","category_id":[1, 2],"count":[1, 1]},
                  {"name":"Factory B","category_id":[1],"count":[2]},
                  {"name":"Factory C","category_id":[2],"count":[4]},
                  {"name":"Factory D","category_id":[],"count":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/factories?select=factory:name,...processes(category:category_id,process_count:count())&order=name" `shouldRespondWith`
                [json|[
                  {"factory":"Factory A","category":[1, 2],"process_count":[1, 1]},
                  {"factory":"Factory B","category":[1],"process_count":[2]},
                  {"factory":"Factory C","category":[2],"process_count":[4]},
                  {"factory":"Factory D","category":[],"process_count":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/factories?select=factory:name,...processes(*,process_count:count())&order=name" `shouldRespondWith`
                [json|[
                  {"factory":"Factory A","id":[1,2],"name":["Process A1","Process A2"],"factory_id":[1,1],"category_id":[1,2],"process_count":[1,1]},
                  {"factory":"Factory B","id":[3,4],"name":["Process B1","Process B2"],"factory_id":[2,2],"category_id":[1,1],"process_count":[1,1]},
                  {"factory":"Factory C","id":[5,6,7,8],"name":["Process C1","Process C2","Process XX","Process YY"],"factory_id":[3,3,3,3],"category_id":[2,2,2,2],"process_count":[1,1,1,1]},
                  {"factory":"Factory D","id":[],"name":[],"factory_id":[],"category_id":[],"process_count":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works on nested resources" $ do
              get "/factories?select=id,...processes(name,...process_supervisor(count()))&order=id" `shouldRespondWith`
                [json|[
                  {"id":1,"name":["Process A1", "Process A2"],"count":[1, 1]},
                  {"id":2,"name":["Process B1", "Process B2"],"count":[2, 2]},
                  {"id":3,"name":["Process C1", "Process C2", "Process XX", "Process YY"],"count":[1, 1, 0, 0]},
                  {"id":4,"name":[],"count":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/factories?select=id,...processes(process:name,...process_supervisor(ps_count:count()))&order=id" `shouldRespondWith`
                [json|[
                  {"id":1,"process":["Process A1", "Process A2"],"ps_count":[1, 1]},
                  {"id":2,"process":["Process B1", "Process B2"],"ps_count":[2, 2]},
                  {"id":3,"process":["Process C1", "Process C2", "Process XX", "Process YY"],"ps_count":[1, 1, 0, 0]},
                  {"id":4,"process":[],"ps_count":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/factories?select=id,...processes(process:name,...process_supervisor(ps_count:count()))&processes.order=name.desc&order=id" `shouldRespondWith`
                [json|[
                  {"id":1,"process":["Process A2", "Process A1"],"ps_count":[1, 1]},
                  {"id":2,"process":["Process B2", "Process B1"],"ps_count":[2, 2]},
                  {"id":3,"process":["Process YY", "Process XX", "Process C2", "Process C1"],"ps_count":[0, 0, 1, 1]},
                  {"id":4,"process":[],"ps_count":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works alongside to-one spread columns in the embedded resource" $
              get "/factories?select=factory:name,...processes(process_count:count(),...process_categories(category:name))&order=name" `shouldRespondWith`
                [json|[
                  {"factory":"Factory A","process_count":[1, 1],"category":["Batch", "Mass"]},
                  {"factory":"Factory B","process_count":[2],"category":["Batch"]},
                  {"factory":"Factory C","process_count":[4],"category":["Mass"]},
                  {"factory":"Factory D","process_count":[],"category":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works alongside non-spread embedded resources" $
              get "/factories?select=factory:name,...processes(process_count:count(),process_categories(category:name))&order=name" `shouldRespondWith`
                [json|[
                  {"factory":"Factory A","process_count":[1, 1],"process_categories":[{"category": "Batch"}, {"category": "Mass"}]},
                  {"factory":"Factory B","process_count":[2],"process_categories":[{"category": "Batch"}]},
                  {"factory":"Factory C","process_count":[4],"process_categories":[{"category": "Mass"}]},
                  {"factory":"Factory D","process_count":[],"process_categories":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works inside non-spread embedded resources" $ do
              get "/process_categories?select=...processes(name,factories(name,...factory_buildings(buildings:count())))" `shouldRespondWith`
                [json|[
                  {
                    "name":["Process A1", "Process B1", "Process B2"],
                    "factories":[{"name": "Factory A", "buildings": 2}, {"name": "Factory B", "buildings": 2}, {"name": "Factory B", "buildings": 2}]
                  },
                  {
                    "name":["Process A2", "Process C1", "Process C2", "Process XX", "Process YY"],
                    "factories":[{"name": "Factory A", "buildings": 2}, {"name": "Factory C", "buildings": 1}, {"name": "Factory C", "buildings": 1}, {"name": "Factory C", "buildings": 1}, {"name": "Factory C", "buildings": 1}]
                  }
                ]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/process_categories?select=...processes(name,operators(count()))" `shouldRespondWith`
                [json|[
                  {
                    "name":["Process A1", "Process B1", "Process B2"],
                    "operators":[[{"count": 2}], [{"count": 1}], [{"count": 2}]]
                  },
                  {
                    "name":["Process A2", "Process C1", "Process C2", "Process XX", "Process YY"],
                    "operators":[[{"count": 3}], [{"count": 0}], [{"count": 2}], [{"count": 1}], [{"count": 0}]]
                  }
                ]|]
                { matchHeaders = [matchContentTypeJson] }



          context "many-to-many" $ do
            it "works by itself in the embedded resource" $ do
              get "/supervisors?select=name,...processes(count())&order=name" `shouldRespondWith`
                [json|[
                  {"name":"Jane","count":0},
                  {"name":"John","count":2},
                  {"name":"Mary","count":2},
                  {"name":"Peter","count":3},
                  {"name":"Sarah","count":1}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/supervisors?select=supervisor:name,...processes(processes_count:count())&order=name" `shouldRespondWith`
                [json|[
                  {"supervisor":"Jane","processes_count":0},
                  {"supervisor":"John","processes_count":2},
                  {"supervisor":"Mary","processes_count":2},
                  {"supervisor":"Peter","processes_count":3},
                  {"supervisor":"Sarah","processes_count":1}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works alongside other columns in the embedded resource" $ do
              get "/supervisors?select=name,...processes(category_id,count())&order=name" `shouldRespondWith`
                [json|[
                  {"name":"Jane","category_id":[],"count":[]},
                  {"name":"John","category_id":[1, 2],"count":[1, 1]},
                  {"name":"Mary","category_id":[1],"count":[2]},
                  {"name":"Peter","category_id":[1, 2],"count":[1, 2]},
                  {"name":"Sarah","category_id":[1],"count":[1]}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/supervisors?select=supervisor:name,...processes(category:category_id,process_count:count())&order=name" `shouldRespondWith`
                [json|[
                  {"supervisor":"Jane","category":[],"process_count":[]},
                  {"supervisor":"John","category":[1, 2],"process_count":[1, 1]},
                  {"supervisor":"Mary","category":[1],"process_count":[2]},
                  {"supervisor":"Peter","category":[1, 2],"process_count":[1, 2]},
                  {"supervisor":"Sarah","category":[1],"process_count":[1]}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/supervisors?select=supervisor:name,...processes(*,process_count:count())&order=name" `shouldRespondWith`
                [json|[
                  {"supervisor":"Jane","id":[],"name":[],"factory_id":[],"category_id":[],"process_count":[]},
                  {"supervisor":"John","id":[2, 4],"name":["Process A2", "Process B2"],"factory_id":[1, 2],"category_id":[2, 1],"process_count":[1, 1]},
                  {"supervisor":"Mary","id":[1, 4],"name":["Process A1", "Process B2"],"factory_id":[1, 2],"category_id":[1, 1],"process_count":[1, 1]},
                  {"supervisor":"Peter","id":[3, 5, 6],"name":["Process B1", "Process C1", "Process C2"],"factory_id":[2, 3, 3],"category_id":[1, 2, 2],"process_count":[1, 1, 1]},
                  {"supervisor":"Sarah","id":[3],"name":["Process B1"],"factory_id":[2],"category_id":[1],"process_count":[1]}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works on nested resources" $ do
              get "/supervisors?select=id,...processes(name,...operators(count()))&order=id" `shouldRespondWith`
                [json|[
                  {"id":1,"name":["Process A1", "Process B2"],"count":[2, 2]},
                  {"id":2,"name":["Process A2", "Process B2"],"count":[3, 2]},
                  {"id":3,"name":["Process B1", "Process C1", "Process C2"],"count":[1, 0, 2]},
                  {"id":4,"name":["Process B1"],"count":[1]},
                  {"id":5,"name":[],"count":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/supervisors?select=supervisor:id,...processes(processes:name,...operators(operators_count:count()))&order=id" `shouldRespondWith`
                [json|[
                  {"supervisor":1,"processes":["Process A1", "Process B2"],"operators_count":[2, 2]},
                  {"supervisor":2,"processes":["Process A2", "Process B2"],"operators_count":[3, 2]},
                  {"supervisor":3,"processes":["Process B1", "Process C1", "Process C2"],"operators_count":[1, 0, 2]},
                  {"supervisor":4,"processes":["Process B1"],"operators_count":[1]},
                  {"supervisor":5,"processes":[],"operators_count":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/supervisors?select=supervisor:id,...processes(processes:name,...operators(operators_count:count()))&processes.order=name.desc&order=id" `shouldRespondWith`
                [json|[
                  {"supervisor":1,"processes":["Process B2", "Process A1"],"operators_count":[2, 2]},
                  {"supervisor":2,"processes":["Process B2", "Process A2"],"operators_count":[2, 3]},
                  {"supervisor":3,"processes":["Process C2", "Process C1", "Process B1"],"operators_count":[2, 0, 1]},
                  {"supervisor":4,"processes":["Process B1"],"operators_count":[1]},
                  {"supervisor":5,"processes":[],"operators_count":[]}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works alongside to-one spread columns in the embedded resource" $ do
              get "/supervisors?select=supervisor:name,...processes(process_count:count(),...process_categories(name))&order=name" `shouldRespondWith`
                [json|[
                  {"supervisor":"Jane","process_count":[],"name":[]},
                  {"supervisor":"John","process_count":[1, 1],"name":["Batch", "Mass"]},
                  {"supervisor":"Mary","process_count":[2],"name":["Batch"]},
                  {"supervisor":"Peter","process_count":[1, 2],"name":["Batch", "Mass"]},
                  {"supervisor":"Sarah","process_count":[1],"name":["Batch"]}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works alongside non-spread embedded resources" $
              get "/supervisors?select=supervisor:name,...processes(process_count:count(),process_categories(name))&order=name" `shouldRespondWith`
                [json|[
                  {"supervisor":"Jane","process_count":[],"process_categories":[]},
                  {"supervisor":"John","process_count":[1, 1],"process_categories":[{"name": "Batch"}, {"name": "Mass"}]},
                  {"supervisor":"Mary","process_count":[2],"process_categories":[{"name": "Batch"}]},
                  {"supervisor":"Peter","process_count":[1, 2],"process_categories":[{"name": "Batch"}, {"name": "Mass"}]},
                  {"supervisor":"Sarah","process_count":[1],"process_categories":[{"name": "Batch"}]}]|]
                { matchHeaders = [matchContentTypeJson] }
            it "works inside non-spread embedded resources" $ do
              get "/supervisors?select=...processes(name,factories(name,...factory_buildings(buildings:count())))" `shouldRespondWith`
                [json|[
                  {"name":["Process A1", "Process B2"],"factories":[{"name": "Factory A", "buildings": 2}, {"name": "Factory B", "buildings": 2}]},
                  {"name":["Process A2", "Process B2"],"factories":[{"name": "Factory A", "buildings": 2}, {"name": "Factory B", "buildings": 2}]},
                  {"name":["Process B1", "Process C1", "Process C2"],"factories":[{"name": "Factory B", "buildings": 2}, {"name": "Factory C", "buildings": 1}, {"name": "Factory C", "buildings": 1}]},
                  {"name":["Process B1"],"factories":[{"name": "Factory B", "buildings": 2}]},
                  {"name":[],"factories":[]}
                ]|]
                { matchHeaders = [matchContentTypeJson] }
              get "/supervisors?select=...processes(name,operators(count()))" `shouldRespondWith`
                [json|[
                  {"name":["Process A1", "Process B2"],"operators":[[{"count": 2}], [{"count": 2}]]},
                  {"name":["Process A2", "Process B2"],"operators":[[{"count": 3}], [{"count": 2}]]},
                  {"name":["Process B1", "Process C1", "Process C2"],"operators":[[{"count": 1}], [{"count": 0}], [{"count": 2}]]},
                  {"name":["Process B1"],"operators":[[{"count": 1}]]},
                  {"name":[],"operators":[]}
                ]|]
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

    it "prevents the use of aggregates on embedded relationships" $
      get "/projects?select=name,project_invoices(invoice_total.sum())" `shouldRespondWith`
        [json|{
          "hint":null,
          "details":null,
          "code":"PGRST123",
          "message":"Use of aggregate functions is not allowed"
        }|]
        { matchStatus = 400
        , matchHeaders = [matchContentTypeJson] }

    it "prevents the use of aggregates on to-one spread embeds" $
      get "/project_invoices?select=...projects(id.count())" `shouldRespondWith`
        [json|{
          "hint":null,
          "details":null,
          "code":"PGRST123",
          "message":"Use of aggregate functions is not allowed"
        }|]
        { matchStatus = 400
        , matchHeaders = [matchContentTypeJson] }

    it "prevents the use of aggregates on to-many spread embeds" $
      get "/factories?select=...processes(id.count())" `shouldRespondWith`
        [json|{
          "hint":null,
          "details":null,
          "code":"PGRST123",
          "message":"Use of aggregate functions is not allowed"
        }|]
        { matchStatus = 400
        , matchHeaders = [matchContentTypeJson] }
