module Feature.Query.ExplainPlanSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config.PgVersion (PgVersion)
import Protolude                  hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec _ =
  describe "Explain plan" $
    it "outputs the plan for a simple filter" $
      request methodGet "/projects?id=in.(1,2,3)"
              (acceptHdrs "application/vnd.pgrst.plan+json") ""
        `shouldRespondWith`
        [json| [
          {
            "Plan": {
              "Node Type": "Aggregate",
              "Strategy": "Plain",
              "Partial Mode": "Simple",
              "Parallel Aware": false,
              "Async Capable": false,
              "Startup Cost": 15.61,
              "Total Cost": 15.63,
              "Plan Rows": 1,
              "Plan Width": 112,
              "Plans": [
                {
                  "Node Type": "Bitmap Heap Scan",
                  "Parent Relationship": "Outer",
                  "Parallel Aware": false,
                  "Async Capable": false,
                  "Relation Name": "projects",
                  "Alias": "projects",
                  "Startup Cost": 8.48,
                  "Total Cost": 15.59,
                  "Plan Rows": 3,
                  "Plan Width": 40,
                  "Recheck Cond": "(id = ANY ('{1,2,3}'::integer[]))",
                  "Plans": [
                    {
                      "Node Type": "Bitmap Index Scan",
                      "Parent Relationship": "Outer",
                      "Parallel Aware": false,
                      "Async Capable": false,
                      "Index Name": "projects_pkey",
                      "Startup Cost": 0.00,
                      "Total Cost": 8.48,
                      "Plan Rows": 3,
                      "Plan Width": 0,
                      "Index Cond": "(id = ANY ('{1,2,3}'::integer[]))"
                    }
                  ]
                }
              ]
            }
          }
        ] |]
        { matchStatus  = 200
        , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.plan+json; charset=utf-8"]
        }
