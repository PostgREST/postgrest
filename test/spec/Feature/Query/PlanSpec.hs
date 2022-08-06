{-# LANGUAGE MultiWayIf #-}

module Feature.Query.PlanSpec where

import Control.Lens     ((^?))
import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import           Data.Aeson.Lens
import           Data.Aeson.QQ
import qualified Data.ByteString.Lazy as LBS
import           Network.HTTP.Types
import           Test.Hspec           hiding (pendingWith)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import PostgREST.Config.PgVersion (PgVersion, pgVersion120,
                                   pgVersion130, pgVersion100)
import Protolude                  hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec actualPgVersion = do
  describe "read table/view plan" $ do
    it "outputs the total cost for a single filter on a table" $ do
      r <- request methodGet "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan") ""

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe`
          if actualPgVersion > pgVersion120
            then Just [aesonQQ|15.63|]
            else Just [aesonQQ|15.69|]

    it "outputs the total cost for a single filter on a view" $ do
      r <- request methodGet "/projects_view?id=gt.2"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe`
          if actualPgVersion > pgVersion120
            then Just [aesonQQ|24.28|]
            else Just [aesonQQ|32.28|]

    it "outputs blocks info when using the buffers option" $
      if actualPgVersion >= pgVersion130
        then do
          r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=buffers") ""

          let blocks  = simpleBody r ^? nth 0 . key "Planning"
              resHeaders = simpleHeaders r

          liftIO $ do
            resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=buffers; charset=utf-8")
            blocks `shouldBe`
              Just [aesonQQ|
                {
                  "Shared Hit Blocks": 0,
                  "Shared Read Blocks": 0,
                  "Shared Dirtied Blocks": 0,
                  "Shared Written Blocks": 0,
                  "Local Hit Blocks": 0,
                  "Local Read Blocks": 0,
                  "Local Dirtied Blocks": 0,
                  "Local Written Blocks": 0,
                  "Temp Read Blocks": 0,
                  "Temp Written Blocks": 0
                }
              |]
        else do
          -- analyze is required for buffers on pg < 13
          r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=analyze|buffers") ""

          let blocks  = simpleBody r ^? nth 0 . key "Plan" . key "Shared Hit Blocks"
              resHeaders = simpleHeaders r

          liftIO $ do
            resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=analyze|buffers; charset=utf-8")
            blocks `shouldBe` Just [aesonQQ| 1.0 |]

    when (actualPgVersion >= pgVersion120) $
      it "outputs the search path when using the settings option" $ do
        r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=settings") ""

        let searchPath  = simpleBody r ^? nth 0 . key "Settings"
            resHeaders = simpleHeaders r

        liftIO $ do
          resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=settings; charset=utf-8")
          searchPath `shouldBe`
            Just [aesonQQ|
              {
                "search_path": "\"test\""
              }
            |]

    when (actualPgVersion >= pgVersion130) $
      it "outputs WAL info when using the wal option" $ do
        r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=analyze|wal") ""

        let walRecords  = simpleBody r ^? nth 0 . key "Plan" . key "WAL Records"
            resHeaders = simpleHeaders r

        liftIO $ do
          resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=analyze|wal; charset=utf-8")
          walRecords `shouldBe` Just [aesonQQ|0|]

    it "outputs columns info when using the verbose option" $ do
      r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=verbose") ""

      let cols  = simpleBody r ^? nth 0 . key "Plan" . key "Plans" . nth 0 . key "Output"
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=verbose; charset=utf-8")
        cols `shouldBe` Just [aesonQQ| ["projects.id", "projects.name", "projects.client_id"] |]

    it "outputs the plan for application/json " $ do
      r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; for=\"application/json\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; options=verbose; charset=utf-8")
        aggCol `shouldBe`
          if actualPgVersion >= pgVersion120
            then Just [aesonQQ| "(COALESCE(json_agg(ROW(projects.id, projects.name, projects.client_id)), '[]'::json))::character varying" |]
            else Just [aesonQQ| "(COALESCE(json_agg(ROW(pgrst_source.id, pgrst_source.name, pgrst_source.client_id)), '[]'::json))::character varying" |]

    it "outputs the plan for application/vnd.pgrst.object " $ do
      r <- request methodGet "/projects_view" (acceptHdrs "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object+json\"; options=verbose; charset=utf-8")
        aggCol `shouldBe`
          if actualPgVersion >= pgVersion120
            then Just [aesonQQ| "COALESCE(((json_agg(ROW(projects.id, projects.name, projects.client_id)) -> 0))::text, 'null'::text)" |]
            else Just [aesonQQ| "COALESCE(((json_agg(ROW(pgrst_source.id, pgrst_source.name, pgrst_source.client_id)) -> 0))::text, 'null'::text)" |]

  describe "writes plans" $ do
    it "outputs the total cost for an insert" $ do
      r <- request methodPost "/projects"
             (acceptHdrs "application/vnd.pgrst.plan") [json|{"id":100, "name": "Project 100"}|]

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe`
          if actualPgVersion > pgVersion120
            then Just [aesonQQ|3.28|]
            else Just [aesonQQ|3.33|]

    it "outputs the total cost for an update" $ do
      r <- request methodPatch "/projects?id=eq.3"
             (acceptHdrs "application/vnd.pgrst.plan") [json|{"name": "Patched Project"}|]

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe`
          if actualPgVersion > pgVersion120
            then Just [aesonQQ|12.45|]
            else Just [aesonQQ|12.5|]

    it "outputs the total cost for a delete" $ do
      r <- request methodDelete "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan") ""

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` Just [aesonQQ|15.68|]

    it "outputs the total cost for a single upsert" $ do
      r <- request methodPut "/tiobe_pls?name=eq.Go"
            (acceptHdrs "application/vnd.pgrst.plan")
            [json| [ { "name": "Go", "rank": 19 } ]|]

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe`
          if actualPgVersion >= pgVersion120
            then Just [aesonQQ|1.3|]
            else Just [aesonQQ|1.35|]

    it "outputs the plan for application/vnd.pgrst.object" $ do
      r <- request methodDelete "/projects?id=eq.6"
        [("Prefer", "return=representation"), ("Accept", "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object\"; options=verbose")] ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 3
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object+json\"; options=verbose; charset=utf-8")
        aggCol `shouldBe` Just [aesonQQ| "COALESCE(((json_agg(ROW(projects.id, projects.name, projects.client_id)) -> 0))::text, 'null'::text)" |]

  describe "function plan" $ do
    it "outputs the total cost for a function call" $ do
      r <- request methodGet "/rpc/getallprojects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan") ""

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` Just [aesonQQ|68.57|]

    it "outputs the plan for text/xml" $ do
      r <- request methodGet "/rpc/return_scalar_xml"
        (acceptHdrs "application/vnd.pgrst.plan+json; for=\"text/xml\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"text/xml\"; options=verbose; charset=utf-8")
        aggCol `shouldBe` Just [aesonQQ| "COALESCE(xmlagg(return_scalar_xml.pgrst_scalar), ''::xml)" |]

  describe "text format" $
    it "outputs the total cost for a function call" $ do
      r <- request methodGet "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan+text") ""

      let resBody    = simpleBody r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+text; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        resBody `shouldSatisfy` (\t -> LBS.take 9 t == "Aggregate")

  describe "resource embedding costs" $ do
    it "a one to many doesn't surpass a threshold" $ do
      r <- request methodGet "/clients?select=*,projects(*)&id=eq.1"
             (acceptHdrs "application/vnd.pgrst.plan") ""

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
      liftIO $ totalCost `shouldBe`
          if actualPgVersion > pgVersion120
            then Just [aesonQQ|58.26|]
            else Just [aesonQQ|33.25|]

    it "a many to one doesn't surpass a threshold" $ do
      r <- request methodGet "/projects?select=*,clients(*)&id=eq.1"
             (acceptHdrs "application/vnd.pgrst.plan") ""

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
      liftIO $ totalCost `shouldBe`
          if actualPgVersion > pgVersion120
            then Just [aesonQQ|16.39|]
            else Just [aesonQQ|16.41|]

    it "a many to many doesn't surpass a threshold" $ do
      r <- request methodGet "/users?select=*,tasks(*)&id=eq.1"
             (acceptHdrs "application/vnd.pgrst.plan") ""

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
      liftIO $ totalCost `shouldBe`
          if | actualPgVersion > pgVersion120 -> Just [aesonQQ|130.44|]
             | actualPgVersion > pgVersion100 -> Just [aesonQQ|69.34|]
             | otherwise                      -> Just [aesonQQ|70.79|]

disabledSpec :: SpecWith ((), Application)
disabledSpec =
  it "doesn't work if db-plan-enabled=false(the default)" $ do
    request methodGet "/projects?id=in.(1,2,3)"
         (acceptHdrs "application/vnd.pgrst.plan") ""
      `shouldRespondWith` 415

    request methodGet "/rpc/getallprojects?id=in.(1,2,3)"
      (acceptHdrs "application/vnd.pgrst.plan") ""
      `shouldRespondWith` 415

    request methodDelete "/projects?id=in.(1,2,3)"
           (acceptHdrs "application/vnd.pgrst.plan") ""
      `shouldRespondWith` 415
