{-# LANGUAGE MultiWayIf #-}

module Feature.Query.PlanSpec where

import Control.Lens     ((^?))
import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import           Data.Aeson.Lens
import           Data.Aeson.QQ
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Network.HTTP.Types
import           Test.Hspec           hiding (pendingWith)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import PostgREST.Config.PgVersion (PgVersion, pgVersion120,
                                   pgVersion130)
import Protolude                  hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec actualPgVersion = do
  describe "read table/view plan" $ do
    it "outputs the total cost for a single filter on a table" $ do
      r <- request methodGet "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe`
          if actualPgVersion > pgVersion120
            then 15.63
            else 15.69

    it "outputs the total cost for a single filter on a view" $ do
      r <- request methodGet "/projects_view?id=gt.2"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe`
          if actualPgVersion > pgVersion120
            then 24.28
            else 32.27

    it "outputs blocks info when using the buffers option" $
      if actualPgVersion >= pgVersion130
        then do
          r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=buffers") ""

          let resBody  = simpleBody r
              resHeaders = simpleHeaders r

          liftIO $ do
            resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; options=buffers; charset=utf-8")
            resBody `shouldSatisfy` (\t -> T.isInfixOf "Shared Hit Blocks" (decodeUtf8 $ LBS.toStrict t))
        else do
          -- analyze is required for buffers on pg < 13
          r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=analyze|buffers") ""

          let blocks  = simpleBody r ^? nth 0 . key "Plan" . key "Shared Hit Blocks"
              resHeaders = simpleHeaders r

          liftIO $ do
            resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; options=analyze|buffers; charset=utf-8")
            blocks `shouldBe` Just [aesonQQ| 1.0 |]

    when (actualPgVersion >= pgVersion120) $
      it "outputs the search path when using the settings option" $ do
        r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=settings") ""

        let searchPath  = simpleBody r ^? nth 0 . key "Settings"
            resHeaders = simpleHeaders r

        liftIO $ do
          resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; options=settings; charset=utf-8")
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
          resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; options=analyze|wal; charset=utf-8")
          walRecords `shouldBe` Just [aesonQQ|0|]

    it "outputs columns info when using the verbose option" $ do
      r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=verbose") ""

      let cols  = simpleBody r ^? nth 0 . key "Plan" . key "Plans" . nth 0 . key "Output"
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; options=verbose; charset=utf-8")
        cols `shouldBe` Just [aesonQQ| ["projects.id", "projects.name", "projects.client_id"] |]

    it "outputs the plan for application/json " $ do
      r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; for=\"application/json\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; options=verbose; charset=utf-8")
        aggCol `shouldBe`
          if actualPgVersion >= pgVersion120
            then Just [aesonQQ| "COALESCE(json_agg(ROW(projects.id, projects.name, projects.client_id)), '[]'::json)" |]
            else Just [aesonQQ| "COALESCE(json_agg(ROW(pgrst_source.id, pgrst_source.name, pgrst_source.client_id)), '[]'::json)" |]

    it "outputs the plan for application/vnd.pgrst.object " $ do
      r <- request methodGet "/projects_view" (acceptHdrs "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object+json\"; options=verbose; charset=utf-8")
        aggCol `shouldBe`
          if actualPgVersion >= pgVersion120
            then Just [aesonQQ| "COALESCE((json_agg(ROW(projects.id, projects.name, projects.client_id)) -> 0), 'null'::json)" |]
            else Just [aesonQQ| "COALESCE((json_agg(ROW(pgrst_source.id, pgrst_source.name, pgrst_source.client_id)) -> 0), 'null'::json)" |]

  describe "writes plans" $ do
    it "outputs the total cost for an insert" $ do
      r <- request methodPost "/projects"
             (acceptHdrs "application/vnd.pgrst.plan+json") [json|{"id":100, "name": "Project 100"}|]

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 3.27

    it "outputs the total cost for an update" $ do
      r <- request methodPatch "/projects?id=eq.3"
             (acceptHdrs "application/vnd.pgrst.plan+json") [json|{"name": "Patched Project"}|]

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 12.45

    it "outputs the total cost for a delete" $ do
      r <- request methodDelete "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 15.68

    it "outputs the total cost for a single upsert" $ do
      r <- request methodPut "/tiobe_pls?name=eq.Go"
            (acceptHdrs "application/vnd.pgrst.plan+json")
            [json| [ { "name": "Go", "rank": 19 } ]|]

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 1.29

    it "outputs the plan for application/vnd.pgrst.object" $ do
      r <- request methodDelete "/projects?id=eq.6"
        [("Prefer", "return=representation"), ("Accept", "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object\"; options=verbose")] ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 3
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object+json\"; options=verbose; charset=utf-8")
        aggCol `shouldBe` Just [aesonQQ| "COALESCE((json_agg(ROW(projects.id, projects.name, projects.client_id)) -> 0), 'null'::json)" |]

  describe "function plan" $ do
    it "outputs the total cost for a function call" $ do
      r <- request methodGet "/rpc/getallprojects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 68.56

    it "outputs the plan for text/xml" $ do
      r <- request methodGet "/rpc/return_scalar_xml"
        (acceptHdrs "application/vnd.pgrst.plan+json; for=\"text/xml\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"text/xml\"; options=verbose; charset=utf-8")
        aggCol `shouldBe` Just [aesonQQ| "COALESCE(xmlagg(return_scalar_xml.pgrst_scalar), ''::xml)" |]

  describe "text format" $ do
    it "outputs the total cost for a function call" $ do
      r <- request methodGet "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan+text") ""

      let resBody    = simpleBody r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+text; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        resBody `shouldSatisfy` (\t -> LBS.take 9 t == "Aggregate")

    it "outputs in text format by default" $ do
      r <- request methodGet "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan") ""

      let resBody    = simpleBody r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+text; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        resBody `shouldSatisfy` (\t -> LBS.take 9 t == "Aggregate")

  describe "resource embedding costs" $ do
    it "a one to many doesn't surpass a threshold" $ do
      r <- request methodGet "/clients?select=*,projects(*)&id=eq.1"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 33.3)

    it "a many to one doesn't surpass a threshold" $ do
      r <- request methodGet "/projects?select=*,clients(*)&id=eq.1"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 16.5)

    it "a many to many doesn't surpass a threshold" $ do
      r <- request methodGet "/users?select=*,tasks(*)&id=eq.1"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      liftIO $ planCost r `shouldSatisfy` (< 70.9)

    context "!inner vs embed not null" $ do
      it "on an o2m, an !inner has a similar cost to not.null" $ do
        r1 <- request methodGet "/clients?select=*,projects!inner(*)&id=eq.1"
               [planHdr] ""

        liftIO $ planCost r1 `shouldSatisfy` (< 33.3)

        r2 <- request methodGet "/clients?select=*,projects(*)&projects=not.is.null&id=eq.1"
               [planHdr] ""

        liftIO $ planCost r2 `shouldSatisfy` (< 33.3)

      it "on an m2o, an !inner has a similar cost to not.null" $ do
        r1 <- request methodGet "/projects?select=*,clients!inner(*)&id=eq.1"
               [planHdr] ""

        liftIO $ planCost r1 `shouldSatisfy` (< 16.42)

        r2 <- request methodGet "/projects?select=*,clients(*)&clients=not.is.null&id=eq.1"
               [planHdr] ""

        liftIO $ planCost r2 `shouldSatisfy` (< 16.42)

      it "on an m2m, an !inner has a similar cost to not.null" $ do
        r1 <- request methodGet "/users?select=*,tasks!inner(*)&tasks.id=eq.1"
               [planHdr] ""

        liftIO $ planCost r1 `shouldSatisfy` (< 20876.14)

        r2 <- request methodGet "/users?select=*,tasks(*)&tasks.id=eq.1&tasks=not.is.null"
               [planHdr] ""

        liftIO $ planCost r2 `shouldSatisfy` (< 20876.14)

  describe "function call costs" $ do
    it "should not exceed cost when calling setof composite proc" $ do
      r <- request methodGet "/rpc/get_projects_below?id=3"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 45.4)

    it "should not exceed cost when calling setof composite proc with empty params" $ do
      r <- request methodGet "/rpc/getallprojects"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 71.0)

    it "should not exceed cost when calling scalar proc" $ do
      r <- request methodGet "/rpc/add_them?a=3&b=4"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 1.18)

    context "function inlining" $ do
      it "should inline a zero argument function(the function won't appear in the plan tree)" $ do
        r <- request methodGet "/rpc/getallusers?id=eq.1"
               [(hAccept, "application/vnd.pgrst.plan")] ""

        let resBody = simpleBody r

        liftIO $ do
          resBody `shouldSatisfy` (\t -> not $ T.isInfixOf "getallusers" (decodeUtf8 $ LBS.toStrict t))

      it "should inline a function with arguments(the function won't appear in the plan tree)" $ do
        r <- request methodGet "/rpc/getitemrange?min=10&max=15"
               [(hAccept, "application/vnd.pgrst.plan")] ""

        let resBody = simpleBody r

        liftIO $ do
          resBody `shouldSatisfy` (\t -> not $ T.isInfixOf "getitemrange" (decodeUtf8 $ LBS.toStrict t))

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
