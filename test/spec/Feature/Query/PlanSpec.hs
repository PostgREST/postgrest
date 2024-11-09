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

import PostgREST.Config.PgVersion (PgVersion, pgVersion130,
                                   pgVersion170)
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
        totalCost `shouldBe` (if actualPgVersion >= pgVersion170 then 11.32 else 15.63)

    it "outputs the total cost for a single filter on a view" $ do
      r <- request methodGet "/projects_view?id=gt.2"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 24.28

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
          Just [aesonQQ| "COALESCE(json_agg(ROW(projects.id, projects.name, projects.client_id)), '[]'::json)" |]

    it "outputs the plan for application/vnd.pgrst.object " $ do
      r <- request methodGet "/projects_view" (acceptHdrs "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/vnd.pgrst.object+json\"; options=verbose; charset=utf-8")
        aggCol `shouldBe`
          Just [aesonQQ| "COALESCE((json_agg(ROW(projects.id, projects.name, projects.client_id)) -> 0), 'null'::json)" |]

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
        totalCost `shouldBe` 0.06

    it "outputs the total cost for an update" $ do
      r <- request methodPatch "/projects?id=eq.3"
             (acceptHdrs "application/vnd.pgrst.plan+json") [json|{"name": "Patched Project"}|]

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 8.23

    it "outputs the total cost for a delete" $ do
      r <- request methodDelete "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` (if actualPgVersion >= pgVersion170 then 11.37 else 15.68)

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
        totalCost `shouldBe` 3.55

    it "outputs the total cost for 2 upserts" $ do
      r <- request methodPost "/tiobe_pls"
            [("Prefer","resolution=merge-duplicates"), ("Accept","application/vnd.pgrst.plan+json")]
            [json| [ { "name": "Python", "rank": 19 }, { "name": "Go", "rank": 20} ]|]

      let totalCost  = planCost r
          resStatus  = simpleStatus r
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 5.53

    it "outputs the total cost for an upsert with 10 rows" $ do
      r <- request methodPost "/tiobe_pls"
            [("Prefer","resolution=merge-duplicates"), ("Accept","application/vnd.pgrst.plan+json")]
            (getInsertDataForTiobePlsTable 10)

      let totalCost  = planCost r
          resStatus  = simpleStatus r
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 5.53

    it "outputs the total cost for an upsert with 100 rows" $ do
      r <- request methodPost "/tiobe_pls"
            [("Prefer","resolution=merge-duplicates"), ("Accept","application/vnd.pgrst.plan+json")]
            (getInsertDataForTiobePlsTable 100)

      let totalCost  = planCost r
          resStatus  = simpleStatus r
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 5.53

    it "outputs the total cost for an upsert with 1000 rows" $ do
      r <- request methodPost "/tiobe_pls"
            [("Prefer","resolution=merge-duplicates"), ("Accept","application/vnd.pgrst.plan+json")]
            (getInsertDataForTiobePlsTable 1000)

      let totalCost  = planCost r
          resStatus  = simpleStatus r
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 5.53

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

        liftIO $ planCost r1 `shouldSatisfy` (< 20888.83)

        r2 <- request methodGet "/users?select=*,tasks(*)&tasks.id=eq.1&tasks=not.is.null"
               [planHdr] ""

        liftIO $ planCost r2 `shouldSatisfy` (< 20888.83)

  describe "function call costs" $ do
    it "should not exceed cost when calling setof composite proc" $ do
      r <- request methodGet "/rpc/get_projects_below?id=3"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 35.4)

    it "should not exceed cost when calling setof composite proc with empty params" $ do
      r <- request methodGet "/rpc/getallprojects"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 71.0)

    it "should not exceed cost when calling scalar proc" $ do
      r <- request methodGet "/rpc/add_them?a=3&b=4"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 0.08)

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

    context "index usage" $ do
      it "should use an index for a json arrow operator filter" $ do
        r <- request methodGet "/bets?data_json->>contractId=eq.1"
               [(hAccept, "application/vnd.pgrst.plan")] ""

        let resBody = simpleBody r

        liftIO $ do
          resBody `shouldSatisfy` (\t -> T.isInfixOf "Index Cond" (decodeUtf8 $ LBS.toStrict t))

      it "should use an index for a jsonb arrow operator filter" $ do
        r <- request methodGet "/bets?data_jsonb->>contractId=eq.1"
               [(hAccept, "application/vnd.pgrst.plan")] ""

        let resBody = simpleBody r

        liftIO $ do
          resBody `shouldSatisfy` (\t -> T.isInfixOf "Index" (decodeUtf8 $ LBS.toStrict t))

      it "should use an index for ordering on a json arrow operator" $ do
        r <- request methodGet "/bets?order=data_json->>contractId"
               [(hAccept, "application/vnd.pgrst.plan")] ""

        let resBody = simpleBody r

        liftIO $ do
          resBody `shouldSatisfy` (\t -> T.isInfixOf "Index" (decodeUtf8 $ LBS.toStrict t))

      it "should use an index for ordering on a jsonb arrow operator" $ do
        r <- request methodGet "/bets?order=data_jsonb->>contractId"
               [(hAccept, "application/vnd.pgrst.plan")] ""

        let resBody = simpleBody r

        liftIO $ do
          resBody `shouldSatisfy` (\t -> T.isInfixOf "Index" (decodeUtf8 $ LBS.toStrict t))

  describe "custom media types" $ do
    it "outputs the plan for a scalar function text/xml" $ do
      r <- request methodGet "/rpc/return_scalar_xml"
        (acceptHdrs "application/vnd.pgrst.plan+json; for=\"text/xml\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"text/xml\"; options=verbose; charset=utf-8")
        aggCol `shouldBe` Just [aesonQQ| "return_scalar_xml.pgrst_scalar" |]

    it "outputs the plan for an aggregate application/vnd.twkb" $ do
      r <- request methodGet "/lines"
        (acceptHdrs "application/vnd.pgrst.plan+json; for=\"application/vnd.twkb\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/vnd.twkb\"; options=verbose; charset=utf-8")
        aggCol `shouldBe`
          Just [aesonQQ| "twkb_agg(ROW(lines.id, lines.name, lines.geom)::lines)" |]

disabledSpec :: SpecWith ((), Application)
disabledSpec =
  it "doesn't work if db-plan-enabled=false(the default)" $ do
    request methodGet "/projects?id=in.(1,2,3)"
         (acceptHdrs "application/vnd.pgrst.plan") ""
      `shouldRespondWith` 406

    request methodGet "/rpc/getallprojects?id=in.(1,2,3)"
      (acceptHdrs "application/vnd.pgrst.plan") ""
      `shouldRespondWith` 406

    request methodDelete "/projects?id=in.(1,2,3)"
           (acceptHdrs "application/vnd.pgrst.plan") ""
      `shouldRespondWith` 406
