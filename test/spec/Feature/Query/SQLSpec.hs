module Feature.Query.SQLSpec where

import Network.Wai.Test (SResponse (..))

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config (AppConfig (..))

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWithConfig
spec withConfig = withConfig (baseCfg { configDbPlanEnabled = True }) $ do
  describe "read table/view query" $ do
    it "outputs the query" $ do
      r <- request methodGet "/projects"
             (acceptHdrs "application/vnd.pgrst.sql") ""

      let resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.sql; for=\"application/json\"; charset=utf-8")
        resHeaders `shouldSatisfy` notZeroContentLength
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

  describe "write query" $ do
    it "outputs the query for an insert" $ do
      r <- request methodPost "/projects"
             (acceptHdrs "application/vnd.pgrst.sql") [json|{"id":100, "name": "Project 100"}|]

      let resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.sql; for=\"application/json\"; charset=utf-8")
        resHeaders `shouldSatisfy` notZeroContentLength
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

    it "outputs the query for an update" $ do
      r <- request methodPatch "/projects?id=eq.3"
             (acceptHdrs "application/vnd.pgrst.sql") [json|{"name": "Patched Project"}|]

      let resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.sql; for=\"application/json\"; charset=utf-8")
        resHeaders `shouldSatisfy` notZeroContentLength
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

    it "outputs the query for a delete" $ do
      r <- request methodDelete "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.sql") ""

      let resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.sql; for=\"application/json\"; charset=utf-8")
        resHeaders `shouldSatisfy` notZeroContentLength
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

    it "outputs the query for a single upsert" $ do
      r <- request methodPut "/tiobe_pls?name=eq.Go"
            (acceptHdrs "application/vnd.pgrst.sql")
            [json| [ { "name": "Go", "rank": 19 } ]|]

      let resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.sql; for=\"application/json\"; charset=utf-8")
        resHeaders `shouldSatisfy` notZeroContentLength
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

    it "outputs the query for 2 upserts" $ do
      r <- request methodPost "/tiobe_pls"
            [("Prefer","resolution=merge-duplicates"), ("Accept","application/vnd.pgrst.sql")]
            [json| [ { "name": "Python", "rank": 19 }, { "name": "Go", "rank": 20} ]|]

      let resStatus  = simpleStatus r
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.sql; for=\"application/json\"; charset=utf-8")
        resHeaders `shouldSatisfy` notZeroContentLength
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

  describe "function query" $ do
    it "outputs the query for a function call" $ do
      r <- request methodGet "/rpc/getallprojects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.sql") ""

      let resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.sql; for=\"application/json\"; charset=utf-8")
        resHeaders `shouldSatisfy` notZeroContentLength
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

  describe "custom media types" $ do
    it "outputs the query for a scalar function text/xml" $ do
      r <- request methodGet "/rpc/return_scalar_xml"
        (acceptHdrs "application/vnd.pgrst.sql; for=\"text/xml\"") ""

      let resHeaders = simpleHeaders r

      liftIO $
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.sql; for=\"text/xml\"; charset=utf-8")

    it "outputs the query for an aggregate application/vnd.twkb" $ do
      r <- request methodGet "/lines"
        (acceptHdrs "application/vnd.pgrst.sql; for=\"application/vnd.twkb\"") ""

      let resHeaders = simpleHeaders r

      liftIO $
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.sql; for=\"application/vnd.twkb\"; charset=utf-8")

disabledSpec :: SpecWithConfig
disabledSpec withConfig = withConfig baseCfg $
  it "doesn't work if db-plan-enabled=false(the default)" $ do
    request methodGet "/projects?id=in.(1,2,3)"
         (acceptHdrs "application/vnd.pgrst.sql") ""
      `shouldRespondWith` 406

    request methodGet "/rpc/getallprojects?id=in.(1,2,3)"
      (acceptHdrs "application/vnd.pgrst.sql") ""
      `shouldRespondWith` 406

    request methodDelete "/projects?id=in.(1,2,3)"
           (acceptHdrs "application/vnd.pgrst.sql") ""
      `shouldRespondWith` 406
