module Feature.Query.ExplainPlanSpec where

import Control.Lens     ((^?))
import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import Data.Aeson.Lens
import Data.Aeson.QQ
import Network.HTTP.Types
import Test.Hspec         hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config.PgVersion (PgVersion, pgVersion120,
                                   pgVersion130)
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

      liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")

      liftIO $ resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

      liftIO $ totalCost `shouldBe`
        if actualPgVersion > pgVersion120
          then Just [aesonQQ|15.63|]
          else Just [aesonQQ|15.69|]

    it "outputs the total cost for a single filter on a view" $ do
      r <- request methodGet "/projects_view?id=gt.2"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")

      liftIO $ resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

      liftIO $ totalCost `shouldBe`
        if actualPgVersion > pgVersion120
          then Just [aesonQQ|24.28|]
          else Just [aesonQQ|32.28|]

    it "outputs blocks info when using the buffers option" $
      if actualPgVersion >= pgVersion130
        then do
          r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=buffers") ""

          let blocks  = simpleBody r ^? nth 0 . key "Planning"
              resHeaders = simpleHeaders r

          liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=buffers; charset=utf-8")

          liftIO $ blocks `shouldBe`
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

          liftIO $ blocks `shouldBe` Just [aesonQQ| 1.0 |]

          liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=analyze|buffers; charset=utf-8")

    when (actualPgVersion >= pgVersion120) $
      it "outputs the search path when using the settings option" $ do
        r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=settings") ""

        let searchPath  = simpleBody r ^? nth 0 . key "Settings"
            resHeaders = simpleHeaders r

        liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=settings; charset=utf-8")

        liftIO $ searchPath `shouldBe`
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

        liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=analyze|wal; charset=utf-8")

        liftIO $ walRecords `shouldBe`
          Just [aesonQQ|0|]

    it "outputs columns info when using the verbose option" $ do
      r <- request methodGet "/projects" (acceptHdrs "application/vnd.pgrst.plan+json; options=verbose") ""

      let cols  = simpleBody r ^? nth 0 . key "Plan" . key "Plans" . nth 0 . key "Output"
          resHeaders = simpleHeaders r

      liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; options=verbose; charset=utf-8")

      liftIO $ cols `shouldBe`
        Just [aesonQQ| ["projects.id", "projects.name", "projects.client_id"] |]

  describe "writes plans" $ do
    it "outputs the total cost for an insert" $ do
      r <- request methodPost "/projects"
             (acceptHdrs "application/vnd.pgrst.plan") [json|{"id":100, "name": "Project 100"}|]

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")

      liftIO $ resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

      liftIO $ totalCost `shouldBe`
        if actualPgVersion > pgVersion120
          then Just [aesonQQ|3.28|]
          else Just [aesonQQ|3.33|]

    it "outputs the total cost for an update" $ do
      r <- request methodGet "/projects?id=eq.3"
             (acceptHdrs "application/vnd.pgrst.plan") [json|{"name": "Patched Project"}|]

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")

      liftIO $ resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

      liftIO $ totalCost `shouldBe`
        if actualPgVersion > pgVersion120
          then Just [aesonQQ|8.2|]
          else Just [aesonQQ|8.22|]

    it "outputs the total cost for a delete" $ do
      r <- request methodDelete "/projects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan") ""

      let totalCost  = simpleBody r ^? nth 0 . key "Plan" . key "Total Cost"
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; charset=utf-8")

      liftIO $ resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }

      liftIO $ totalCost `shouldBe` Just [aesonQQ|15.68|]
