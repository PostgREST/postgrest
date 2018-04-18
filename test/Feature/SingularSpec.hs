module Feature.SingularSpec where

import Text.Heredoc
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(..))

import Network.Wai (Application)

import SpecHelper
import Protolude hiding (get)


spec :: SpecWith Application
spec =
  describe "Requesting singular json object" $ do
    let pgrstObj = "application/vnd.pgrst.object+json"
        singular = ("Accept", pgrstObj)

    context "with GET request" $ do
      it "fails for zero rows" $
        request methodGet  "/items?id=gt.0&id=lt.0" [singular] ""
          `shouldRespondWith` 406

      it "will select an existing object" $ do
        request methodGet "/items?id=eq.5" [singular] ""
          `shouldRespondWith` [str|{"id":5}|]
        -- also test without the +json suffix
        request methodGet "/items?id=eq.5"
          [("Accept", "application/vnd.pgrst.object")] ""
          `shouldRespondWith` [str|{"id":5}|]

      it "can combine multiple prefer values" $
        request methodGet "/items?id=eq.5" [singular, ("Prefer","count=none")] ""
          `shouldRespondWith` [str|{"id":5}|]

      it "can shape plurality singular object routes" $
        request methodGet "/projects_view?id=eq.1&select=id,name,clients(*),tasks(id,name)" [singular] ""
          `shouldRespondWith`
            [json|{"id":1,"name":"Windows 7","clients":{"id":1,"name":"Microsoft"},"tasks":[{"id":1,"name":"Design w7"},{"id":2,"name":"Code w7"}]}|]
            { matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"] }

    context "when updating rows" $ do

      it "works for one row" $ do
        _ <- post "/addresses" [json| { id: 97, address: "A Street" } |]
        request methodPatch
          "/addresses?id=eq.97"
          [("Prefer", "return=representation"), singular]
          [json| { address: "B Street" } |]
          `shouldRespondWith`
            [str|{"id":97,"address":"B Street"}|]

      it "raises an error for multiple rows" $ do
        _ <- post "/addresses" [json| { id: 98, address: "xxx" } |]
        _ <- post "/addresses" [json| { id: 99, address: "yyy" } |]
        p <- request methodPatch
          "/addresses?id=gt.0"
          [("Prefer", "return=representation"), singular]
          [json| { address: "zzz" } |]
        liftIO $ do
          simpleStatus p `shouldBe` notAcceptable406
          isErrorFormat (simpleBody p) `shouldBe` True

        -- the rows should not be updated, either
        get "/addresses?id=eq.98" `shouldRespondWith` [str|[{"id":98,"address":"xxx"}]|]

      it "raises an error for zero rows" $ do
        p <- request methodPatch  "/items?id=gt.0&id=lt.0"
          [("Prefer", "return=representation"), singular] [json|{"id":1}|]
        liftIO $ do
          simpleStatus p `shouldBe` notAcceptable406
          isErrorFormat (simpleBody p) `shouldBe` True

    context "when creating rows" $ do

      it "works for one row" $ do
        p <- request methodPost
          "/addresses"
          [("Prefer", "return=representation"), singular]
          [json| [ { id: 100, address: "xxx" } ] |]
        liftIO $ simpleBody p `shouldBe` [str|{"id":100,"address":"xxx"}|]

      it "works for one row even with return=minimal" $ do
        request methodPost "/addresses"
          [("Prefer", "return=minimal"), singular]
          [json| [ { id: 101, address: "xxx" } ] |]
          `shouldRespondWith` ""
          { matchStatus  = 201
          , matchHeaders = ["Content-Range" <:> "*/*"]
          }
        -- and the element should exist
        get "/addresses?id=eq.101"
          `shouldRespondWith` [str|[{"id":101,"address":"xxx"}]|]
          { matchStatus  = 200
          , matchHeaders = []
          }

      it "raises an error when attempting to create multiple entities" $ do
        p <- request methodPost
          "/addresses"
          [("Prefer", "return=representation"), singular]
          [json| [ { id: 200, address: "xxx" }, { id: 201, address: "yyy" } ] |]
        liftIO $ simpleStatus p `shouldBe` notAcceptable406

        -- the rows should not exist, either
        get "/addresses?id=eq.200" `shouldRespondWith` "[]"

      it "return=minimal allows request to create multiple elements" $
        request methodPost "/addresses"
          [("Prefer", "return=minimal"), singular]
          [json| [ { id: 200, address: "xxx" }, { id: 201, address: "yyy" } ] |]
          `shouldRespondWith` ""
          { matchStatus  = 201
          , matchHeaders = ["Content-Range" <:> "*/*"]
          }

      it "raises an error when creating zero entities" $ do
        p <- request methodPost
          "/addresses"
          [("Prefer", "return=representation"), singular]
          [json| [ ] |]
        liftIO $ do
          simpleStatus p `shouldBe` notAcceptable406
          isErrorFormat (simpleBody p) `shouldBe` True

    context "when deleting rows" $ do

      it "works for one row" $ do
        p <- request methodDelete
          "/items?id=eq.11"
          [("Prefer", "return=representation"), singular] ""
        liftIO $ simpleBody p `shouldBe` [str|{"id":11}|]

      it "raises an error when attempting to delete multiple entities" $ do
        let firstItems = "/items?id=gt.0&id=lt.11"
        request methodDelete firstItems
          [("Prefer", "return=representation"), singular] ""
          `shouldRespondWith` 406

        get firstItems
          `shouldRespondWith` [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10}] |]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "0-9/*"]
          }

      it "raises an error when deleting zero entities" $ do
        p <- request methodDelete "/items?id=lt.0"
          [("Prefer", "return=representation"), singular] ""
        liftIO $ do
          simpleStatus p `shouldBe` notAcceptable406
          isErrorFormat (simpleBody p) `shouldBe` True

    context "when calling a stored proc" $ do

      it "fails for zero rows" $ do
        p <- request methodPost "/rpc/getproject"
          [singular] [json|{ "id": 9999999}|]
        liftIO $ do
          simpleStatus p `shouldBe` notAcceptable406
          isErrorFormat (simpleBody p) `shouldBe` True

      -- this one may be controversial, should vnd.pgrst.object include
      -- the likes of 2 and "hello?"
      it "succeeds for scalar result" $
        request methodPost "/rpc/sayhello"
          [singular] [json|{ "name": "world"}|]
          `shouldRespondWith` 200

      it "returns a single object for json proc" $
        request methodPost "/rpc/getproject"
          [singular] [json|{ "id": 1}|] `shouldRespondWith`
          [str|{"id":1,"name":"Windows 7","client_id":1}|]

      it "fails for multiple rows" $ do
        p <- request methodPost "/rpc/getallprojects" [singular] "{}"
        liftIO $ do
          simpleStatus p `shouldBe` notAcceptable406
          isErrorFormat (simpleBody p) `shouldBe` True

      it "executes the proc exactly once per request" $ do
        request methodPost "/rpc/getproject?select=id,name" [] [json| {"id": 1} |]
          `shouldRespondWith` [str|[{"id":1,"name":"Windows 7"}]|]
        p <- request methodPost "/rpc/setprojects" [singular]
          [json| {"id_l": 1, "id_h": 2, "name": "changed"} |]
        liftIO $ do
          simpleStatus p `shouldBe` notAcceptable406
          isErrorFormat (simpleBody p) `shouldBe` True

        -- should not actually have executed the function
        request methodPost "/rpc/getproject?select=id,name" [] [json| {"id": 1} |]
          `shouldRespondWith` [str|[{"id":1,"name":"Windows 7"}]|]
