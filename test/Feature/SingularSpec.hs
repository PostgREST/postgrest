module Feature.SingularSpec where

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Text.Heredoc

import Protolude  hiding (get)
import SpecHelper


spec :: SpecWith ((), Application)
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
      it "works for one row with return=rep" $ do
        _ <- post "/addresses" [json| { id: 97, address: "A Street" } |]
        request methodPatch
          "/addresses?id=eq.97"
          [("Prefer", "return=representation"), singular]
          [json| { address: "B Street" } |]
          `shouldRespondWith`
            [str|{"id":97,"address":"B Street"}|]

      it "works for one row with return=minimal" $
        request methodPatch
          "/addresses?id=eq.97"
          [("Prefer", "return=minimal"), singular]
          [json| { address: "C Street" } |]
          `shouldRespondWith`
            ""
            { matchStatus  = 204 }

      it "raises an error for multiple rows" $ do
        _ <- post "/addresses" [json| { id: 98, address: "xxx" } |]
        _ <- post "/addresses" [json| { id: 99, address: "yyy" } |]
        p <- request methodPatch "/addresses?id=gt.0"
                [singular]
                [json| { address: "zzz" } |]
        liftIO $ do
          simpleStatus p `shouldBe` notAcceptable406
          isErrorFormat (simpleBody p) `shouldBe` True

        -- the rows should not be updated, either
        get "/addresses?id=eq.98" `shouldRespondWith` [str|[{"id":98,"address":"xxx"}]|]

      it "raises an error for multiple rows with return=rep" $ do
        _ <- post "/addresses" [json| { id: 100, address: "xxx" } |]
        _ <- post "/addresses" [json| { id: 101, address: "yyy" } |]
        p <- request methodPatch "/addresses?id=gt.0"
                [("Prefer", "return=representation"), singular]
                [json| { address: "zzz" } |]
        liftIO $ do
          simpleStatus p `shouldBe` notAcceptable406
          isErrorFormat (simpleBody p) `shouldBe` True

        -- the rows should not be updated, either
        get "/addresses?id=eq.100" `shouldRespondWith` [str|[{"id":100,"address":"xxx"}]|]

      it "raises an error for zero rows" $
        request methodPatch "/items?id=gt.0&id=lt.0"
                [singular] [json|{"id":1}|]
          `shouldRespondWith`
                  [str|{"details":"Results contain 0 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                  { matchStatus  = 406
                  , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                  }

      it "raises an error for zero rows with return=rep" $
        request methodPatch "/items?id=gt.0&id=lt.0"
                [("Prefer", "return=representation"), singular] [json|{"id":1}|]
          `shouldRespondWith`
                  [str|{"details":"Results contain 0 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                  { matchStatus  = 406
                  , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                  }

    context "when creating rows" $ do
      it "works for one row with return=rep" $ do
        p <- request methodPost
          "/addresses"
          [("Prefer", "return=representation"), singular]
          [json| [ { id: 102, address: "xxx" } ] |]
        liftIO $ simpleBody p `shouldBe` [str|{"id":102,"address":"xxx"}|]

      it "works for one row with return=minimal" $ do
        request methodPost "/addresses"
          [("Prefer", "return=minimal"), singular]
          [json| [ { id: 103, address: "xxx" } ] |]
          `shouldRespondWith` ""
          { matchStatus  = 201
          , matchHeaders = ["Content-Range" <:> "*/*"]
          }
        -- and the element should exist
        get "/addresses?id=eq.103"
          `shouldRespondWith` [str|[{"id":103,"address":"xxx"}]|]
          { matchStatus  = 200
          , matchHeaders = []
          }

      it "raises an error when attempting to create multiple entities" $ do
        p <- request methodPost
          "/addresses"
          [singular]
          [json| [ { id: 200, address: "xxx" }, { id: 201, address: "yyy" } ] |]
        liftIO $ simpleStatus p `shouldBe` notAcceptable406

        -- the rows should not exist, either
        get "/addresses?id=eq.200" `shouldRespondWith` "[]"

      it "raises an error when attempting to create multiple entities with return=rep" $ do
        p <- request methodPost
          "/addresses"
          [("Prefer", "return=representation"), singular]
          [json| [ { id: 202, address: "xxx" }, { id: 203, address: "yyy" } ] |]
        liftIO $ simpleStatus p `shouldBe` notAcceptable406

        -- the rows should not exist, either
        get "/addresses?id=eq.202" `shouldRespondWith` "[]"

      it "raises an error regardless of return=minimal" $ do
        request methodPost "/addresses"
                [("Prefer", "return=minimal"), singular]
                [json| [ { id: 204, address: "xxx" }, { id: 205, address: "yyy" } ] |]
          `shouldRespondWith`
                  [str|{"details":"Results contain 2 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                  { matchStatus  = 406
                  , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                  }

        -- the rows should not exist, either
        get "/addresses?id=eq.204" `shouldRespondWith` "[]"

      it "raises an error when creating zero entities" $
        request methodPost "/addresses"
                [singular]
                [json| [ ] |]
          `shouldRespondWith`
                  [str|{"details":"Results contain 0 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                  { matchStatus  = 406
                  , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                  }

      it "raises an error when creating zero entities with return=rep" $
        request methodPost "/addresses"
                [("Prefer", "return=representation"), singular]
                [json| [ ] |]
          `shouldRespondWith`
                  [str|{"details":"Results contain 0 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                  { matchStatus  = 406
                  , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                  }

    context "when deleting rows" $ do
      it "works for one row with return=rep" $ do
        p <- request methodDelete
          "/items?id=eq.11"
          [("Prefer", "return=representation"), singular] ""
        liftIO $ simpleBody p `shouldBe` [str|{"id":11}|]

      it "works for one row with return=minimal" $ do
        p <- request methodDelete
          "/items?id=eq.12"
          [("Prefer", "return=minimal"), singular] ""
        liftIO $ simpleBody p `shouldBe` ""

      it "raises an error when attempting to delete multiple entities" $ do
        let firstItems = "/items?id=gt.0&id=lt.6"
        request methodDelete firstItems
          [singular] ""
          `shouldRespondWith` 406

        get firstItems
          `shouldRespondWith` [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5}] |]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "0-4/*"]
          }

      it "raises an error when attempting to delete multiple entities with return=rep" $ do
        let firstItems = "/items?id=gt.5&id=lt.11"
        request methodDelete firstItems
          [("Prefer", "return=representation"), singular] ""
          `shouldRespondWith` 406

        get firstItems
          `shouldRespondWith` [json| [{"id":6},{"id":7},{"id":8},{"id":9},{"id":10}] |]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "0-4/*"]
          }

      it "raises an error when deleting zero entities" $
        request methodDelete "/items?id=lt.0"
                [singular] ""
          `shouldRespondWith`
                [str|{"details":"Results contain 0 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                { matchStatus  = 406
                , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                }

      it "raises an error when deleting zero entities with return=rep" $
        request methodDelete "/items?id=lt.0"
                [("Prefer", "return=representation"), singular] ""
          `shouldRespondWith`
                [str|{"details":"Results contain 0 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                { matchStatus  = 406
                , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                }

    context "when calling a stored proc" $ do
      it "fails for zero rows" $
        request methodPost "/rpc/getproject"
                [singular] [json|{ "id": 9999999}|]
          `shouldRespondWith`
                [str|{"details":"Results contain 0 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                { matchStatus  = 406
                , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                }

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

      it "fails for multiple rows" $
        request methodPost "/rpc/getallprojects"
                [singular] "{}"
          `shouldRespondWith`
                [str|{"details":"Results contain 5 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                { matchStatus  = 406
                , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                }

      it "executes the proc exactly once per request" $ do
        request methodPost "/rpc/getproject?select=id,name" [] [json| {"id": 1} |]
          `shouldRespondWith` [str|[{"id":1,"name":"Windows 7"}]|]

        request methodPost "/rpc/setprojects" [singular]
                [json| {"id_l": 1, "id_h": 2, "name": "changed"} |]
          `shouldRespondWith`
                [str|{"details":"Results contain 2 rows, application/vnd.pgrst.object+json requires 1 row","message":"JSON object requested, multiple (or no) rows returned"}|]
                { matchStatus  = 406
                , matchHeaders = ["Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"]
                }

        -- should not actually have executed the function
        request methodPost "/rpc/getproject?select=id,name" [] [json| {"id": 1} |]
          `shouldRespondWith` [str|[{"id":1,"name":"Windows 7"}]|]
