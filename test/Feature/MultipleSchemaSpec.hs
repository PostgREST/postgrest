module Feature.MultipleSchemaSpec where

import Control.Lens    ((^?))
import Data.Aeson.Lens
import Data.Aeson.QQ

import Network.HTTP.Types
import Network.Wai        (Application)
import Network.Wai.Test   (SResponse (simpleHeaders), simpleBody)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "multiple schemas in single instance" $ do
    context "Reading tables on different schemas" $ do
      it "succeeds in reading table from default schema v1 if no schema is selected via header" $
        request methodGet "/parents" [] "" `shouldRespondWith`
          [json|[
            {"id":1,"name":"parent v1-1"},
            {"id":2,"name":"parent v1-2"}
          ]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v1"]
          }

      it "succeeds in reading table from default schema v1 after explicitly passing it in the header" $
        request methodGet "/parents" [("Accept-Profile", "v1")] "" `shouldRespondWith`
          [json|[
            {"id":1,"name":"parent v1-1"},
            {"id":2,"name":"parent v1-2"}
          ]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v1"]
          }

      it "succeeds in reading table from schema v2" $
        request methodGet "/parents" [("Accept-Profile", "v2")] "" `shouldRespondWith`
          [json|[
            {"id":3,"name":"parent v2-3"},
            {"id":4,"name":"parent v2-4"}
          ]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
          }

      it "succeeds in reading another_table from schema v2" $
        request methodGet "/another_table" [("Accept-Profile", "v2")] "" `shouldRespondWith`
          [json|[
            {"id":5,"another_value":"value 5"},
            {"id":6,"another_value":"value 6"}
          ]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
          }

      it "doesn't find another_table in schema v1" $
        request methodGet "/another_table" [("Accept-Profile", "v1")] "" `shouldRespondWith` 404

      it "fails trying to read table from unkown schema" $
        request methodGet "/parents" [("Accept-Profile", "unkown")] "" `shouldRespondWith`
          [json|{"message":"The schema must be one of the following: v1, v2"}|]
          {
            matchStatus = 406
          }

    context "Inserting tables on different schemas" $ do
      it "succeeds inserting on default schema and returning it" $
        request methodPost "/children"
            [("Prefer", "return=representation")]
            [json|{"id": 0, "name": "child v1-1", "parent_id": 1}|]
          `shouldRespondWith`
            [json|[{"id": 0, "name": "child v1-1", "parent_id": 1}]|]
            {
              matchStatus = 201
            , matchHeaders = ["Content-Profile" <:> "v1"]
            }

      it "succeeds inserting on the v1 schema and returning its parent" $
        request methodPost "/children?select=id,parent(*)"
            [("Prefer", "return=representation"), ("Content-Profile", "v1")]
            [json|{"id": 0, "name": "child v1-2", "parent_id": 2}|]
          `shouldRespondWith`
            [json|[{"id": 0, "parent": {"id": 2, "name": "parent v1-2"}}]|]
            {
              matchStatus = 201
            , matchHeaders = ["Content-Profile" <:> "v1"]
            }

      it "succeeds inserting on the v2 schema and returning its parent" $
        request methodPost "/children?select=id,parent(*)"
            [("Prefer", "return=representation"), ("Content-Profile", "v2")]
            [json|{"id": 0, "name": "child v2-3", "parent_id": 3}|]
          `shouldRespondWith`
            [json|[{"id": 0, "parent": {"id": 3, "name": "parent v2-3"}}]|]
            {
              matchStatus = 201
            , matchHeaders = ["Content-Profile" <:> "v2"]
            }

      it "fails when inserting on an unknown schema" $
        request methodPost "/children" [("Content-Profile", "unknown")]
          [json|{"name": "child 4", "parent_id": 4}|]
          `shouldRespondWith`
          [json|{"message":"The schema must be one of the following: v1, v2"}|]
          {
            matchStatus = 406
          }

    context "calling procs on different schemas" $ do
      it "succeeds in calling the default schema proc" $
        request methodGet "/rpc/get_parents_below?id=6" [] ""
          `shouldRespondWith`
          [json|[{"id":1,"name":"parent v1-1"}, {"id":2,"name":"parent v1-2"}]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v1"]
          }

      it "succeeds in calling the v1 schema proc and embedding" $
        request methodGet "/rpc/get_parents_below?id=6&select=id,name,children(id,name)" [("Accept-Profile", "v1")] ""
          `shouldRespondWith`
          [json| [
            {"id":1,"name":"parent v1-1","children":[{"id":1,"name":"child v1-1"}]},
            {"id":2,"name":"parent v1-2","children":[{"id":2,"name":"child v1-2"}]}] |]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v1"]
          }

      it "succeeds in calling the v2 schema proc and embedding" $
        request methodGet "/rpc/get_parents_below?id=6&select=id,name,children(id,name)" [("Accept-Profile", "v2")] ""
          `shouldRespondWith`
          [json| [
            {"id":3,"name":"parent v2-3","children":[{"id":1,"name":"child v2-3"}]},
            {"id":4,"name":"parent v2-4","children":[]}] |]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
          }

      it "succeeds in calling the v2 schema proc with POST by using Content-Profile" $
        request methodPost "/rpc/get_parents_below?select=id,name" [("Content-Profile", "v2")]
          [json|{"id": "6"}|]
          `shouldRespondWith`
          [json| [
            {"id":3,"name":"parent v2-3"},
            {"id":4,"name":"parent v2-4"}]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
          }

    context "Modifying tables on different schemas" $ do
      it "succeeds in patching on the v1 schema and returning its parent" $
        request methodPatch "/children?select=name,parent(name)&id=eq.1" [("Content-Profile", "v1"), ("Prefer", "return=representation")]
          [json|{"name": "child v1-1 updated"}|]
          `shouldRespondWith`
          [json|[{"name":"child v1-1 updated", "parent": {"name": "parent v1-1"}}]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v1"]
          }

      it "succeeds in patching on the v2 schema and returning its parent" $
        request methodPatch "/children?select=name,parent(name)&id=eq.1" [("Content-Profile", "v2"), ("Prefer", "return=representation")]
          [json|{"name": "child v2-1 updated"}|]
          `shouldRespondWith`
          [json|[{"name":"child v2-1 updated", "parent": {"name": "parent v2-3"}}]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
          }

      it "succeeds on deleting on the v2 schema" $ do
        request methodDelete "/children?id=eq.1"
            [("Content-Profile", "v2"), ("Prefer", "return=representation")]
            ""
          `shouldRespondWith`
            [json|[{"id": 1, "name": "child v2-3", "parent_id": 3}]|]
            { matchHeaders = ["Content-Profile" <:> "v2"] }

      it "succeeds on PUT on the v2 schema" $
        request methodPut "/children?id=eq.111" [("Content-Profile", "v2"), ("Prefer", "return=representation")]
          [json| [ { "id": 111, "name": "child v2-111", "parent_id": null } ]|]
          `shouldRespondWith`
          [json|[{ "id": 111, "name": "child v2-111", "parent_id": null }]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
          }

    context "OpenAPI output" $ do
      it "succeeds in reading table definition from default schema v1 if no schema is selected via header" $ do
          req <- request methodGet "/" [] ""

          liftIO $ do
            simpleHeaders req `shouldSatisfy` matchHeader "Content-Profile" "v1"

            let def = simpleBody req ^? key "definitions" . key "parents"

            def `shouldBe` Just
                [aesonQQ|
                  {
                    "type" : "object",
                    "properties" : {
                      "id" : {
                        "description" : "Note:\nThis is a Primary Key.<pk/>",
                        "format" : "integer",
                        "type" : "integer"
                      },
                      "name" : {
                        "format" : "text",
                        "type" : "string"
                      }
                    },
                    "required" : [
                      "id"
                    ]
                  }
                |]

      it "succeeds in reading table definition from default schema v1 after explicitly passing it in the header" $ do
          r <- request methodGet "/" [("Accept-Profile", "v1")] ""

          liftIO $ do
            simpleHeaders r `shouldSatisfy` matchHeader "Content-Profile" "v1"

            let def = simpleBody r ^? key "definitions" . key "parents"

            def `shouldBe` Just
                [aesonQQ|
                  {
                    "type" : "object",
                    "properties" : {
                      "id" : {
                        "description" : "Note:\nThis is a Primary Key.<pk/>",
                        "format" : "integer",
                        "type" : "integer"
                      },
                      "name" : {
                        "format" : "text",
                        "type" : "string"
                      }
                    },
                    "required" : [
                      "id"
                    ]
                  }
                |]

      it "succeeds in reading table definition from schema v2" $ do
          r <- request methodGet "/" [("Accept-Profile", "v2")] ""

          liftIO $ do
            simpleHeaders r `shouldSatisfy` matchHeader "Content-Profile" "v2"

            let def = simpleBody r ^? key "definitions" . key "parents"

            def `shouldBe` Just
                [aesonQQ|
                  {
                    "type" : "object",
                    "properties" : {
                      "id" : {
                        "description" : "Note:\nThis is a Primary Key.<pk/>",
                        "format" : "integer",
                        "type" : "integer"
                      },
                      "name" : {
                        "format" : "text",
                        "type" : "string"
                      }
                    },
                    "required" : [
                      "id"
                    ]
                  }
                |]

      it "succeeds in reading another_table definition from schema v2" $ do
          r <- request methodGet "/" [("Accept-Profile", "v2")] ""

          liftIO $ do
            simpleHeaders r `shouldSatisfy` matchHeader "Content-Profile" "v2"

            let def = simpleBody r ^? key "definitions" . key "another_table"

            def `shouldBe` Just
                [aesonQQ|
                  {
                    "type" : "object",
                    "properties" : {
                      "id" : {
                        "description" : "Note:\nThis is a Primary Key.<pk/>",
                        "format" : "integer",
                        "type" : "integer"
                      },
                      "another_value" : {
                        "format" : "text",
                        "type" : "string"
                      }
                    },
                    "required" : [
                      "id"
                    ]
                  }
                |]

      it "doesn't find another_table definition in schema v1" $ do
        r <- request methodGet "/" [("Accept-Profile", "v1")] ""

        liftIO $ do
          let def = simpleBody r ^? key "definitions" . key "another_table"
          def `shouldBe` Nothing

      it "fails trying to read definitions from unkown schema" $
        request methodGet "/" [("Accept-Profile", "unkown")] "" `shouldRespondWith`
          [json|{"message":"The schema must be one of the following: v1, v2"}|]
          {
            matchStatus = 406
          }
