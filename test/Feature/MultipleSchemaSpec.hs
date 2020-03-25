module Feature.MultipleSchemaSpec where

import Control.Lens    ((^?))
import Data.Aeson.Lens
import Data.Aeson.QQ

import Network.HTTP.Types
import Network.Wai        (Application)
import Network.Wai.Test   (simpleBody, SResponse (simpleHeaders))

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "multiple schemas in single instance" $ do
    it "succeeds in reading table from default schema v1 if no schema is selected via header" $
      request methodGet "/table" [] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value1"},
          {"id":2,"value":"value2"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v1"]
        }

    it "succeeds in reading table from default schema v1 after explicitly passing it in the header" $
      request methodGet "/table" [("Accept-Profile", "v1")] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value1"},
          {"id":2,"value":"value2"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v1"]
        }

    it "succeeds in reading table from schema v2" $
      request methodGet "/table" [("Accept-Profile", "v2")] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value3"},
          {"id":2,"value":"value4"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
        }

    it "succeeds in reading another_table from schema v2" $
      request methodGet "/another_table" [("Accept-Profile", "v2")] "" `shouldRespondWith`
        [json|[
          {"id":1,"another_value":"value5"},
          {"id":2,"another_value":"value6"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
        }

    it "doesn't find another_table in schema v1" $
      request methodGet "/another_table" [("Accept-Profile", "v1")] "" `shouldRespondWith` 404

    it "fails trying to read table from unkown schema" $
      request methodGet "/table" [("Accept-Profile", "unkown")] "" `shouldRespondWith`
        [json|{"message":"The schema must be one of the following: v1, v2"}|]
        {
          matchStatus = 406
        , matchHeaders = []
        }

    context "OpenAPI output" $ do
      it "succeeds in reading table definition from default schema v1 if no schema is selected via header" $ do
          r <- request methodGet "/" [] ""

          liftIO $ do
            simpleHeaders r `shouldSatisfy` matchHeader "Content-Profile" "v1"

            let def = simpleBody r ^? key "definitions" . key "table"

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
                      "value" : {
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

            let def = simpleBody r ^? key "definitions" . key "table"

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
                      "value" : {
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

            let def = simpleBody r ^? key "definitions" . key "table"

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
                      "value" : {
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

      it "fails trying to read definitions from unkown schema" $ do
        request methodGet "/" [("Accept-Profile", "unkown")] "" `shouldRespondWith`
          [json|{"message":"The schema must be one of the following: v1, v2"}|]
          {
            matchStatus = 406
          , matchHeaders = []
          }
