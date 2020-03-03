module Feature.MultipleSchemaSpec where

import Control.Lens       ((^?))
import Data.Aeson.Lens
import Data.Aeson.QQ

import Network.HTTP.Types
import Network.Wai        (Application)
import Network.Wai.Test   (simpleBody)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "multiple schemas in single instance" $ do
    it "read table from default schema(v1) if no schema is selected via header" $
      request methodGet "/table" [] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value1"},
          {"id":2,"value":"value2"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "read table from default schema(v1) after explicitly passing it in the header" $
      request methodGet "/table" [("Accept-Version", "v1")] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value1"},
          {"id":2,"value":"value2"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "read table from other schema(v2)" $
      request methodGet "/table" [("Accept-Version", "v2")] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value3"},
          {"id":2,"value":"value4"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "read another_table from other schema(v2)" $
      request methodGet "/another_table" [("Accept-Version", "v2")] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value5"},
          {"id":2,"value":"value6"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "read table from unkown schema" $
      request methodGet "/table" [("Accept-Version", "unkown")] "" `shouldRespondWith`
        [json|{"message":"The schema must be one of the following: v1, v2"}|]
        {
          matchStatus = 406
        , matchHeaders = []
        }

    it "read table definition from default schema (v1)" $ do
        r <- simpleBody <$> request methodGet "/" [] ""

        let def = r ^? key "definitions" . key "table"

        liftIO $

          def `shouldBe` Just
              [aesonQQ|
                {
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
                    "id",
                    "value"
                  ],
                  "type" : "object"
                }
              |]

    it "read table definition from default schema (v1) after explicitly passing it in the header" $ do
        r <- simpleBody <$> request methodGet "/" [("Accept-Version", "v1")] ""

        let def = r ^? key "definitions" . key "table"

        liftIO $

          def `shouldBe` Just
              [aesonQQ|
                {
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
                    "id",
                    "value"
                  ],
                  "type" : "object"
                }
              |]

    it "read table definition from other schema (v2)" $ do
        r <- simpleBody <$> request methodGet "/" [("Accept-Version", "v2")] ""

        let def = r ^? key "definitions" . key "table"

        liftIO $

          def `shouldBe` Just
              [aesonQQ|
                {
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
                    "id",
                    "value"
                  ],
                  "type" : "object"
                }
              |]

    it "read another_table definition from other schema (v2)" $ do
        r <- simpleBody <$> request methodGet "/" [("Accept-Version", "v2")] ""

        let def = r ^? key "definitions" . key "another_table"

        liftIO $

          def `shouldBe` Just
              [aesonQQ|
                {
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
                    "id",
                    "value"
                  ],
                  "type" : "object"
                }
              |]

    it "read table definition from unkown schema" $ do
        r <- simpleBody <$> request methodGet "/" [("Accept-Version", "unkown")] ""

        let def = r ^? key "definitions" . key "table"

        liftIO $

          def `shouldBe` Nothing
