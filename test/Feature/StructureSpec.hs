module Feature.StructureSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Network.HTTP.Types

import Control.Lens ((^?))
import Data.Aeson.Lens
import Data.Aeson.QQ

import SpecHelper

import Network.Wai (Application)
import Network.Wai.Test (SResponse(..))

import Protolude hiding (get)

spec :: SpecWith Application
spec = do

  describe "OpenAPI" $ do
    it "root path returns a valid openapi spec" $
      validateOpenApiResponse [("Accept", "application/openapi+json")]

    it "should respond to openapi request on none root path with 415" $
      request methodGet "/items"
              (acceptHdrs "application/openapi+json") ""
        `shouldRespondWith` 415

    describe "table" $

      it "includes paths to tables" $ do
        r <- simpleBody <$> get "/"

        let method s = key "paths" . key "/child_entities" . key s
            getParameters = r ^? method "get" . key "parameters"
            postResponse = r ^? method "post" . key "responses" . key "201" . key "description"
            patchResponse = r ^? method "patch" . key "responses" . key "204" . key "description"
            deleteResponse = r ^? method "delete" . key "responses" . key "204" . key "description"

        liftIO $ do

          getParameters `shouldBe` Just
            [aesonQQ|
              [
                { "$ref": "#/parameters/rowFilter.child_entities.id" },
                { "$ref": "#/parameters/rowFilter.child_entities.name" },
                { "$ref": "#/parameters/rowFilter.child_entities.parent_id" },
                { "$ref": "#/parameters/select" },
                { "$ref": "#/parameters/order" },
                { "$ref": "#/parameters/range" },
                { "$ref": "#/parameters/rangeUnit" },
                { "$ref": "#/parameters/offset" },
                { "$ref": "#/parameters/limit" },
                { "$ref": "#/parameters/preferCount" }
              ]
            |]

          postResponse `shouldBe` Just "Created"

          patchResponse `shouldBe` Just "No Content"

          deleteResponse `shouldBe` Just "No Content"

    it "includes definitions to tables" $ do
        r <- simpleBody <$> get "/"

        let def = r ^? key "definitions" . key "child_entities"

        liftIO $ do

          def `shouldBe` Just
            [aesonQQ|
              {
                "type": "object",
                "description": "child_entities comment",
                "properties": {
                  "id": {
                    "description": "child_entities id comment\n\nNote:\nThis is a Primary Key.<pk/>",
                    "format": "integer",
                    "type": "integer"
                  },
                  "name": {
                    "description": "child_entities name comment",
                    "format": "text",
                    "type": "string"
                  },
                  "parent_id": {
                    "description": "Note:\nThis is a Foreign Key to `entities.id`.<fk table='entities' column='id'/>",
                    "format": "integer",
                    "type": "integer"
                  }
                }
              }
            |]

    describe "RPC" $

      it "includes body schema for arguments" $ do
        r <- simpleBody <$> get "/"
        let args = r ^? key "paths" . key "/rpc/varied_arguments"
                      . key "post"  . key "parameters"
                      . nth 0       . key "schema"

        liftIO $ do
          args `shouldBe` Just
            [aesonQQ|
              {
                "required": [
                  "double",
                  "varchar",
                  "boolean",
                  "date",
                  "money",
                  "enum"
                ],
                "properties": {
                  "double": {
                    "format": "double precision",
                    "type": "string"
                  },
                  "varchar": {
                    "format": "character varying",
                    "type": "string"
                  },
                  "boolean": {
                    "format": "boolean",
                    "type": "boolean"
                  },
                  "date": {
                    "format": "date",
                    "type": "string"
                  },
                  "money": {
                    "format": "money",
                    "type": "string"
                  },
                  "enum": {
                    "format": "test.enum_menagerie_type",
                    "type": "string"
                  },
                  "integer": {
                    "format": "integer",
                    "type": "integer"
                  }
                },
                "type": "object"
              }
            |]

  describe "Allow header" $ do

    it "includes read/write verbs for writeable table" $ do
      r <- request methodOptions "/items" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "GET,POST,PATCH,DELETE"

    it "includes read verbs for read-only table" $ do
      r <- request methodOptions "/has_count_column" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "GET"
