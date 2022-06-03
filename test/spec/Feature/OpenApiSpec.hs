module Feature.OpenApiSpec where

import Control.Lens     ((^?))
import Data.Aeson.Types (Value (..))
import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import Data.Aeson.Lens
import Data.Aeson.QQ
import Network.HTTP.Types
import Test.Hspec         hiding (pendingWith)
import Test.Hspec.Wai

import PostgREST.Config.PgVersion (PgVersion, pgVersion100,
                                   pgVersion110)

import PostgREST.Version (docsVersion)
import Protolude         hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec actualPgVersion = describe "OpenAPI" $ do
  it "root path returns a valid openapi spec" $ do
    validateOpenApiResponse [("Accept", "application/openapi+json")]
    request methodHead "/" (acceptHdrs "application/openapi+json") ""
      `shouldRespondWith` "" { matchStatus  = 200 }

  it "should respond to openapi request on none root path with 415" $
    request methodGet "/items"
            (acceptHdrs "application/openapi+json") ""
      `shouldRespondWith` 415

  it "includes postgrest.org current version api docs" $ do
    r <- simpleBody <$> get "/"

    let docsUrl = r ^? key "externalDocs" . key "url"

    liftIO $ docsUrl `shouldBe` Just (String ("https://postgrest.org/en/" <> docsVersion <> "/api.html"))

  describe "table" $ do

    it "includes paths to tables" $ do
      r <- simpleBody <$> get "/"

      let method s = key "paths" . key "/child_entities" . key s
          childGetSummary = r ^? method "get" . key "summary"
          childGetDescription = r ^? method "get" . key "description"
          getParameters = r ^? method "get" . key "parameters"
          postParameters = r ^? method "post" . key "parameters"
          postResponse = r ^? method "post" . key "responses" . key "201" . key "description"
          patchResponse = r ^? method "patch" . key "responses" . key "204" . key "description"
          deleteResponse = r ^? method "delete" . key "responses" . key "204" . key "description"

      let grandChildGet s = key "paths" . key "/grandchild_entities" . key "get" . key s
          grandChildGetSummary = r ^? grandChildGet "summary"
          grandChildGetDescription = r ^? grandChildGet "description"

      liftIO $ do

        childGetSummary `shouldBe` Just "child_entities comment"

        childGetDescription `shouldBe` Nothing

        grandChildGetSummary `shouldBe` Just "grandchild_entities summary"

        grandChildGetDescription `shouldBe` Just "grandchild_entities description\nthat spans\nmultiple lines"

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

        postParameters `shouldBe` Just
          [aesonQQ|
            [
              { "$ref": "#/parameters/body.child_entities" },
              { "$ref": "#/parameters/select" },
              { "$ref": "#/parameters/preferReturn" }
            ]
          |]

        postResponse `shouldBe` Just "Created"

        patchResponse `shouldBe` Just "No Content"

        deleteResponse `shouldBe` Just "No Content"

    it "includes an array type for GET responses" $ do
      r <- simpleBody <$> get "/"

      let childGetSchema = r ^? key "paths"
            . key "/child_entities"
            . key "get"
            . key "responses"
            . key "200"
            . key "schema"

      liftIO $
        childGetSchema `shouldBe` Just
          [aesonQQ|
            {
              "items": {
                "$ref": "#/definitions/child_entities"
              },
              "type": "array"
            }
          |]

    it "includes definitions to tables" $ do
      r <- simpleBody <$> get "/"

      let def = r ^? key "definitions" . key "child_entities"

      liftIO $

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
                  "description": "child_entities name comment. Can be longer than sixty-three characters long",
                  "format": "text",
                  "type": "string"
                },
                "parent_id": {
                  "description": "Note:\nThis is a Foreign Key to `entities.id`.<fk table='entities' column='id'/>",
                  "format": "integer",
                  "type": "integer"
                }
              },
              "required": [
                "id"
              ]
            }
          |]

    it "doesn't include privileged table for anonymous" $ do
      r <- simpleBody <$> get "/"
      let tablePath = r ^? key "paths" . key "/authors_only"

      liftIO $ tablePath `shouldBe` Nothing

    it "includes table if user has permission" $ do
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIn0.Xod-F15qsGL0WhdOCr2j3DdKuTw9QJERVgoFD3vGaWA"
      r <- simpleBody <$> request methodGet "/" [auth] ""
      let tableTag = r ^? key "paths" . key "/authors_only"
                    . key "post"  . key "tags"
                    . nth 0
      liftIO $ tableTag `shouldBe` Just [aesonQQ|"authors_only"|]

  describe "Foreign table" $

    it "includes foreign table properties" $ do
      r <- simpleBody <$> get "/"

      let method s = key "paths" . key "/projects_dump" . key s
          getSummary = r ^? method "get" . key "summary"
          getDescription = r ^? method "get" . key "description"
          getParameters = r ^? method "get" . key "parameters"

      liftIO $ do

        getSummary `shouldBe` Just "A temporary projects dump"

        getDescription `shouldBe` Just "Just a test for foreign tables"

        getParameters `shouldBe` Just
          [aesonQQ|
            [
              { "$ref": "#/parameters/rowFilter.projects_dump.id" },
              { "$ref": "#/parameters/rowFilter.projects_dump.name" },
              { "$ref": "#/parameters/rowFilter.projects_dump.client_id" },
              { "$ref": "#/parameters/select" },
              { "$ref": "#/parameters/order" },
              { "$ref": "#/parameters/range" },
              { "$ref": "#/parameters/rangeUnit" },
              { "$ref": "#/parameters/offset" },
              { "$ref": "#/parameters/limit" },
              { "$ref": "#/parameters/preferCount" }
            ]
          |]

  when (actualPgVersion >= pgVersion100) $ do
    describe "Partitioned table" $

      it "includes partitioned table properties" $ do
        r <- simpleBody <$> get "/"

        let method s = key "paths" . key "/car_models" . key s
            getSummary = r ^? method "get" . key "summary"
            getDescription = r ^? method "get" . key "description"
            getParameterName = r ^? method "get" . key "parameters" . nth 0 . key "$ref"
            getParameterYear = r ^? method "get" . key "parameters" . nth 1 . key "$ref"
            getParameterRef = r ^? method "get" . key "parameters" . nth 2 . key "$ref"

        liftIO $ do

          getSummary `shouldBe` Just "A partitioned table"

          getDescription `shouldBe` Just "A test for partitioned tables"

          getParameterName `shouldBe` Just "#/parameters/rowFilter.car_models.name"

          getParameterYear `shouldBe` Just "#/parameters/rowFilter.car_models.year"

          when (actualPgVersion >= pgVersion110) $
            getParameterRef `shouldBe` Just "#/parameters/rowFilter.car_models.car_brand_name"

  describe "Materialized view" $

    it "includes materialized view properties" $ do
      r <- simpleBody <$> get "/"

      let method s = key "paths" . key "/materialized_projects" . key s
          summary = r ^? method "get" . key "summary"
          description = r ^? method "get" . key "description"
          parameters = r ^? method "get" . key "parameters"

      liftIO $ do

        summary `shouldBe` Just "A materialized view for projects"

        description `shouldBe` Just "Just a test for materialized views"

        parameters `shouldBe` Just
          [aesonQQ|
            [
              { "$ref": "#/parameters/rowFilter.materialized_projects.id" },
              { "$ref": "#/parameters/rowFilter.materialized_projects.name" },
              { "$ref": "#/parameters/rowFilter.materialized_projects.client_id" },
              { "$ref": "#/parameters/select" },
              { "$ref": "#/parameters/order" },
              { "$ref": "#/parameters/range" },
              { "$ref": "#/parameters/rangeUnit" },
              { "$ref": "#/parameters/offset" },
              { "$ref": "#/parameters/limit" },
              { "$ref": "#/parameters/preferCount" }
            ]
          |]

  describe "VIEW that has a source FK based on a UNIQUE key" $

    it "includes fk description" $ do
      r <- simpleBody <$> get "/"

      let referralLink = r ^? key "definitions" . key "referrals" . key "properties" . key "link"

      liftIO $
        referralLink `shouldBe` Just
          [aesonQQ|
            {
              "format": "integer",
              "type": "integer",
              "description": "Note:\nThis is a Foreign Key to `pages.link`.<fk table='pages' column='link'/>"
            }
          |]

  describe "PostgreSQL to Swagger Type Mapping" $ do

    it "character varying to string" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_character_varying"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "character varying",
              "type": "string"
            }
          |]
    it "character(1) to string" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_character"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "maxLength": 1,
              "format": "character",
              "type": "string"
            }
          |]

    it "text to string" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_text"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "text",
              "type": "string"
            }
          |]

    it "boolean to boolean" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_boolean"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "boolean",
              "type": "boolean"
            }
          |]

    it "smallint to integer" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_smallint"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "smallint",
              "type": "integer"
            }
          |]

    it "integer to integer" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_integer"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "integer",
              "type": "integer"
            }
          |]

    it "bigint to integer" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_bigint"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "bigint",
              "type": "integer"
            }
          |]

    it "numeric to number" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_numeric"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "numeric",
              "type": "number"
            }
          |]

    it "real to number" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_real"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "real",
              "type": "number"
            }
          |]

    it "double_precision to number" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_double_precision"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "double precision",
              "type": "number"
            }
          |]

    it "json to any" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_json"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "json"
            }
          |]

    it "jsonb to any" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_types" . key "properties" . key "a_jsonb"

      liftIO $

        types `shouldBe` Just
          [aesonQQ|
            {
              "format": "jsonb"
            }
          |]

  describe "Detects default values" $ do

    it "text" $ do
      r <- simpleBody <$> get "/"

      let defaultValue = r ^? key "definitions" . key "openapi_defaults" . key "properties" . key "text" . key "default"

      liftIO $

        defaultValue `shouldBe` Just "default"

    it "boolean" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_defaults" . key "properties" . key "boolean" . key "default"

      liftIO $

        types `shouldBe` Just (Bool False)

    it "integer" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_defaults" . key "properties" . key "integer" . key "default"

      liftIO $

        types `shouldBe` Just (Number 42)

    it "numeric" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_defaults" . key "properties" . key "numeric" . key "default"

      liftIO $

        types `shouldBe` Just (Number 42.2)

    it "date" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_defaults" . key "properties" . key "date" . key "default"

      liftIO $

        types `shouldBe` Just "1900-01-01"

    it "time" $ do
      r <- simpleBody <$> get "/"

      let types = r ^? key "definitions" . key "openapi_defaults" . key "properties" . key "time" . key "default"

      liftIO $

        types `shouldBe` Just "13:00:00"

  describe "RPC" $ do

    it "includes function summary/description and body schema for arguments" $ do
      r <- simpleBody <$> get "/"

      let method s = key "paths" . key "/rpc/varied_arguments" . key s
          args = r ^? method "post" . key "parameters" . nth 0 . key "schema"
          summary = r ^? method "post" . key "summary"
          description = r ^? method "post" . key "description"

      liftIO $ do

        summary `shouldBe` Just "An RPC function"

        description `shouldBe` Just "Just a test for RPC function arguments"

        args `shouldBe` Just
          [aesonQQ|
            {
              "required": [
                "double",
                "varchar",
                "boolean",
                "date",
                "money",
                "enum",
                "arr"
              ],
              "properties": {
                "double": {
                  "format": "double precision",
                  "type": "number"
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
                  "format": "enum_menagerie_type",
                  "type": "string"
                },
                "arr": {
                  "format": "text[]",
                  "type": "string"
                },
                "integer": {
                  "format": "integer",
                  "type": "integer"
                },
                "json": {
                  "format": "json"
                },
                "jsonb": {
                  "format": "jsonb"
                }
              },
              "type": "object",
              "description": "An RPC function\n\nJust a test for RPC function arguments"
            }
          |]

    it "doesn't include privileged function for anonymous" $ do
      r <- simpleBody <$> get "/"
      let funcPath = r ^? key "paths" . key "/rpc/privileged_hello"

      liftIO $ funcPath `shouldBe` Nothing

    it "includes function if user has permission" $ do
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIn0.Xod-F15qsGL0WhdOCr2j3DdKuTw9QJERVgoFD3vGaWA"
      r <- simpleBody <$> request methodGet "/" [auth] ""
      let funcTag = r ^? key "paths" . key "/rpc/privileged_hello"
                    . key "post"  . key "tags"
                    . nth 0

      liftIO $ funcTag `shouldBe` Just [aesonQQ|"(rpc) privileged_hello"|]

    it "doesn't include OUT params of function as required parameters" $ do
      r <- simpleBody <$> get "/"
      let params = r ^? key "paths" . key "/rpc/many_out_params"
                      . key "post" . key "parameters" .  nth 0
                      . key "schema". key "required"

      liftIO $ params `shouldBe` Nothing

    it "includes INOUT params(with no DEFAULT) of function as required parameters" $ do
      r <- simpleBody <$> get "/"
      let params = r ^? key "paths" . key "/rpc/many_inout_params"
                      . key "post" . key "parameters" .  nth 0
                      . key "schema". key "required"

      liftIO $ params `shouldBe` Just [aesonQQ|["num", "str"]|]

