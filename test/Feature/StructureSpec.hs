module Feature.StructureSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import SpecHelper

import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (SResponse(simpleStatus, simpleHeaders, simpleBody))

import Data.Maybe (fromJust)
import Data.Aeson (decode)
import qualified Data.JsonSchema.Draft4 as D4

spec :: SpecWith Application
spec = do

  describe "GET /" $ do
    it "lists views in schema" $
      request methodGet "/" [] ""
        `shouldRespondWith` [json| [
          {"schema":"test","name":"Escap3e;","insertable":true}
        , {"schema":"test","name":"addresses","insertable":true}
        , {"schema":"test","name":"articleStars","insertable":true}
        , {"schema":"test","name":"articles","insertable":true}
        , {"schema":"test","name":"auto_incrementing_pk","insertable":true}
        , {"schema":"test","name":"clients","insertable":true}
        , {"schema":"test","name":"comments","insertable":true}
        , {"schema":"test","name":"complex_items","insertable":true}
        , {"schema":"test","name":"compound_pk","insertable":true}
        , {"schema":"test","name":"empty_table","insertable":true}
        , {"schema":"test","name":"filtered_tasks","insertable":true}
        , {"schema":"test","name":"ghostBusters","insertable":true}
        , {"schema":"test","name":"has_count_column","insertable":false}
        , {"schema":"test","name":"has_fk","insertable":true}
        , {"schema":"test","name":"insertable_view_with_join","insertable":true}
        , {"schema":"test","name":"insertonly","insertable":true}
        , {"schema":"test","name":"items","insertable":true}
        , {"schema":"test","name":"json","insertable":true}
        , {"schema":"test","name":"materialized_view","insertable":false}
        , {"schema":"test","name":"menagerie","insertable":true}
        , {"schema":"test","name":"no_pk","insertable":true}
        , {"schema":"test","name":"nullable_integer","insertable":true}
        , {"schema":"test","name":"orders","insertable":true}
        , {"schema":"test","name":"projects","insertable":true}
        , {"schema":"test","name":"projects_view","insertable":true}
        , {"schema":"test","name":"simple_pk","insertable":true}
        , {"schema":"test","name":"tasks","insertable":true}
        , {"schema":"test","name":"tsearch","insertable":true}
        , {"schema":"test","name":"users","insertable":true}
        , {"schema":"test","name":"users_projects","insertable":true}
        , {"schema":"test","name":"users_tasks","insertable":true}
        , {"schema":"test","name":"withUnique","insertable":true}
        ] |]
        {matchStatus = 200}

    it "lists only views user has permission to see" $ do
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"

      request methodGet "/" [auth] ""
        `shouldRespondWith` [json| [
            {"schema":"test","name":"authors_only","insertable":true}
        ] |]
        {matchStatus = 200}

    it "returns a valid openapi spec" $ do
      r <- request methodGet "/" [("Accept", "application/openapi+json")] ""
      liftIO $
        let respStatus = simpleStatus r in
        respStatus `shouldSatisfy`
          \s -> s == Status { statusCode = 200, statusMessage="OK" }
      liftIO $
        let respHeaders = simpleHeaders r in
        respHeaders `shouldSatisfy`
          \hs -> ("Content-Type", "application/openapi+json; charset=utf-8") `elem` hs
      liftIO $
        let respBody = simpleBody r
            schema :: D4.Schema
            schema = D4.emptySchema { D4._schemaRef = Just "openapi.json" }
            schemaContext :: D4.SchemaWithURI D4.Schema
            schemaContext = D4.SchemaWithURI
              { D4._swSchema = schema
              , D4._swURI    = Just "test/fixtures/openapi.json"
              }
           in
           D4.fetchFilesystemAndValidate schemaContext ((fromJust . decode) respBody) `shouldReturn` Right ()

    it "should respond to openapi request on none root path with 415" $
      request methodGet "/none_root_path"
              (acceptHdrs "application/openapi+json") ""
        `shouldRespondWith` 415

  describe "Table info" $ do
    it "The structure of complex views is correctly detected" $
      request methodOptions "/filtered_tasks" [] "" `shouldRespondWith`
      [json|
      {
        "pkey": [
          "myId"
        ],
        "columns": [
          {
            "references": null,
            "default": null,
            "precision": 32,
            "updatable": true,
            "schema": "test",
            "name": "myId",
            "type": "integer",
            "maxLen": null,
            "enum": [],
            "nullable": true,
            "position": 1
          },
          {
            "references": null,
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "test",
            "name": "name",
            "type": "text",
            "maxLen": null,
            "enum": [],
            "nullable": true,
            "position": 2
          },
          {
            "references": {
              "schema": "test",
              "column": "id",
              "table": "projects"
            },
            "default": null,
            "precision": 32,
            "updatable": true,
            "schema": "test",
            "name": "projectID",
            "type": "integer",
            "maxLen": null,
            "enum": [],
            "nullable": true,
            "position": 3
          }
        ]
      }
      |]

    it "is available with OPTIONS verb" $
      request methodOptions "/menagerie" [] "" `shouldRespondWith`
      [json|
      {
        "pkey":["integer"],
        "columns":[
          {
            "default": null,
            "precision": 32,
            "updatable": true,
            "schema": "test",
            "name": "integer",
            "type": "integer",
            "maxLen": null,
            "enum": [],
            "nullable": false,
            "position": 1,
            "references": null,
            "default": null
          }, {
            "default": null,
            "precision": 53,
            "updatable": true,
            "schema": "test",
            "name": "double",
            "type": "double precision",
            "maxLen": null,
            "enum": [],
            "nullable": false,
            "references": null,
            "position": 2
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "test",
            "name": "varchar",
            "type": "character varying",
            "maxLen": null,
            "enum": [],
            "nullable": false,
            "position": 3,
            "references": null,
            "default": null
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "test",
            "name": "boolean",
            "type": "boolean",
            "maxLen": null,
            "enum": [],
            "nullable": false,
            "references": null,
            "position": 4
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "test",
            "name": "date",
            "type": "date",
            "maxLen": null,
            "enum": [],
            "nullable": false,
            "references": null,
            "position": 5
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "test",
            "name": "money",
            "type": "money",
            "maxLen": null,
            "enum": [],
            "nullable": false,
            "position": 6,
            "references": null,
            "default": null
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "test",
            "name": "enum",
            "type": "test.enum_menagerie_type",
            "maxLen": null,
            "enum": [
              "foo",
              "bar"
            ],
            "nullable": false,
            "position": 7,
            "references": null,
            "default": null
          }
        ]
      }
      |]

    it "it includes primary and foreign keys for views" $
      request methodOptions "/projects_view" [] "" `shouldRespondWith`
      [json|
      {
         "pkey":[
            "id"
         ],
         "columns":[
          {
            "references":null,
            "default":null,
            "precision":32,
            "updatable":true,
            "schema":"test",
            "name":"id",
            "type":"integer",
            "maxLen":null,
            "enum":[],
            "nullable":true,
            "position":1
          },
          {
            "references":null,
            "default":null,
            "precision":null,
            "updatable":true,
            "schema":"test",
            "name":"name",
            "type":"text",
            "maxLen":null,
            "enum":[],
            "nullable":true,
            "position":2
          },
          {
            "references": {
              "schema":"test",
              "column":"id",
              "table":"clients"
            },
            "default":null,
            "precision":32,
            "updatable":true,
            "schema":"test",
            "name":"client_id",
            "type":"integer",
            "maxLen":null,
            "enum":[],
            "nullable":true,
            "position":3
          }
        ]
      }
      |]

    it "includes foreign key data" $
      request methodOptions "/has_fk" [] ""
        `shouldRespondWith` [json|
      {
        "pkey": ["id"],
        "columns":[
          {
            "default": "nextval('test.has_fk_id_seq'::regclass)",
            "precision": 64,
            "updatable": true,
            "schema": "test",
            "name": "id",
            "type": "bigint",
            "maxLen": null,
            "nullable": false,
            "position": 1,
            "enum": [],
            "references": null
          }, {
            "default": null,
            "precision": 32,
            "updatable": true,
            "schema": "test",
            "name": "auto_inc_fk",
            "type": "integer",
            "maxLen": null,
            "nullable": true,
            "position": 2,
            "enum": [],
            "references": {"schema":"test", "table": "auto_incrementing_pk", "column": "id"}
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "test",
            "name": "simple_fk",
            "type": "character varying",
            "maxLen": 255,
            "nullable": true,
            "position": 3,
            "enum": [],
            "references": {"schema":"test", "table": "simple_pk", "column": "k"}
          }
        ]
      }
      |]

    it "includes all information on views for renamed columns, and raises relations to correct schema" $
      request methodOptions "/articleStars" [] ""
        `shouldRespondWith` [json|
          {
            "pkey": [
              "articleId",
              "userId"
            ],
            "columns": [
              {
                "references": {
                  "schema": "test",
                  "column": "id",
                  "table": "articles"
                },
                "default": null,
                "precision": 32,
                "updatable": true,
                "schema": "test",
                "name": "articleId",
                "type": "integer",
                "maxLen": null,
                "enum": [],
                "nullable": true,
                "position": 1
              },
              {
                "references": {
                  "schema": "test",
                  "column": "id",
                  "table": "users"
                },
                "default": null,
                "precision": 32,
                "updatable": true,
                "schema": "test",
                "name": "userId",
                "type": "integer",
                "maxLen": null,
                "enum": [],
                "nullable": true,
                "position": 2
              },
              {
                "references": null,
                "default": null,
                "precision": null,
                "updatable": true,
                "schema": "test",
                "name": "createdAt",
                "type": "timestamp without time zone",
                "maxLen": null,
                "enum": [],
                "nullable": true,
                "position": 3
              }
            ]
          }
        |]

    it "errors for non existant tables" $
      request methodOptions "/dne" [] "" `shouldRespondWith` 404

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
