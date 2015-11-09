module Feature.StructureSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import SpecHelper

import Network.HTTP.Types

spec :: Spec
spec = around withApp $ do
  describe "GET /" $ do
    it "lists views in schema" $
      request methodGet "/" [] ""
        `shouldRespondWith` [json| [
          {"schema":"test","name":"auto_incrementing_pk","insertable":true}
        , {"schema":"test","name":"clients","insertable":true}
        , {"schema":"test","name":"comments","insertable":true}
        , {"schema":"test","name":"complex_items","insertable":true}
        , {"schema":"test","name":"compound_pk","insertable":true}
        , {"schema":"test","name":"has_count_column","insertable":false}
        , {"schema":"test","name":"has_fk","insertable":true}
        , {"schema":"test","name":"insertable_view_with_join","insertable":true}
        , {"schema":"test","name":"items","insertable":true}
        , {"schema":"test","name":"json","insertable":true}
        , {"schema":"test","name":"materialized_view","insertable":false}
        , {"schema":"test","name":"menagerie","insertable":true}
        , {"schema":"test","name":"no_pk","insertable":true}
        , {"schema":"test","name":"nullable_integer","insertable":true}
        , {"schema":"test","name":"projects","insertable":true}
        , {"schema":"test","name":"projects_view","insertable":true}
        , {"schema":"test","name":"simple_pk","insertable":true}
        , {"schema":"test","name":"tasks","insertable":true}
        , {"schema":"test","name":"tsearch","insertable":true}
        , {"schema":"test","name":"users","insertable":true}
        , {"schema":"test","name":"users_projects","insertable":true}
        , {"schema":"test","name":"users_tasks","insertable":true}
        ] |]
        {matchStatus = 200}

    it "lists only views user has permission to see" $ do
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"

      request methodGet "/" [auth] ""
        `shouldRespondWith` [json| [
            {"schema":"test","name":"authors_only","insertable":true}
        ] |]
        {matchStatus = 200}


  describe "Table info" $ do
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
            "type": "USER-DEFINED",
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
      request methodOptions "/insertable_view_with_join" [] "" `shouldRespondWith`
      [json|
      {
         "pkey":[
            "id"
         ],
         "columns":[
            {
               "references":null,
               "default":null,
               "precision":64,
               "updatable":false,
               "schema":"test",
               "name":"id",
               "type":"bigint",
               "maxLen":null,
               "enum":[],
               "nullable":true,
               "position":1
            },
            {
               "references":{
                  "schema":"test",
                  "column":"id",
                  "table":"auto_incrementing_pk"
               },
               "default":null,
               "precision":32,
               "updatable":false,
               "schema":"test",
               "name":"auto_inc_fk",
               "type":"integer",
               "maxLen":null,
               "enum":[],
               "nullable":true,
               "position":2
            },
            {
               "references":{
                  "schema":"test",
                  "column":"k",
                  "table":"simple_pk"
               },
               "default":null,
               "precision":null,
               "updatable":false,
               "schema":"test",
               "name":"simple_fk",
               "type":"character varying",
               "maxLen":255,
               "enum":[],
               "nullable":true,
               "position":3
            },
            {
               "references":null,
               "default":null,
               "precision":null,
               "updatable":false,
               "schema":"test",
               "name":"nullable_string",
               "type":"character varying",
               "maxLen":null,
               "enum":[],
               "nullable":true,
               "position":4
            },
            {
               "references":null,
               "default":null,
               "precision":null,
               "updatable":false,
               "schema":"test",
               "name":"non_nullable_string",
               "type":"character varying",
               "maxLen":null,
               "enum":[],
               "nullable":true,
               "position":5
            },
            {
               "references":null,
               "default":null,
               "precision":null,
               "updatable":false,
               "schema":"test",
               "name":"inserted_at",
               "type":"timestamp with time zone",
               "maxLen":null,
               "enum":[],
               "nullable":true,
               "position":6
            }
         ]
      }
      |]

    it "includes foreign key data" $ do
      pendingWith "have to resolve issue #107"

      request methodOptions "/has_fk" [] ""
        `shouldRespondWith` [json|
      {
        "pkey": ["id"],
        "columns":[
          {
            "default": "nextval('\"1\".has_fk_id_seq'::regclass)",
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
            "references": {"table": "auto_incrementing_pk", "column": "id"}
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
            "references": {"table": "simple_pk", "column": "k"}
          }
        ]
      }
      |]
