{-# LANGUAGE QuasiQuotes #-}
module Feature.StructureSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import SpecHelper

import Network.HTTP.Types
import Codec.Binary.Base64.String (encode)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)

spec :: Spec
spec = let {uName = "a user"; uPass = "nobody can ever know";
uRole = "dbapi_test"} in
  around withDatabaseConnection $
  aroundWith (withUser uName uPass uRole) $ aroundWith withApp $ do
  describe "GET /" $
    it "lists views in schema" $
      request methodGet "/"
        [("Authorization", "Basic "<>(cs.encode $ cs uName<>":"<>cs uPass))] ""
        `shouldRespondWith` [json| [
        {"schema":"1","name":"authors_only","insertable":true}
        , {"schema":"1","name":"auto_incrementing_pk","insertable":true}
        , {"schema":"1","name":"compound_pk","insertable":true}
        , {"schema":"1","name":"has_fk","insertable":true}
        , {"schema":"1","name":"items","insertable":true}
        , {"schema":"1","name":"menagerie","insertable":true}
        , {"schema":"1","name":"no_pk","insertable":true}
        , {"schema":"1","name":"simple_pk","insertable":true}
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
            "schema": "1",
            "name": "integer",
            "type": "integer",
            "maxLen": null,
            "enum": null,
            "nullable": false,
            "position": 1,
            "references": null,
            "default": null
          }, {
            "default": null,
            "precision": 53,
            "updatable": true,
            "schema": "1",
            "name": "double",
            "type": "double precision",
            "maxLen": null,
            "enum": null,
            "nullable": false,
            "references": null,
            "position": 2
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "1",
            "name": "varchar",
            "type": "character varying",
            "maxLen": null,
            "enum": null,
            "nullable": false,
            "position": 3,
            "references": null,
            "default": null
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "1",
            "name": "boolean",
            "type": "boolean",
            "maxLen": null,
            "enum": null,
            "nullable": false,
            "references": null,
            "position": 4
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "1",
            "name": "date",
            "type": "date",
            "maxLen": null,
            "enum": null,
            "nullable": false,
            "references": null,
            "position": 5
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "1",
            "name": "money",
            "type": "money",
            "maxLen": null,
            "enum": null,
            "nullable": false,
            "position": 6,
            "references": null,
            "default": null
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "1",
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

    it "includes foreign key data" $
      request methodOptions "/has_fk"
        [("Authorization", "Basic "<>(cs.encode $ cs uName<>":"<>cs uPass))] ""
        `shouldRespondWith` [json|
      {
        "pkey": ["id"],
        "columns":[
          {
            "default": "nextval('\"1\".has_fk_id_seq'::regclass)",
            "precision": 64,
            "updatable": true,
            "schema": "1",
            "name": "id",
            "type": "bigint",
            "maxLen": null,
            "nullable": false,
            "position": 1,
            "enum": null,
            "references": null
          }, {
            "default": null,
            "precision": 32,
            "updatable": true,
            "schema": "1",
            "name": "auto_inc_fk",
            "type": "integer",
            "maxLen": null,
            "nullable": true,
            "position": 2,
            "enum": null,
            "references": {"table": "auto_incrementing_pk", "column": "id"}
          }, {
            "default": null,
            "precision": null,
            "updatable": true,
            "schema": "1",
            "name": "simple_fk",
            "type": "character varying",
            "maxLen": 255,
            "nullable": true,
            "position": 3,
            "enum": null,
            "references": {"table": "simple_pk", "column": "k"}
          }
        ]
      }
      |]
