{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Feature.StructureSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import SpecHelper

import Network.HTTP.Types

spec :: Spec
spec = around appWithFixture $ do
  describe "GET /" $
    it "lists views in schema" $
      get "/" `shouldRespondWith`
        [json| [
          {"schema":"1","name":"auto_incrementing_pk","insertable":true}
        , {"schema":"1","name":"compound_pk","insertable":true}
        , {"schema":"1","name":"items","insertable":true}
        , {"schema":"1","name":"menagerie","insertable":true}
        , {"schema":"1","name":"no_pk","insertable":true}
        , {"schema":"1","name":"simple_pk","insertable":true}
        ] |]
        {matchStatus = 200}

  describe "Table info" $
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
            "position": 1
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
            "position": 3
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
            "position": 6
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
            "position": 7
          }
        ]
      }
      |]
