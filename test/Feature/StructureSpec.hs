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
      request methodOptions "/auto_incrementing_pk" [] "" `shouldRespondWith` [json|
      {
        "pkey":["id"],
        "columns":[
          {
            "precision": 32,
            "updatable": true,
            "schema": "1",
            "name": "id",
            "type": "integer",
            "maxLen": null,
            "nullable": false,
            "position": 1
          }, {
            "precision": null,
            "updatable": true,
            "schema": "1",
            "name": "nullable_string",
            "type": "character varying",
            "maxLen": null,
            "nullable": true,
            "position": 2
          }, {
            "precision": null,
            "updatable": true,
            "schema": "1",
            "name": "non_nullable_string",
            "type": "character varying",
            "maxLen": null,
            "nullable": false,
            "position": 3
          }, {
            "precision": null,
            "updatable": true,
            "schema": "1",
            "name": "inserted_at",
            "type": "timestamp with time zone",
            "maxLen": null,
            "nullable": true,
            "position": 4
          }
        ]
      }
      |]
