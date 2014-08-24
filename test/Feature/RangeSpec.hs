{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Feature.RangeSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import SpecHelper

import Network.HTTP.Types

spec :: Spec
spec = around appWithFixture $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "lists views in schema" $ do
      get "/" `shouldRespondWith` [json|
        [{"schema":"1","name":"auto_incrementing_pk","insertable":true}]
      |]

  describe "Table info" $ do
    it "is available with OPTIONS verb" $ do
      -- {{{ big json object
      request methodOptions "/auto_incrementing_pk" "" `shouldRespondWith` [json|
      {
        "pkey":["id"],
        "columns":{
          "inserted_at":{
            "precision":null,
            "updatable":true,
            "schema":"1",
            "name":"inserted_at",
            "type":"timestamp with time zone",
            "maxLen":null,
            "nullable":true,
            "position":4},
          "id":{
            "precision":32,
            "updatable":true,
            "schema":"1",
            "name":"id",
            "type":"integer",
            "maxLen":null,
            "nullable":false,
            "position":1},
          "non_nullable_string":{
            "precision":null,
            "updatable":true,
            "schema":"1",
            "name":"non_nullable_string",
            "type":"character varying",
            "maxLen":null,
            "nullable":false,
            "position":3},
          "nullable_string":{
            "precision":null,
            "updatable":true,
            "schema":"1",
            "name":"nullable_string",
            "type":"character varying",
            "maxLen":null,
            "nullable":true,
            "position":2}}
      }
      |]
      -- }}}

  describe "GET /view" $ do
    context "without range headers" $ do
      context "with response under server size limit" $ do
        it "returns whole range with status 200" $ do
          get "/auto_incrementing_pk" `shouldRespondWith` 206

  describe "Posting new record" $ do
    context "into a table with auto-incrementing pk" $ do
      it "does not require pk in the payload" $ do
        post "/auto_incrementing_pk" [json|
          { "non_nullable_string":"not null"} |]
          `shouldRespondWith` 200
