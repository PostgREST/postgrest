{-# LANGUAGE QuasiQuotes #-}
module Feature.InsertSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Test (SResponse(simpleBody,simpleHeaders,simpleStatus))

import SpecHelper

import qualified Data.Aeson as JSON
import Data.Maybe (fromJust)
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Control.Monad (replicateM_)

import TestTypes(IncPK(..), CompoundPK(..))

--import Debug.Trace

spec :: Spec
spec = around withApp $ do
  describe "Posting new record" $ do
    after_ (clearTable "menagerie") . it "accepts disparate json types" $ do
      p <- post "/menagerie"
        [json| {
          "integer": 13, "double": 3.14159, "varchar": "testing!"
        , "boolean": false, "date": "1900-01-01", "money": "$3.99"
        , "enum": "foo"
        } |]
      liftIO $ do
        simpleBody p `shouldBe` ""
        simpleStatus p `shouldBe` created201

    context "with no pk supplied" $ do
      context "into a table with auto-incrementing pk" . after_ (clearTable "auto_incrementing_pk") $
        it "succeeds with 201 and link" $ do
          p <- post "/auto_incrementing_pk" [json| { "non_nullable_string":"not null"} |]
          liftIO $ do
            simpleBody p `shouldBe` ""
            simpleHeaders p `shouldSatisfy` matchHeader hLocation "/auto_incrementing_pk\\?id=eq\\.[0-9]+"
            simpleStatus p `shouldBe` created201
          let Just location = lookup hLocation $ simpleHeaders p
          r <- get location
          let [record] = fromJust (JSON.decode $ simpleBody r :: Maybe [IncPK])
          liftIO $ do
            incStr record `shouldBe` "not null"
            incNullableStr record `shouldBe` Nothing

      context "into a table with simple pk" $
        it "fails with 400 and error" $
          post "/simple_pk" [json| { "extra":"foo"} |]
            `shouldRespondWith` 400

      context "into a table with no pk" . after_ (clearTable "no_pk") $
        it "succeeds with 201 and a link including all fields" $ do
          p <- post "/no_pk" [json| { "a":"foo", "b":"bar" } |]
          liftIO $ do
            simpleBody p `shouldBe` ""
            simpleHeaders p `shouldSatisfy` matchHeader hLocation "/no_pk\\?a=eq.foo&b=eq.bar"
            simpleStatus p `shouldBe` created201

    context "with compound pk supplied" . after_ (clearTable "compound_pk") $
      it "builds response location header appropriately" $
        post "/compound_pk" [json| { "k1":12, "k2":42 } |]
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Nothing,
            matchStatus  = 201,
            matchHeaders = ["Location" <:> "/compound_pk?k1=eq.12&k2=eq.42"]
          }

    context "with invalid json payload" $
      it "fails with 400 and error" $
        post "/simple_pk" "}{ x = 2"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [json| {"message":"Failed to parse JSON payload. Failed reading: satisfy"} |]
          , matchStatus  = 400
          , matchHeaders = []
          }

  describe "Putting record" $ do

    context "to unkonwn uri" $
      it "gives a 404" $
        request methodPut "/fake" []
          [json| { "real": false } |]
            `shouldRespondWith` 404

    context "to a known uri" $ do
      context "without a fully-specified primary key" $
        it "is not an allowed operation" $
          request methodPut "/compound_pk?k1=eq.12" []
            [json| { "k1":12, "k2":42 } |]
              `shouldRespondWith` 405

      context "with a fully-specified primary key" $ do

        context "not specifying every column in the table" $
          it "is rejected for lack of idempotence" $
            request methodPut "/compound_pk?k1=eq.12&k2=eq.42" []
              [json| { "k1":12, "k2":42 } |]
                `shouldRespondWith` 400

        context "specifying every column in the table" . after_ (clearTable "compound_pk") $ do
          it "can create a new record" $ do
            p <- request methodPut "/compound_pk?k1=eq.12&k2=eq.42" []
                 [json| { "k1":12, "k2":42, "extra":3 } |]
            liftIO $ do
              simpleBody p `shouldBe` ""
              simpleStatus p `shouldBe` status204

            r <- get "/compound_pk?k1=eq.12&k2=eq.42"
            let rows = fromJust (JSON.decode $ simpleBody r :: Maybe [CompoundPK])
            liftIO $ do
              length rows `shouldBe` 1
              let record = head rows
              compoundK1 record `shouldBe` 12
              compoundK2 record `shouldBe` 42
              compoundExtra record `shouldBe` Just 3

          it "can update an existing record" $ do
            _ <- request methodPut "/compound_pk?k1=eq.12&k2=eq.42" []
                 [json| { "k1":12, "k2":42, "extra":4 } |]
            _ <- request methodPut "/compound_pk?k1=eq.12&k2=eq.42" []
                 [json| { "k1":12, "k2":42, "extra":5 } |]

            r <- get "/compound_pk?k1=eq.12&k2=eq.42"
            let rows = fromJust (JSON.decode $ simpleBody r :: Maybe [CompoundPK])
            liftIO $ do
              length rows `shouldBe` 1
              let record = head rows
              compoundExtra record `shouldBe` Just 5

      context "with an auto-incrementing primary key" . after_ (clearTable "auto_incrementing_pk") $

        it "succeeds with 204" $
          request methodPut "/auto_incrementing_pk?id=eq.1" []
               [json| {
                 "id":1,
                 "nullable_string":"hi",
                 "non_nullable_string":"bye",
                 "inserted_at": "2020-11-11"
               } |]
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Nothing,
              matchStatus  = 204,
              matchHeaders = []
            }

  describe "Patching record" $ do

    context "to unkonwn uri" $
      it "gives a 404" $
        request methodPatch "/fake" []
          [json| { "real": false } |]
            `shouldRespondWith` 404

    context "on an empty table" $
      it "succeeds with no effect" $
        request methodPatch "/simple_pk" []
          [json| { "extra":20 } |]
            `shouldRespondWith` 204

    context "in a nonempty table" . before_ (clearTable "items" >> createItems 15) .
      after_ (clearTable "items") $ do
      it "can update a single item" $ do
        g <- get "/items?id=eq.42"
        liftIO $ simpleHeaders g
          `shouldSatisfy` matchHeader "Content-Range" "\\*/0"
        request methodPatch "/items?id=eq.1" []
          [json| { "id":42 } |]
            `shouldRespondWith` 204
        g' <- get "/items?id=eq.42"
        liftIO $ simpleHeaders g'
          `shouldSatisfy` matchHeader "Content-Range" "0-0/1"

      it "can update multiple items" $ do
        replicateM_ 10 $ post "/auto_incrementing_pk"
          [json| { non_nullable_string: "a" } |]
        replicateM_ 10 $ post "/auto_incrementing_pk"
          [json| { non_nullable_string: "b" } |]
        _ <- request methodPatch
          "/auto_incrementing_pk?non_nullable_string=eq.a" []
          [json| { non_nullable_string: "c" } |]
        g <- get "/auto_incrementing_pk?non_nullable_string=eq.c"
        liftIO $ simpleHeaders g
          `shouldSatisfy` matchHeader "Content-Range" "0-9/10"
