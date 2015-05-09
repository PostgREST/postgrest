module Feature.InsertSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Test (SResponse(simpleBody,simpleHeaders,simpleStatus))

import SpecHelper

import qualified Data.Aeson as JSON
import Data.Maybe (fromJust)
import Text.Heredoc
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Control.Monad (replicateM_)

import TestTypes(IncPK(..), CompoundPK(..))

spec :: Spec
spec = afterAll_ resetDb $ around withApp $ do
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

      context "into a table with no pk" . after_ (clearTable "no_pk") $ do
        it "succeeds with 201 and a link including all fields" $ do
          p <- post "/no_pk" [json| { "a":"foo", "b":"bar" } |]
          liftIO $ do
            simpleBody p `shouldBe` ""
            simpleHeaders p `shouldSatisfy` matchHeader hLocation "/no_pk\\?a=eq.foo&b=eq.bar"
            simpleStatus p `shouldBe` created201

        it "returns full details of inserted record if asked" $ do
          p <- request methodPost "/no_pk"
                       [("Prefer", "return=representation")]
                       [json| { "a":"bar", "b":"baz" } |]
          liftIO $ do
            simpleBody p `shouldBe` [json| { "a":"bar", "b":"baz" } |]
            simpleHeaders p `shouldSatisfy` matchHeader hLocation "/no_pk\\?a=eq.bar&b=eq.baz"
            simpleStatus p `shouldBe` created201

        it "can post nulls" $ do
          p <- request methodPost "/no_pk"
                       [("Prefer", "return=representation")]
                       [json| { "a":null, "b":"foo" } |]
          liftIO $ do
            simpleBody p `shouldBe` [json| { "a":null, "b":"foo" } |]
            simpleHeaders p `shouldSatisfy` matchHeader hLocation "/no_pk\\?a=is.null&b=eq.foo"
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

    context "jsonb" . after_ (clearTable "json") $ do
      it "serializes nested object" $ do
        let inserted = [json| { "data": { "foo":"bar" } } |]
        p <- request methodPost "json" [("Prefer", "return=representation")] inserted
        liftIO $ do
          simpleBody p `shouldBe` inserted
          simpleHeaders p `shouldSatisfy` matchHeader hLocation "/json\\?data=eq\\.%7B%22foo%22%3A%22bar%22%7D"
          simpleStatus p `shouldBe` created201
      it "serializes nested array" $ do
        let inserted = [json| { "data": [1,2,3] } |]
        p <- request methodPost "json" [("Prefer", "return=representation")] inserted
        liftIO $ do
          simpleBody p `shouldBe` inserted
          simpleHeaders p `shouldSatisfy` matchHeader hLocation "/json\\?data=eq\\.%5B1%2C2%2C3%5D"
          simpleStatus p `shouldBe` created201

  describe "CSV insert" $ do

    after_ (clearTable "menagerie") . context "disparate csv types" $
      it "succeeds with multipart response" $ do
        p <- request methodPost "/menagerie" [("Content-Type", "text/csv")]
               [str|integer,double,varchar,boolean,date,money,enum
                   |13,3.14159,testing!,false,1900-01-01,$3.99,foo
                   |12,0.1,a string,true,1929-10-01,12,bar
                   |]
        liftIO $ do
          simpleBody p `shouldBe` "Content-Type: application/json\nLocation: /menagerie?integer=eq.13\n\n\n--postgrest_boundary\nContent-Type: application/json\nLocation: /menagerie?integer=eq.12\n\n"
          simpleStatus p `shouldBe` created201

    after_ (clearTable "no_pk") . context "requesting full representation" $ do
      it "returns full details of inserted record" $
        request methodPost "/no_pk"
                     [("Content-Type", "text/csv"), ("Prefer", "return=representation")]
                     "a,b\nbar,baz"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [json| { "a":"bar", "b":"baz" } |]
          , matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "application/json",
                            "Location" <:> "/no_pk?a=eq.bar&b=eq.baz"]
          }

      it "can post nulls" $
        request methodPost "/no_pk"
                     [("Content-Type", "text/csv"), ("Prefer", "return=representation")]
                     "a,b\nNULL,foo"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [json| { "a":null, "b":"foo" } |]
          , matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "application/json",
                            "Location" <:> "/no_pk?a=is.null&b=eq.foo"]
          }

    after_ (clearTable "no_pk") . context "with wrong number of columns" $ do
      it "fails for too few" $ do
        p <- request methodPost "/no_pk" [("Content-Type", "text/csv")] "a,b\nfoo,bar\nbaz"
        liftIO $ simpleStatus p `shouldBe` badRequest400
      it "fails for too many" $ do
        p <- request methodPost "/no_pk" [("Content-Type", "text/csv")] "a,b\nfoo,bar\nbaz,bat,bad"
        liftIO $ simpleStatus p `shouldBe` badRequest400

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
      it "indicates no records found to update" $
        request methodPatch "/simple_pk" []
          [json| { "extra":20 } |]
            `shouldRespondWith` 404

    context "in a nonempty table" . before_ (clearTable "items" >> createItems 15) .
      after_ (clearTable "items") $ do
      it "can update a single item" $ do
        g <- get "/items?id=eq.42"
        liftIO $ simpleHeaders g
          `shouldSatisfy` matchHeader "Content-Range" "\\*/0"
        request methodPatch "/items?id=eq.1" []
          [json| { "id":42 } |]
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Nothing,
              matchStatus  = 204,
              matchHeaders = ["Content-Range" <:> "0-0/1"]
            }
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

      it "can provide a representation" $ do
        _ <- post "/items"
          [json| { id: 1 } |]
        request methodPatch
          "/items?id=eq.1"
          [("Prefer", "return=representation")]
          [json| { id: 99 } |]
          `shouldRespondWith` [json| [{id:99}] |]
