module Feature.InsertSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Test (SResponse(simpleBody,simpleHeaders,simpleStatus))

import SpecHelper

import qualified Data.Aeson as JSON
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Text.Heredoc
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Control.Monad (replicateM_, void)

import TestTypes(IncPK(..), CompoundPK(..))
import Network.Wai (Application)

spec :: SpecWith Application
spec = do
  describe "Posting new record" $ do
    context "disparate json types" $ do
      it "accepts disparate json types" $ do
        p <- post "/menagerie"
          [json| {
            "integer": 13, "double": 3.14159, "varchar": "testing!"
          , "boolean": false, "date": "1900-01-01", "money": "$3.99"
          , "enum": "foo"
          } |]
        liftIO $ do
          simpleBody p `shouldBe` ""
          simpleStatus p `shouldBe` created201

      it "filters columns in result using &select" $
        request methodPost "/menagerie?select=integer,varchar" [("Prefer", "return=representation")]
          [json| {
            "integer": 14, "double": 3.14159, "varchar": "testing!"
          , "boolean": false, "date": "1900-01-01", "money": "$3.99"
          , "enum": "foo"
          } |] `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|{"integer":14,"varchar":"testing!"}|]
          , matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
          }

      it "includes related data after insert" $
        request methodPost "/projects?select=id,name,clients{id,name}" [("Prefer", "return=representation")]
          [str|{"id":6,"name":"New Project","client_id":2}|] `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|{"id":6,"name":"New Project","clients":{"id":2,"name":"Apple"}}|]
          , matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8", "Location" <:> "/projects?id=eq.6"]
          }


    context "with no pk supplied" $ do
      context "into a table with auto-incrementing pk" $
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

      context "into a table with no pk" $ do
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

        it "can insert in tables with no select privileges" $ do
          p <- request methodPost "/insertonly"
                       [("Prefer", "return=minimal")]
                       [json| { "v":"some value" } |]
          liftIO $ do
            simpleBody p `shouldBe` ""
            simpleStatus p `shouldBe` created201


        it "can post nulls" $ do
          p <- request methodPost "/no_pk"
                       [("Prefer", "return=representation")]
                       [json| { "a":null, "b":"foo" } |]
          liftIO $ do
            simpleBody p `shouldBe` [json| { "a":null, "b":"foo" } |]
            simpleHeaders p `shouldSatisfy` matchHeader hLocation "/no_pk\\?a=is.null&b=eq.foo"
            simpleStatus p `shouldBe` created201

    context "with compound pk supplied" $
      it "builds response location header appropriately" $
        post "/compound_pk" [json| { "k1":12, "k2":42 } |]
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Nothing,
            matchStatus  = 201,
            matchHeaders = ["Location" <:> "/compound_pk?k1=eq.12&k2=eq.42"]
          }

    context "with invalid json payload" $
      it "fails with 400 and error" $
        post "/simple_pk" "}{ x = 2" `shouldRespondWith` 400

    context "with valid json payload" $
      it "succeeds and returns 201 created" $
        post "/simple_pk" [json| { "k":"k1", "extra":"e1" } |] `shouldRespondWith` 201

    context "attempting to insert a row with the same primary key" $
      it "fails returning a 409 Conflict" $
        post "/simple_pk" [json| { "k":"k1", "extra":"e1" } |] `shouldRespondWith` 409

    context "attempting to insert a row with conflicting unique constraint" $
      it "fails returning a 409 Conflict" $
        post "/withUnique"  [json| { "uni":"nodup", "extra":"e2" } |] `shouldRespondWith` 409

    context "jsonb" $ do
      it "serializes nested object" $ do
        let inserted = [json| { "data": { "foo":"bar" } } |]
        request methodPost "/json"
                     [("Prefer", "return=representation")]
                     inserted
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just inserted
          , matchStatus  = 201
          , matchHeaders = ["Location" <:> [str|/json?data=eq.{"foo":"bar"}|]]
          }

      it "serializes nested array" $ do
        let inserted = [json| { "data": [1,2,3] } |]
        request methodPost "/json"
                     [("Prefer", "return=representation")]
                     inserted
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just inserted
          , matchStatus  = 201
          , matchHeaders = ["Location" <:> [str|/json?data=eq.[1,2,3]|]]
          }

  describe "CSV insert" $ do

    context "disparate csv types" $
      it "succeeds with multipart response" $ do
        pendingWith "Decide on what to do with CSV insert"
        let inserted = [str|integer,double,varchar,boolean,date,money,enum
            |13,3.14159,testing!,false,1900-01-01,$3.99,foo
            |12,0.1,a string,true,1929-10-01,12,bar
            |]
        request methodPost "/menagerie" [("Content-Type", "text/csv"), ("Accept", "text/csv"), ("Prefer", "return=representation")] inserted

           `shouldRespondWith` ResponseMatcher {
             matchBody    = Just inserted
           , matchStatus  = 201
           , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8"]
           }

    context "requesting full representation" $ do
      it "returns full details of inserted record" $
        request methodPost "/no_pk"
                     [("Content-Type", "text/csv"), ("Accept", "text/csv"),  ("Prefer", "return=representation")]
                     "a,b\nbar,baz"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just "a,b\nbar,baz"
          , matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8",
                            "Location" <:> "/no_pk?a=eq.bar&b=eq.baz"]
          }

      it "can post nulls" $
        request methodPost "/no_pk"
                     [("Content-Type", "text/csv"), ("Accept", "text/csv"), ("Prefer", "return=representation")]
                     "a,b\nNULL,foo"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just "a,b\n,foo"
          , matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8",
                            "Location" <:> "/no_pk?a=is.null&b=eq.foo"]
          }


    context "with wrong number of columns" $
      it "fails for too few" $ do
        p <- request methodPost "/no_pk" [("Content-Type", "text/csv")] "a,b\nfoo,bar\nbaz"
        liftIO $ simpleStatus p `shouldBe` badRequest400

    context "with unicode values" $
      it "succeeds and returns usable location header" $ do
        let payload = [json| { "a":"圍棋", "b":"￥" } |]
        p <- request methodPost "/no_pk"
                     [("Prefer", "return=representation")]
                     payload
        liftIO $ do
          simpleBody p `shouldBe` payload
          simpleStatus p `shouldBe` created201

        let Just location = lookup hLocation $ simpleHeaders p
        r <- get location
        liftIO $ simpleBody r `shouldBe` "["<>payload<>"]"


  describe "Putting record" $ do

    context "to unknown uri" $
      it "gives a 404" $ do
        pendingWith "Decide on PUT usefullness"
        request methodPut "/fake" []
          [json| { "real": false } |]
            `shouldRespondWith` 404

    context "to a known uri" $ do
      context "without a fully-specified primary key" $
        it "is not an allowed operation" $ do
          pendingWith "Decide on PUT usefullness"
          request methodPut "/compound_pk?k1=eq.12" []
            [json| { "k1":12, "k2":42 } |]
              `shouldRespondWith` 405

      context "with a fully-specified primary key" $ do

        context "not specifying every column in the table" $
          it "is rejected for lack of idempotence" $ do
            pendingWith "Decide on PUT usefullness"
            request methodPut "/compound_pk?k1=eq.12&k2=eq.42" []
              [json| { "k1":12, "k2":42 } |]
                `shouldRespondWith` 400

        context "specifying every column in the table" $ do
          it "can create a new record" $ do
            pendingWith "Decide on PUT usefullness"
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
            pendingWith "Decide on PUT usefullness"
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

      context "with an auto-incrementing primary key"$

        it "succeeds with 204" $ do
          pendingWith "Decide on PUT usefullness"
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

    context "to unknown uri" $
      it "gives a 404" $
        request methodPatch "/fake" []
          [json| { "real": false } |]
            `shouldRespondWith` 404

    context "on an empty table" $
      it "indicates no records found to update" $
        request methodPatch "/empty_table" []
          [json| { "extra":20 } |]
            `shouldRespondWith` 404

    context "in a nonempty table" $ do
      it "can update a single item" $ do
        g <- get "/items?id=eq.42"
        liftIO $ simpleHeaders g
          `shouldSatisfy` matchHeader "Content-Range" "\\*/0"
        request methodPatch "/items?id=eq.2" []
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

      it "can set a column to NULL" $ do
        _ <- post "/no_pk" [json| { a: "keepme", b: "nullme" } |]
        _ <- request methodPatch "/no_pk?b=eq.nullme" [] [json| { b: null } |]
        get "/no_pk?a=eq.keepme" `shouldRespondWith`
          [json| [{ a: "keepme", b: null }] |]

      it "can update based on a computed column" $
        request methodPatch
          "/items?always_true=eq.false"
          [("Prefer", "return=representation")]
          [json| { id: 100 } |]
          `shouldRespondWith` 404
      it "can provide a representation" $ do
        _ <- post "/items"
          [json| { id: 1 } |]
        request methodPatch
          "/items?id=eq.1"
          [("Prefer", "return=representation")]
          [json| { id: 99 } |]
          `shouldRespondWith` [json| [{id:99}] |]

      it "can set a json column to escaped value" $ do
        _ <- post "/json" [json| { data: {"escaped":"bar"} } |]
        request methodPatch "/json?data->>escaped=eq.bar"
                     [("Prefer", "return=representation")]
                     [json| { "data": { "escaped":" \"bar" } } |]
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [json| [{ "data": { "escaped":" \"bar" } }] |]
          , matchStatus  = 200
          , matchHeaders = []
          }

    context "with unicode values" $
      it "succeeds and returns values intact" $ do
        void $ request methodPost "/no_pk" []
          [json| { "a":"patchme", "b":"patchme" } |]
        let payload = [json| { "a":"圍棋", "b":"￥" } |]
        p <- request methodPatch "/no_pk?a=eq.patchme&b=eq.patchme"
          [("Prefer", "return=representation")] payload
        liftIO $ do
          simpleBody p `shouldBe` "["<>payload<>"]"
          simpleStatus p `shouldBe` ok200

  describe "Row level permission" $
    it "set user_id when inserting rows" $ do
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
      _ <- post "/postgrest/users" [json| { "id":"jdoe", "pass": "1234", "role": "postgrest_test_author" } |]
      _ <- post "/postgrest/users" [json| { "id":"jroe", "pass": "1234", "role": "postgrest_test_author" } |]

      p1 <- request methodPost "/authors_only"
        [ auth, ("Prefer", "return=representation") ]
        [json| { "secret": "nyancat" } |]
      liftIO $ do
          simpleBody p1 `shouldBe` [str|{"owner":"jdoe","secret":"nyancat"}|]
          simpleStatus p1 `shouldBe` created201

      p2 <- request methodPost "/authors_only"
        -- jwt token for jroe
        [ authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqcm9lIn0.YuF_VfmyIxWyuceT7crnNKEprIYXsJAyXid3rjPjIow", ("Prefer", "return=representation") ]
        [json| { "secret": "lolcat", "owner": "hacker" } |]
      liftIO $ do
          simpleBody p2 `shouldBe` [str|{"owner":"jroe","secret":"lolcat"}|]
          simpleStatus p2 `shouldBe` created201
