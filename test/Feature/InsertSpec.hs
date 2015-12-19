module Feature.InsertSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Test (SResponse(simpleBody,simpleHeaders,simpleStatus))

import Hasql as H
import Hasql.Postgres as P

import SpecHelper
import PostgREST.Types (DbStructure(..))

import qualified Data.Aeson as JSON
import Data.Maybe (fromJust)
import Text.Heredoc
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Control.Monad (replicateM_)

import TestTypes(IncPK(..), CompoundPK(..))

spec :: DbStructure -> H.Pool P.Postgres -> Spec
spec struct pool = beforeAll_ resetDb $ around (withApp cfgDefault struct pool) $ do
  describe "Posting new record" $ do
    after_ (clearTable "menagerie") . context "disparate csv types" $ do
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
          , matchHeaders = ["Content-Type" <:> "application/json"]
          }

      it "includes related data after insert" $
        request methodPost "/projects?select=id,name,clients{id,name}" [("Prefer", "return=representation")]
          [str|{"id":6,"name":"New Project","client_id":2}|] `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|{"id":6,"name":"New Project","clients":{"id":2,"name":"Apple"}}|]
          , matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "application/json", "Location" <:> "/projects?id=eq.6"]
          }


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
        post "/simple_pk" "}{ x = 2" `shouldRespondWith` 400

    context "jsonb" . after_ (clearTable "json") $ do
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

    after_ (clearTable "menagerie") . context "disparate csv types" $
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
           , matchHeaders = ["Content-Type" <:> "text/csv"]
           }

    after_ (clearTable "no_pk") . context "requesting full representation" $ do
      it "returns full details of inserted record" $
        request methodPost "/no_pk"
                     [("Content-Type", "text/csv"), ("Accept", "text/csv"),  ("Prefer", "return=representation")]
                     "a,b\nbar,baz"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just "a,b\nbar,baz"
          , matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "text/csv",
                            "Location" <:> "/no_pk?a=eq.bar&b=eq.baz"]
          }

      it "can post nulls" $
        request methodPost "/no_pk"
                     [("Content-Type", "text/csv"), ("Accept", "text/csv"), ("Prefer", "return=representation")]
                     "a,b\nNULL,foo"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just "a,b\n,foo"
          , matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "text/csv",
                            "Location" <:> "/no_pk?a=is.null&b=eq.foo"]
          }


    after_ (clearTable "no_pk") . context "with wrong number of columns" $
      it "fails for too few" $ do
        p <- request methodPost "/no_pk" [("Content-Type", "text/csv")] "a,b\nfoo,bar\nbaz"
        liftIO $ simpleStatus p `shouldBe` badRequest400

  describe "Patching record" $ do

    context "to unkonwn uri" $
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
