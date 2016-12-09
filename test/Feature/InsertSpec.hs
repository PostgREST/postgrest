module Feature.InsertSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Test (SResponse(simpleBody,simpleHeaders,simpleStatus))

import SpecHelper

import qualified Data.Aeson as JSON
import Data.List (lookup)
import Data.Maybe (fromJust)
import Text.Heredoc
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Control.Monad (replicateM_, void)

import TestTypes(IncPK(..), CompoundPK(..))
import Network.Wai (Application)

import Protolude hiding (get)

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
          -- should not have content type set when body is empty
          lookup hContentType (simpleHeaders p) `shouldBe` Nothing

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
        request methodPost "/projects?select=id,name,clients{id,name}"
                [("Prefer", "return=representation"), ("Prefer", "count=exact")]
          [str|{"id":6,"name":"New Project","client_id":2}|] `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|{"id":6,"name":"New Project","clients":{"id":2,"name":"Apple"}}|]
          , matchStatus  = 201
          , matchHeaders = [ "Content-Type" <:> "application/json; charset=utf-8"
                           , "Location" <:> "/projects?id=eq.6"
                           , "Content-Range" <:> "*/1" ]
          }

    context "from an html form" $
      it "accepts disparate json types" $ do
        p <- request methodPost "/menagerie"
               [("Content-Type", "application/x-www-form-urlencoded")]
               ("integer=7&double=2.71828&varchar=forms+are+fun&" <>
                "boolean=false&date=1900-01-01&money=$3.99&enum=foo")
        liftIO $ do
          simpleBody p `shouldBe` ""
          simpleStatus p `shouldBe` created201

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
      it "builds response location header appropriately" $ do
        let inserted    = [json| { "k1":12, "k2":"Rock & R+ll" } |]
            expectedObj = CompoundPK 12 "Rock & R+ll" Nothing
            expectedLoc = "/compound_pk?k1=eq.12&k2=eq.Rock%20%26%20R%2Bll"
        p <- request methodPost "/compound_pk"
                     [("Prefer", "return=representation")]
                     inserted
        liftIO $ do
          JSON.decode (simpleBody p) `shouldBe` Just expectedObj
          simpleStatus p `shouldBe` created201
          lookup hLocation (simpleHeaders p) `shouldBe` Just expectedLoc

        r <- get expectedLoc
        liftIO $ do
          JSON.decode (simpleBody r) `shouldBe` Just [expectedObj]
          simpleStatus r `shouldBe` ok200

    context "with bulk insert" $
      it "returns 201 but no location header" $ do
        let bulkData = [json| [ {"k1":21, "k2":"hello world"}
                              , {"k1":22, "k2":"bye for now"}]
                            |]
        p <- request methodPost "/compound_pk" [] bulkData
        liftIO $ do
          simpleStatus p `shouldBe` created201
          lookup hLocation (simpleHeaders p) `shouldBe` Nothing

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
            location = "/json?data=eq.%7B%22foo%22%3A%22bar%22%7D"
        request methodPost "/json"
                     [("Prefer", "return=representation")]
                     inserted
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just inserted
          , matchStatus  = 201
          , matchHeaders = ["Location" <:> location]
          }

      it "serializes nested array" $ do
        let inserted = [json| { "data": [1,2,3] } |]
            location = "/json?data=eq.%5B1%2C2%2C3%5D"
        request methodPost "/json"
                     [("Prefer", "return=representation")]
                     inserted
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just inserted
          , matchStatus  = 201
          , matchHeaders = ["Location" <:> location]
          }

    context "empty object" $
      it "successfully populates table with all-default columns" $
        post "/items" "{}" `shouldRespondWith` ResponseMatcher {
            matchBody    = Just ""
          , matchStatus  = 201
          , matchHeaders = []
          }
    context "table with limited privileges" $ do
      it "succeeds if correct select is applied" $
        request methodPost "/limited_article_stars?select=article_id,user_id" [("Prefer", "return=representation")]
          [json| {"article_id": 2, "user_id": 1} |] `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|{"article_id":2,"user_id":1}|]
          , matchStatus  = 201
          , matchHeaders = []
          }
      it "fails if more columns are selected" $
        request methodPost "/limited_article_stars?select=article_id,user_id,created_at" [("Prefer", "return=representation")]
          [json| {"article_id": 2, "user_id": 2} |] `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|{"hint":null,"details":null,"code":"42501","message":"permission denied for relation limited_article_stars"}|]
          , matchStatus  = 401
          , matchHeaders = []
          }
      it "fails if select is not specified" $
        request methodPost "/limited_article_stars" [("Prefer", "return=representation")]
          [json| {"article_id": 3, "user_id": 1} |] `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|{"hint":null,"details":null,"code":"42501","message":"permission denied for relation limited_article_stars"}|]
          , matchStatus  = 401
          , matchHeaders = []
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
          `shouldSatisfy` matchHeader "Content-Range" "\\*/\\*"
        p <- request methodPatch "/items?id=eq.2" [] [json| { "id":42 } |]
        pure p `shouldRespondWith` ResponseMatcher {
            matchBody    = Nothing,
            matchStatus  = 204,
            matchHeaders = ["Content-Range" <:> "0-0/*"]
          }
        liftIO $
          lookup hContentType (simpleHeaders p) `shouldBe` Nothing

        g' <- get "/items?id=eq.42"
        liftIO $ simpleHeaders g'
          `shouldSatisfy` matchHeader "Content-Range" "0-0/\\*"

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
          `shouldSatisfy` matchHeader "Content-Range" "0-9/\\*"

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

    context "in a table" $ do
      it "can provide a singular representation when updating one entity" $ do
        _ <- post "/addresses" [json| { id: 97, address: "A Street" } |]
        p <- request methodPatch
          "/addresses?id=eq.97"
          [("Prefer", "return=representation,plurality=singular")]
          [json| { address: "B Street" } |]
        liftIO $ simpleBody p `shouldBe` [str|{"id":97,"address":"B Street"}|]
      it "raises an error when attempting to update multiple entities with plurality=singular" $ do
        _ <- post "/addresses" [json| { id: 98, address: "xxx" } |]
        _ <- post "/addresses" [json| { id: 99, address: "yyy" } |]
        p <- request methodPatch
          "/addresses?id=gt.0"
          [("Prefer", "return=representation,plurality=singular")]
          [json| { address: "zzz" } |]
        liftIO $ simpleStatus p `shouldBe` status400
      it "can provide a singular representation when creating one entity" $ do
        p <- request methodPost
          "/addresses"
          [("Prefer", "return=representation,plurality=singular")]
          [json| [ { id: 100, address: "xxx" } ] |]
        liftIO $ simpleBody p `shouldBe` [str|{"id":100,"address":"xxx"}|]
      it "raises an error when attempting to create multiple entities with plurality=singular" $ do
        p <- request methodPost
          "/addresses"
          [("Prefer", "return=representation,plurality=singular")]
          [json| [ { id: 100, address: "xxx" }, { id: 101, address: "xxx" } ] |]
        liftIO $ simpleStatus p `shouldBe` status400

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
