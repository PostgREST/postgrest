module Feature.InsertSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.Matcher (bodyEquals)
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

import PostgREST.Types (PgVersion, pgVersion112)

spec :: PgVersion -> SpecWith Application
spec actualPgVersion = do
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
          [json| [{
            "integer": 14, "double": 3.14159, "varchar": "testing!"
          , "boolean": false, "date": "1900-01-01", "money": "$3.99"
          , "enum": "foo"
          }] |] `shouldRespondWith` [str|[{"integer":14,"varchar":"testing!"}]|]
          { matchStatus  = 201
          , matchHeaders = [matchContentTypeJson]
          }

    context "non uniform json array" $ do
      it "rejects json array that isn't exclusivily composed of objects" $
        post "/articles"
             [json| [{"id": 100, "body": "xxxxx"}, 123, "xxxx", {"id": 111, "body": "xxxx"}] |]
        `shouldRespondWith`
             [json| {"message":"All object keys must match"} |]
             { matchStatus  = 400
             , matchHeaders = [matchContentTypeJson]
             }

      it "rejects json array that has objects with different keys" $
        post "/articles"
             [json| [{"id": 100, "body": "xxxxx"}, {"id": 111, "body": "xxxx", "owner": "me"}] |]
        `shouldRespondWith`
             [json| {"message":"All object keys must match"} |]
             { matchStatus  = 400
             , matchHeaders = [matchContentTypeJson]
             }

    context "requesting full representation" $ do
      it "includes related data after insert" $
        request methodPost "/projects?select=id,name,clients(id,name)"
                [("Prefer", "return=representation"), ("Prefer", "count=exact")]
          [str|{"id":6,"name":"New Project","client_id":2}|] `shouldRespondWith` [str|[{"id":6,"name":"New Project","clients":{"id":2,"name":"Apple"}}]|]
          { matchStatus  = 201
          , matchHeaders = [ matchContentTypeJson
                           , "Location" <:> "/projects?id=eq.6"
                           , "Content-Range" <:> "*/1" ]
          }

      it "can rename and cast the selected columns" $
        request methodPost "/projects?select=pId:id::text,pName:name,cId:client_id::text"
                [("Prefer", "return=representation")]
          [str|{"id":7,"name":"New Project","client_id":2}|] `shouldRespondWith`
          [str|[{"pId":"7","pName":"New Project","cId":"2"}]|]
          { matchStatus  = 201
          , matchHeaders = [ matchContentTypeJson
                           , "Location" <:> "/projects?id=eq.7"
                           , "Content-Range" <:> "*/*" ]
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
          `shouldRespondWith`
          [json|{"hint":null,"details":"Failing row contains (null, foo).","code":"23502","message":"null value in column \"k\" violates not-null constraint"}|]
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }

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
            simpleBody p `shouldBe` [json| [{ "a":"bar", "b":"baz" }] |]
            simpleHeaders p `shouldSatisfy` matchHeader hLocation "/no_pk\\?a=eq.bar&b=eq.baz"
            simpleStatus p `shouldBe` created201

        it "returns empty array when no items inserted, and return=rep" $ do
          p <- request methodPost "/no_pk"
                       [("Prefer", "return=representation")]
                       [json| [] |]
          liftIO $ do
            simpleBody p `shouldBe` [json| [] |]
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
            simpleBody p `shouldBe` [json| [{ "a":null, "b":"foo" }] |]
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
          JSON.decode (simpleBody p) `shouldBe` Just [expectedObj]
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
        post "/simple_pk" "}{ x = 2"
        `shouldRespondWith`
        [json|{"message":"Error in $: Failed reading: not a valid json value"}|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

    context "with valid json payload" $
      it "succeeds and returns 201 created" $
        post "/simple_pk" [json| { "k":"k1", "extra":"e1" } |] `shouldRespondWith` 201

    context "attempting to insert a row with the same primary key" $
      it "fails returning a 409 Conflict" $
        post "/simple_pk" [json| { "k":"k1", "extra":"e1" } |]
          `shouldRespondWith`
          [json|{"hint":null,"details":"Key (k)=(k1) already exists.","code":"23505","message":"duplicate key value violates unique constraint \"contacts_pkey\""}|]
          { matchStatus  = 409
          , matchHeaders = [matchContentTypeJson]
          }

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
          `shouldRespondWith` [str|[{"data":{"foo":"bar"}}]|]
          { matchStatus  = 201
          , matchHeaders = ["Location" <:> location]
          }

      it "serializes nested array" $ do
        let inserted = [json| { "data": [1,2,3] } |]
            location = "/json?data=eq.%5B1%2C2%2C3%5D"
        request methodPost "/json"
                     [("Prefer", "return=representation")]
                     inserted
          `shouldRespondWith` [str|[{"data":[1,2,3]}]|]
          { matchStatus  = 201
          , matchHeaders = ["Location" <:> location]
          }

    context "empty objects" $ do
      it "successfully inserts a row with all-default columns" $ do
        post "/items" "{}" `shouldRespondWith` ""
          { matchStatus  = 201
          , matchHeaders = []
          }
        post "/items" "[{}]" `shouldRespondWith` ""
          { matchStatus  = 201
          , matchHeaders = []
          }

      it "successfully inserts two rows with all-default columns" $
        post "/items" "[{}, {}]" `shouldRespondWith` ""
          { matchStatus  = 201
          , matchHeaders = []
          }

    context "table with limited privileges" $ do
      it "succeeds if correct select is applied" $
        request methodPost "/limited_article_stars?select=article_id,user_id" [("Prefer", "return=representation")]
          [json| {"article_id": 2, "user_id": 1} |] `shouldRespondWith` [str|[{"article_id":2,"user_id":1}]|]
          { matchStatus  = 201
          , matchHeaders = []
          }
      it "fails if more columns are selected" $
        request methodPost "/limited_article_stars?select=article_id,user_id,created_at" [("Prefer", "return=representation")]
          [json| {"article_id": 2, "user_id": 2} |] `shouldRespondWith` (
        if actualPgVersion >= pgVersion112 then
        [str|{"hint":null,"details":null,"code":"42501","message":"permission denied for view limited_article_stars"}|]
           else
        [str|{"hint":null,"details":null,"code":"42501","message":"permission denied for relation limited_article_stars"}|]
                                                                        )
          { matchStatus  = 401
          , matchHeaders = []
          }
      it "fails if select is not specified" $
        request methodPost "/limited_article_stars" [("Prefer", "return=representation")]
          [json| {"article_id": 3, "user_id": 1} |] `shouldRespondWith` (
        if actualPgVersion >= pgVersion112 then
        [str|{"hint":null,"details":null,"code":"42501","message":"permission denied for view limited_article_stars"}|]
           else
        [str|{"hint":null,"details":null,"code":"42501","message":"permission denied for relation limited_article_stars"}|]
                                                                        )
          { matchStatus  = 401
          , matchHeaders = []
          }

    context "POST with ?columns parameter" $ do
      it "ignores json keys not included in ?columns" $ do
        request methodPost "/articles?columns=id,body" [("Prefer", "return=representation")]
          [json| {"id": 200, "body": "xxx", "smth": "here", "other": "stuff", "fake_id": 13} |] `shouldRespondWith`
          [json|[{"id": 200, "body": "xxx", "owner": "postgrest_test_anonymous"}]|]
          { matchStatus  = 201
          , matchHeaders = [] }
        request methodPost "/articles?columns=id,body&select=id,body" [("Prefer", "return=representation")]
          [json| [
            {"id": 201, "body": "yyy", "smth": "here", "other": "stuff", "fake_id": 13},
            {"id": 202, "body": "zzz", "garbage": "%%$&", "kkk": "jjj"},
            {"id": 203, "body": "aaa", "hey": "ho"} ]|] `shouldRespondWith`
          [json|[
            {"id": 201, "body": "yyy"},
            {"id": 202, "body": "zzz"},
            {"id": 203, "body": "aaa"} ]|]
          { matchStatus  = 201
          , matchHeaders = [] }

      -- TODO parse columns error message needs to be improved
      it "disallows blank ?columns" $
        post "/articles?columns="
          [json|[
            {"id": 204, "body": "yyy"},
            {"id": 205, "body": "zzz"}]|]
          `shouldRespondWith`
          [json|  {"details":"unexpected end of input expecting field name (* or [a..z0..9_])","message":"\"failed to parse columns parameter ()\" (line 1, column 1)"} |]
          { matchStatus  = 400
          , matchHeaders = []
          }

      it "disallows array elements that are not json objects" $
        post "/articles?columns=id,body"
          [json|[
            {"id": 204, "body": "yyy"},
            333,
            "asdf",
            {"id": 205, "body": "zzz"}]|] `shouldRespondWith`
          [json|{
              "code": "22023",
              "details": null,
              "hint": null,
              "message": "argument of json_populate_recordset must be an array of objects"}|]
          { matchStatus  = 400
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
           `shouldRespondWith` ResponseMatcher
           { matchStatus  = 201
           , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8"]
           , matchBody = bodyEquals inserted
           }

    context "requesting full representation" $ do
      it "returns full details of inserted record" $
        request methodPost "/no_pk"
                     [("Content-Type", "text/csv"), ("Accept", "text/csv"),  ("Prefer", "return=representation")]
                     "a,b\nbar,baz"
          `shouldRespondWith` "a,b\nbar,baz"
          { matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8",
                            "Location" <:> "/no_pk?a=eq.bar&b=eq.baz"]
          }

      it "can post nulls" $
        request methodPost "/no_pk"
                     [("Content-Type", "text/csv"), ("Accept", "text/csv"), ("Prefer", "return=representation")]
                     "a,b\nNULL,foo"
          `shouldRespondWith` "a,b\n,foo"
          { matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8",
                            "Location" <:> "/no_pk?a=is.null&b=eq.foo"]
          }

      it "only returns the requested column header with its associated data" $
        request methodPost "/projects?select=id"
                     [("Content-Type", "text/csv"), ("Accept", "text/csv"), ("Prefer", "return=representation")]
                     "id,name,client_id\n8,Xenix,1\n9,Windows NT,1"
          `shouldRespondWith` "id\n8\n9"
          { matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8",
                            "Content-Range" <:> "*/*"]
          }

    context "with wrong number of columns" $
      it "fails for too few" $
        request methodPost "/no_pk" [("Content-Type", "text/csv")] "a,b\nfoo,bar\nbaz"
        `shouldRespondWith`
        [json|{"message":"All lines must have same number of fields"}|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

    context "with unicode values" $
      it "succeeds and returns usable location header" $ do
        let payload = [json| { "a":"圍棋", "b":"￥" } |]
        p <- request methodPost "/no_pk"
                     [("Prefer", "return=representation")]
                     payload
        liftIO $ do
          simpleBody p `shouldBe` "["<>payload<>"]"
          simpleStatus p `shouldBe` created201

        let Just location = lookup hLocation $ simpleHeaders p
        r <- get location
        liftIO $ simpleBody r `shouldBe` "["<>payload<>"]"

  describe "Patching record" $ do
    context "to unknown uri" $
      it "indicates no table found by returning 404" $
        request methodPatch "/fake" []
          [json| { "real": false } |]
            `shouldRespondWith` 404

    context "on an empty table" $
      it "indicates no records found to update by returning 404" $
        request methodPatch "/empty_table" []
          [json| { "extra":20 } |]
          `shouldRespondWith` ""
          { matchStatus  = 404,
            matchHeaders = []
          }

    context "in a nonempty table" $ do
      it "can update a single item" $ do
        g <- get "/items?id=eq.42"
        liftIO $ simpleHeaders g
          `shouldSatisfy` matchHeader "Content-Range" "\\*/\\*"
        p <- request methodPatch "/items?id=eq.2" [] [json| { "id":42 } |]
        pure p `shouldRespondWith` ""
          { matchStatus  = 204,
            matchHeaders = ["Content-Range" <:> "0-0/*"]
          }
        liftIO $ lookup hContentType (simpleHeaders p) `shouldBe` Nothing

        -- check it really got updated
        g' <- get "/items?id=eq.42"
        liftIO $ simpleHeaders g'
          `shouldSatisfy` matchHeader "Content-Range" "0-0/\\*"
        -- put value back for other tests
        void $ request methodPatch "/items?id=eq.42" [] [json| { "id":2 } |]

      it "returns empty array when no rows updated and return=rep" $
        request methodPatch "/items?id=eq.999999"
          [("Prefer", "return=representation")] [json| { "id":999999 } |]
          `shouldRespondWith` "[]"
          {
            matchStatus  = 404,
            matchHeaders = []
          }

      it "gives a 404 when no rows updated" $
        request methodPatch "/items?id=eq.99999999" []
          [json| { "id": 42 } |]
            `shouldRespondWith` 404

      it "returns updated object as array when return=rep" $
        request methodPatch "/items?id=eq.2"
          [("Prefer", "return=representation")] [json| { "id":2 } |]
          `shouldRespondWith` [str|[{"id":2}]|]
          { matchStatus  = 200,
            matchHeaders = ["Content-Range" <:> "0-0/*"]
          }

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
          { matchHeaders = [matchContentTypeJson] }

      context "filtering by a computed column" $ do
        it "is successful" $
          request methodPatch
            "/items?is_first=eq.true"
            [("Prefer", "return=representation")]
            [json| { id: 100 } |]
            `shouldRespondWith` [json| [{ id: 100 }] |]
            { matchStatus  = 200,
              matchHeaders = [matchContentTypeJson, "Content-Range" <:> "0-0/*"]
            }

        it "indicates no records updated by returning 404" $
          request methodPatch
            "/items?always_true=eq.false"
            [("Prefer", "return=representation")]
            [json| { id: 100 } |]
            `shouldRespondWith` "[]"
            { matchStatus  = 404,
              matchHeaders = []
            }

      it "can provide a representation" $ do
        _ <- post "/items"
          [json| { id: 1 } |]
        request methodPatch
          "/items?id=eq.1"
          [("Prefer", "return=representation")]
          [json| { id: 99 } |]
          `shouldRespondWith` [json| [{id:99}] |]
          { matchHeaders = [matchContentTypeJson] }
        -- put value back for other tests
        void $ request methodPatch "/items?id=eq.99" [] [json| { "id":1 } |]

      it "makes no updates and returns 204, when patching with an empty json object/array" $ do
        request methodPatch "/items" [] [json| {} |]
          `shouldRespondWith` ""
          {
            matchStatus  = 204,
            matchHeaders = ["Content-Range" <:> "*/*"]
          }

        request methodPatch "/items" [] [json| [] |]
          `shouldRespondWith` ""
          {
            matchStatus  = 204,
            matchHeaders = ["Content-Range" <:> "*/*"]
          }

        request methodPatch "/items" [] [json| [{}] |]
          `shouldRespondWith` ""
          {
            matchStatus  = 204,
            matchHeaders = ["Content-Range" <:> "*/*"]
          }

      it "makes no updates and and returns 200, when patching with an empty json object and return=rep" $
        request methodPatch "/items" [("Prefer", "return=representation")] [json| {} |]
          `shouldRespondWith` "[]"
          {
            matchStatus  = 200,
            matchHeaders = ["Content-Range" <:> "*/*"]
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

    context "PATCH with ?columns parameter" $ do
      it "ignores json keys not included in ?columns" $
        request methodPatch "/articles?id=eq.200&columns=body" [("Prefer", "return=representation")]
          [json| {"body": "Some real content", "smth": "here", "other": "stuff", "fake_id": 13} |] `shouldRespondWith`
          [json|[{"id": 200, "body": "Some real content", "owner": "postgrest_test_anonymous"}]|]
          { matchStatus  = 200
          , matchHeaders = [] }

      it "ignores json keys and gives 404 if no record updated" $
        request methodPatch "/articles?id=eq.2001&columns=body" [("Prefer", "return=representation")]
          [json| {"body": "Some real content", "smth": "here", "other": "stuff", "fake_id": 13} |] `shouldRespondWith` 404

  describe "Row level permission" $
    it "set user_id when inserting rows" $ do
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.B-lReuGNDwAlU1GOC476MlO0vAt9JNoHIlxg2vwMaO0"
      _ <- post "/postgrest/users" [json| { "id":"jdoe", "pass": "1234", "role": "postgrest_test_author" } |]
      _ <- post "/postgrest/users" [json| { "id":"jroe", "pass": "1234", "role": "postgrest_test_author" } |]

      p1 <- request methodPost "/authors_only"
        [ auth, ("Prefer", "return=representation") ]
        [json| { "secret": "nyancat" } |]
      liftIO $ do
        simpleBody p1 `shouldBe` [str|[{"owner":"jdoe","secret":"nyancat"}]|]
        simpleStatus p1 `shouldBe` created201

      p2 <- request methodPost "/authors_only"
        -- jwt token for jroe
        [ authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqcm9lIn0.2e7mx0U4uDcInlbJVOBGlrRufwqWLINDIEDC1vS0nw8", ("Prefer", "return=representation") ]
        [json| { "secret": "lolcat", "owner": "hacker" } |]
      liftIO $ do
        simpleBody p2 `shouldBe` [str|[{"owner":"jroe","secret":"lolcat"}]|]
        simpleStatus p2 `shouldBe` created201
