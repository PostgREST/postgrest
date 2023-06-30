module Feature.Query.InsertSpec where

import Data.List              (lookup)
import Network.Wai            (Application)
import Network.Wai.Test       (SResponse (simpleHeaders))
import Test.Hspec             hiding (pendingWith)
import Test.Hspec.Wai.Matcher (bodyEquals)

import Network.HTTP.Types
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Text.Heredoc

import PostgREST.Config.PgVersion (PgVersion, pgVersion100,
                                   pgVersion110, pgVersion112,
                                   pgVersion120, pgVersion130,
                                   pgVersion140)

import Protolude  hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec actualPgVersion = do
  describe "Posting new record" $ do
    context "disparate json types" $ do
      it "accepts disparate json types" $ do
        post "/menagerie"
          [json| {
            "integer": 13, "double": 3.14159, "varchar": "testing!"
          , "boolean": false, "date": "1900-01-01", "money": "$3.99"
          , "enum": "foo"
          } |] `shouldRespondWith` ""
          { matchStatus  = 201
            -- should not have content type set when body is empty
          , matchHeaders = [matchHeaderAbsent hContentType]
          }

      it "filters columns in result using &select" $
        request methodPost "/menagerie?select=integer,varchar" [("Prefer", "return=representation")]
          [json| [{
            "integer": 14, "double": 3.14159, "varchar": "testing!"
          , "boolean": false, "date": "1900-01-01", "money": "$3.99"
          , "enum": "foo"
          }] |] `shouldRespondWith` [json|[{"integer":14,"varchar":"testing!"}]|]
          { matchStatus  = 201
          , matchHeaders = [matchContentTypeJson]
          }

      it "ignores &select when return not set or using return=minimal" $ do
        request methodPost "/menagerie?select=integer,varchar"
            []
            [json| [{
              "integer": 15, "double": 3.14159, "varchar": "testing!",
              "boolean": false, "date": "1900-01-01", "money": "$3.99",
              "enum": "foo"
            }] |]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [matchHeaderAbsent hContentType]
            }
        request methodPost "/menagerie?select=integer,varchar"
            [("Prefer", "return=minimal")]
            [json| [{
              "integer": 16, "double": 3.14159, "varchar": "testing!",
              "boolean": false, "date": "1900-01-01", "money": "$3.99",
              "enum": "foo"
            }] |]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [matchHeaderAbsent hContentType]
            }

    context "non uniform json array" $ do
      it "rejects json array that isn't exclusivily composed of objects" $
        post "/articles"
             [json| [{"id": 100, "body": "xxxxx"}, 123, "xxxx", {"id": 111, "body": "xxxx"}] |]
        `shouldRespondWith`
             [json| {"message":"All object keys must match","code":"PGRST102","hint":null,"details":null} |]
             { matchStatus  = 400
             , matchHeaders = [matchContentTypeJson]
             }

      it "rejects json array that has objects with different keys" $
        post "/articles"
             [json| [{"id": 100, "body": "xxxxx"}, {"id": 111, "body": "xxxx", "owner": "me"}] |]
        `shouldRespondWith`
             [json| {"message":"All object keys must match","code":"PGRST102","hint":null,"details":null} |]
             { matchStatus  = 400
             , matchHeaders = [matchContentTypeJson]
             }

    context "requesting full representation" $ do
      it "includes related data after insert" $
        request methodPost "/projects?select=id,name,clients(id,name)"
                [("Prefer", "return=representation"), ("Prefer", "count=exact")]
          [json|{"id":6,"name":"New Project","client_id":2}|] `shouldRespondWith` [json|[{"id":6,"name":"New Project","clients":{"id":2,"name":"Apple"}}]|]
          { matchStatus  = 201
          , matchHeaders = [ matchContentTypeJson
                           , matchHeaderAbsent hLocation
                           , "Content-Range" <:> "*/1" ]
          }

      it "can rename and cast the selected columns" $
        request methodPost "/projects?select=pId:id::text,pName:name,cId:client_id::text"
                [("Prefer", "return=representation")]
          [json|{"id":7,"name":"New Project","client_id":2}|] `shouldRespondWith`
          [json|[{"pId":"7","pName":"New Project","cId":"2"}]|]
          { matchStatus  = 201
          , matchHeaders = [ matchContentTypeJson
                           , matchHeaderAbsent hLocation
                           , "Content-Range" <:> "*/*" ]
          }

      it "should not throw and return location header when selecting without PK" $
        request methodPost "/projects?select=name,client_id" [("Prefer", "return=representation")]
          [json|{"id":10,"name":"New Project","client_id":2}|] `shouldRespondWith`
          [json|[{"name":"New Project","client_id":2}]|]
          { matchStatus  = 201
          , matchHeaders = [ matchContentTypeJson
                           , matchHeaderAbsent hLocation
                           , "Content-Range" <:> "*/*" ]
          }

    context "requesting headers only representation" $ do
      it "should not throw and return location header when selecting without PK" $
        request methodPost "/projects?select=name,client_id"
            [("Prefer", "return=headers-only")]
            [json|{"id":11,"name":"New Project","client_id":2}|]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Location" <:> "/projects?id=eq.11"
                             , "Content-Range" <:> "*/*" ]
            }

      when (actualPgVersion >= pgVersion110) $
        it "should not throw and return location header for partitioned tables when selecting without PK" $
          request methodPost "/car_models"
              [("Prefer", "return=headers-only")]
              [json|{"name":"Enzo","year":2021}|]
            `shouldRespondWith`
              ""
              { matchStatus  = 201
              , matchHeaders = [ matchHeaderAbsent hContentType
                               , "Location" <:> "/car_models?name=eq.Enzo&year=eq.2021"
                               , "Content-Range" <:> "*/*" ]
              }

    context "requesting no representation" $
      it "should not throw and return no location header when selecting without PK" $
        request methodPost "/projects?select=name,client_id"
            []
            [json|{"id":12,"name":"New Project","client_id":2}|]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , matchHeaderAbsent hLocation ]
            }

    context "from an html form" $
      it "accepts disparate json types" $ do
        request methodPost "/menagerie"
            [("Content-Type", "application/x-www-form-urlencoded")]
             ("integer=7&double=2.71828&varchar=forms+are+fun&" <>
              "boolean=false&date=1900-01-01&money=$3.99&enum=foo")
          `shouldRespondWith`
            ""
            { matchStatus = 201
            , matchHeaders = [ matchHeaderAbsent hContentType ]
            }

    context "with no pk supplied" $ do
      context "into a table with auto-incrementing pk" $
        it "succeeds with 201 and location header" $ do
          -- reset pk sequence first to make test repeatable
          request methodPost "/rpc/reset_sequence"
              [("Prefer", "tx=commit")]
              [json|{"name": "auto_incrementing_pk_id_seq", "value": 2}|]
            `shouldRespondWith`
              ""
              { matchStatus = 204
              , matchHeaders = [ matchHeaderAbsent hContentType ]
              }

          request methodPost "/auto_incrementing_pk"
              [("Prefer", "return=headers-only")]
              [json| { "non_nullable_string":"not null"} |]
            `shouldRespondWith`
              ""
              { matchStatus  = 201
              , matchHeaders = [ matchHeaderAbsent hContentType
                               , "Location" <:> "/auto_incrementing_pk?id=eq.2" ]
              }

      context "into a table with simple pk" $
        it "fails with 400 and error" $
          post "/simple_pk" [json| { "extra":"foo"} |]
          `shouldRespondWith`
          (if actualPgVersion >= pgVersion130 then
            [json|{"hint":null,"details":"Failing row contains (null, foo).","code":"23502","message":"null value in column \"k\" of relation \"simple_pk\" violates not-null constraint"}|]
           else
            [json|{"hint":null,"details":"Failing row contains (null, foo).","code":"23502","message":"null value in column \"k\" violates not-null constraint"}|]
          )
          { matchStatus  = 400
          , matchHeaders = [matchContentTypeJson]
          }

      context "into a table with no pk" $ do
        it "succeeds with 201 but no location header" $ do
          post "/no_pk"
              [json| { "a":"foo", "b":"bar" } |]
            `shouldRespondWith`
              ""
              { matchStatus  = 201
              , matchHeaders = [ matchHeaderAbsent hContentType
                               , matchHeaderAbsent hLocation ]
              }

        it "returns full details of inserted record if asked" $ do
          request methodPost "/no_pk"
              [("Prefer", "return=representation")]
              [json| { "a":"bar", "b":"baz" } |]
            `shouldRespondWith`
              [json| [{ "a":"bar", "b":"baz" }] |]
              { matchStatus  = 201
              , matchHeaders = [matchHeaderAbsent hLocation]
              }

        it "returns empty array when no items inserted, and return=rep" $ do
          request methodPost "/no_pk"
              [("Prefer", "return=representation")]
              [json| [] |]
            `shouldRespondWith`
              [json| [] |]
              { matchStatus = 201 }

        it "can post nulls" $ do
          request methodPost "/no_pk"
              [("Prefer", "return=representation")]
              [json| { "a":null, "b":"foo" } |]
            `shouldRespondWith`
              [json| [{ "a":null, "b":"foo" }] |]
              { matchStatus  = 201
              , matchHeaders = [matchHeaderAbsent hLocation]
              }

    context "with compound pk supplied" $
      it "builds response location header appropriately" $ do
        request methodPost "/compound_pk"
            [("Prefer", "return=representation")]
            [json| { "k1":12, "k2":"Rock & R+ll" } |]
          `shouldRespondWith`
            [json|[ { "k1":12, "k2":"Rock & R+ll", "extra": null } ]|]
            { matchStatus  = 201
            , matchHeaders = [ matchHeaderAbsent hLocation ]
            }

    context "with bulk insert" $
      it "returns 201 but no location header" $ do
        let bulkData = [json| [ {"k1":21, "k2":"hello world"}
                              , {"k1":22, "k2":"bye for now"}]
                            |]
        request methodPost "/compound_pk"
            []
            bulkData
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , matchHeaderAbsent hLocation ]
            }

    context "with invalid json payload" $
      it "fails with 400 and error" $
        post "/simple_pk" "}{ x = 2"
        `shouldRespondWith`
        [json|{"message":"Error in $: Failed reading: not a valid json value at '}{x=2'","code":"PGRST102","details":null,"hint":null}|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

    context "with no payload" $
      it "fails with 400 and error" $
        post "/simple_pk" ""
        `shouldRespondWith`
        [json|{"message":"Error in $: not enough input","code":"PGRST102","details":null,"hint":null}|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

    context "with valid json payload" $
      it "succeeds and returns 201 created" $
        post "/simple_pk"
            [json| { "k":"k1", "extra":"e1" } |]
          `shouldRespondWith`
            ""
            { matchStatus = 201
            , matchHeaders = [matchHeaderAbsent hContentType]
            }

    context "attempting to insert a row with the same primary key" $
      it "fails returning a 409 Conflict" $
        post "/simple_pk"
            [json| { "k":"xyyx", "extra":"e1" } |]
          `shouldRespondWith`
            [json|{"hint":null,"details":"Key (k)=(xyyx) already exists.","code":"23505","message":"duplicate key value violates unique constraint \"simple_pk_pkey\""}|]
            { matchStatus  = 409 }

    context "attempting to insert a row with conflicting unique constraint" $
      it "fails returning a 409 Conflict" $
        post "/withUnique"  [json| { "uni":"nodup", "extra":"e2" } |] `shouldRespondWith` 409

    context "jsonb" $ do
      it "serializes nested object" $ do
        let inserted = [json| { "data": { "foo":"bar" } } |]
        request methodPost "/json_table"
                     [("Prefer", "return=representation")]
                     inserted
          `shouldRespondWith` [json|[{"data":{"foo":"bar"}}]|]
          { matchStatus  = 201
          }

      it "serializes nested array" $ do
        let inserted = [json| { "data": [1,2,3] } |]
        request methodPost "/json_table"
                     [("Prefer", "return=representation")]
                     inserted
          `shouldRespondWith` [json|[{"data":[1,2,3]}]|]
          { matchStatus  = 201
          }

    context "empty objects" $ do
      it "successfully inserts a row with all-default columns" $ do
        post "/items"
            [json|{}|]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [matchHeaderAbsent hContentType]
            }
        post "/items" "[{}]" `shouldRespondWith` ""
          { matchStatus  = 201
          , matchHeaders = []
          }

      it "successfully inserts two rows with all-default columns" $
        post "/items"
            [json|[{}, {}]|]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [matchHeaderAbsent hContentType]
            }

      it "successfully inserts a row with all-default columns with prefer=rep" $ do
        -- reset pk sequence first to make test repeatable
        request methodPost "/rpc/reset_sequence"
            [("Prefer", "tx=commit")]
            [json|{"name": "items2_id_seq", "value": 20}|]
          `shouldRespondWith`
            ""
            { matchStatus = 204
            , matchHeaders = [ matchHeaderAbsent hContentType ]
            }

        request methodPost "/items2"
            [("Prefer", "return=representation")]
            [json|{}|]
          `shouldRespondWith`
            [json|[{ id: 20 }]|]
            { matchStatus  = 201 }

      it "successfully inserts a row with all-default columns with prefer=rep and &select=" $ do
        -- reset pk sequence first to make test repeatable
        request methodPost "/rpc/reset_sequence"
            [("Prefer", "tx=commit")]
            [json|{"name": "items3_id_seq", "value": 20}|]
          `shouldRespondWith`
            ""
            { matchStatus = 204
            , matchHeaders = [ matchHeaderAbsent hContentType ]
            }

        request methodPost "/items3?select=id"
            [("Prefer", "return=representation")]
            [json|{}|]
          `shouldRespondWith` [json|[{ id: 20 }]|]
            { matchStatus  = 201 }

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
          [json|  {"details":"unexpected end of input expecting field name (* or [a..z0..9_$])","message":"\"failed to parse columns parameter ()\" (line 1, column 1)","code":"PGRST100","hint":null} |]
          { matchStatus  = 400
          , matchHeaders = []
          }

      it "disallows ?columns which don't exist" $
        post "/articles?columns=helicopter"
          [json|[
            {"id": 204, "body": "yyy"},
            {"id": 205, "body": "zzz"}]|]
          `shouldRespondWith`
          [json|{"code":"PGRST204","details":null,"hint":null,"message":"Column 'helicopter' of relation 'articles' does not exist"} |]
          { matchStatus  = 400
          , matchHeaders = []
          }

      it "returns missing table error even if also has invalid ?columns" $
        post "/garlic?columns=helicopter"
          [json|[
            {"id": 204, "body": "yyy"},
            {"id": 205, "body": "zzz"}]|]
          `shouldRespondWith`
          [json|{} |]
          { matchStatus  = 404
          , matchHeaders = []
          }

      it "disallows array elements that are not json objects" $
        post "/articles?columns=id,body"
          [json|[
            {"id": 204, "body": "yyy"},
            333,
            "asdf",
            {"id": 205, "body": "zzz"}]|] `shouldRespondWith` 400

      context "apply defaults on missing values" $ do
        -- inserting the array fails on pg 9.6, but the feature should work normally
        when (actualPgVersion >= pgVersion100) $
          it "inserts table default values(field-with_sep) when json keys are undefined" $
            request methodPost "/complex_items?columns=id,name,field-with_sep,arr_data" [("Prefer", "return=representation"), ("Prefer", "missing=default")]
                [json|[
                  {"id": 4, "name": "Vier"},
                  {"id": 5, "name": "Funf", "arr_data": null},
                  {"id": 6, "name": "Sechs", "field-with_sep": 6, "arr_data": "{1,2,3}"}
                ]|]
              `shouldRespondWith`
                [json|[
                  {"id": 4, "name": "Vier", "field-with_sep": 1, "settings":null,"arr_data":null},
                  {"id": 5, "name": "Funf", "field-with_sep": 1, "settings":null,"arr_data":null},
                  {"id": 6, "name": "Sechs", "field-with_sep": 6, "settings":null,"arr_data":[1,2,3]}
                ]|]
                { matchStatus  = 201
                , matchHeaders = ["Preference-Applied" <:> "missing=default"]
                }

        it "inserts view default values(field-with_sep) when json keys are undefined" $
          request methodPost "/complex_items_view?columns=id,name" [("Prefer", "return=representation"), ("Prefer", "missing=default")]
              [json|[
                {"id": 7, "name": "Sieben"},
                {"id": 8}
              ]|]
            `shouldRespondWith`
              [json|[
                {"id": 7, "name": "Sieben", "field-with_sep": 1, "settings":null,"arr_data":null},
                {"id": 8, "name": "Default", "field-with_sep": 1, "settings":null,"arr_data":null}
              ]|]
              { matchStatus  = 201
              , matchHeaders = ["Preference-Applied" <:> "missing=default"]
              }

        it "doesn't insert json duplicate keys(since it uses jsonb)" $
          request methodPost "/tbl_w_json?columns=id,data" [("Prefer", "return=representation"), ("Prefer", "missing=default")]
              [json| { "data": { "a": 1, "a": 2 }, "id": 3 } |]
            `shouldRespondWith`
              [json| [ { "data": { "a": 2 }, "id": 3 } ] |]
              { matchStatus  = 201
              , matchHeaders = ["Preference-Applied" <:> "missing=default"]
              }

        when (actualPgVersion >= pgVersion100) $
          it "inserts a default on a generated by default as identity column" $
            request methodPost "/channels?columns=id,data,slug&select=data,slug" [("Prefer", "return=representation"), ("Prefer", "missing=default")]
                [json| { "slug": "foo" } |]
              `shouldRespondWith`
                [json| [{"data":{"foo": "bar"},"slug":"foo"}] |] -- id 1 was inserted here, we don't get it for idempotence in the tests
                { matchStatus  = 201
                , matchHeaders = ["Preference-Applied" <:> "missing=default"]
                }

        when (actualPgVersion >= pgVersion120) $
          it "fails with a good error message on generated always columns" $
            request methodPost "/foo?columns=a,b" [("Prefer", "return=representation"), ("Prefer", "missing=default")]
                [json| [
                  {"a": "val"},
                  {"a": "val", "b": "val"}
                ]|]
              `shouldRespondWith`
                (if actualPgVersion < pgVersion140
                  then [json| {
                    "code": "42601",
                    "details": "Column \"b\" is a generated column.",
                    "hint": null,
                    "message": "cannot insert into column \"b\""
                  }|]
                  else [json| {
                    "code": "428C9",
                    "details": "Column \"b\" is a generated column.",
                    "hint": null,
                    "message": "cannot insert a non-DEFAULT value into column \"b\""
                  }|])
                { matchStatus  = 400 }

        it "inserts a default on a DOMAIN with default" $
          request methodPost "/evil_friends?columns=id,name" [("Prefer", "return=representation"), ("Prefer", "missing=default")]
              [json| { "name": "Lu" } |]
            `shouldRespondWith`
              [json| [{"id": 666, "name": "Lu"}] |]
              { matchStatus  = 201
              , matchHeaders = ["Preference-Applied" <:> "missing=default"]
              }

    it "inserts json that has duplicate keys" $ do
      request methodPost "/tbl_w_json" [("Prefer", "return=representation")]
          [json| { "data": { "a": 1, "a": 2 }, "id": 3 } |]
        `shouldRespondWith`
          [json| [ { "data": { "a": 1, "a": 2 }, "id": 3 } ] |]
          { matchStatus  = 201
          }
      request methodPost "/tbl_w_json?columns=id,data" [("Prefer", "return=representation")]
          [json| { "data": { "a": 1, "a": 2 }, "id": 3 } |]
        `shouldRespondWith`
          [json| [ { "data": { "a": 1, "a": 2 }, "id": 3 } ] |]
          { matchStatus  = 201
          }

    context "with unicode values" $ do
      it "succeeds and returns full representation" $
        request methodPost "/simple_pk2?select=extra,k"
            [("Prefer", "return=representation")]
            [json| { "k":"圍棋", "extra":"￥" } |]
        `shouldRespondWith`
          [json|[ { "k":"圍棋", "extra":"￥" } ]|]
          { matchStatus = 201 }

      it "succeeds and returns usable location header" $ do
        p <- request methodPost "/simple_pk2?select=extra,k"
            [("Prefer", "tx=commit"), ("Prefer", "return=headers-only")]
            [json| { "k":"圍棋", "extra":"￥" } |]
        pure p `shouldRespondWith`
          ""
          { matchStatus = 201 }

        Just location <- pure $ lookup hLocation $ simpleHeaders p
        get location
          `shouldRespondWith`
            [json|[ { "k":"圍棋", "extra":"￥" } ]|]

        request methodDelete location
            [("Prefer", "tx=commit")]
            ""
          `shouldRespondWith`
            ""
            { matchStatus = 204
            , matchHeaders = [matchHeaderAbsent hContentType]
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
          , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8"]
          }

      it "can post nulls" $
        request methodPost "/no_pk"
                     [("Content-Type", "text/csv"), ("Accept", "text/csv"), ("Prefer", "return=representation")]
                     "a,b\nNULL,foo"
          `shouldRespondWith` "a,b\n,foo"
          { matchStatus  = 201
          , matchHeaders = ["Content-Type" <:> "text/csv; charset=utf-8"]
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
        [json|{"message":"All lines must have same number of fields","code":"PGRST102","details":null,"hint":null}|]
        { matchStatus  = 400
        , matchHeaders = [matchContentTypeJson]
        }

  describe "Row level permission" $
    it "set user_id when inserting rows" $ do
      request methodPost "/authors_only"
          [ authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.B-lReuGNDwAlU1GOC476MlO0vAt9JNoHIlxg2vwMaO0", ("Prefer", "return=representation") ]
          [json| { "secret": "nyancat" } |]
        `shouldRespondWith`
          [json|[{"owner":"jdoe","secret":"nyancat"}]|]
          { matchStatus  = 201 }

      request methodPost "/authors_only"
          -- jwt token for jroe
          [ authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqcm9lIn0.2e7mx0U4uDcInlbJVOBGlrRufwqWLINDIEDC1vS0nw8", ("Prefer", "return=representation") ]
          [json| { "secret": "lolcat", "owner": "hacker" } |]
        `shouldRespondWith`
          [json|[{"owner":"jroe","secret":"lolcat"}]|]
          { matchStatus  = 201 }

  context "tables with self reference foreign keys" $ do
    it "embeds parent after insert" $
      request methodPost "/web_content?select=id,name,parent_content:p_web_id(name)"
              [("Prefer", "return=representation")]
        [json|{"id":6, "name":"wot", "p_web_id":4}|]
        `shouldRespondWith`
        [json|[{"id":6,"name":"wot","parent_content":{"name":"wut"}}]|]
        { matchStatus  = 201
        , matchHeaders = [ matchContentTypeJson , matchHeaderAbsent hLocation ]
        }

  context "table with limited privileges" $ do
    it "succeeds inserting if correct select is applied" $
      request methodPost "/limited_article_stars?select=article_id,user_id" [("Prefer", "return=representation")]
        [json| {"article_id": 2, "user_id": 1} |] `shouldRespondWith` [json|[{"article_id":2,"user_id":1}]|]
        { matchStatus  = 201
        , matchHeaders = []
        }

    it "fails inserting if more columns are selected" $
      request methodPost "/limited_article_stars?select=article_id,user_id,created_at" [("Prefer", "return=representation")]
        [json| {"article_id": 2, "user_id": 2} |] `shouldRespondWith` (
      if actualPgVersion >= pgVersion112 then
      [json|{"hint":null,"details":null,"code":"42501","message":"permission denied for view limited_article_stars"}|]
         else
      [json|{"hint":null,"details":null,"code":"42501","message":"permission denied for relation limited_article_stars"}|]
                                                                      )
        { matchStatus  = 401
        , matchHeaders = []
        }

    it "fails inserting if select is not specified" $
      request methodPost "/limited_article_stars" [("Prefer", "return=representation")]
        [json| {"article_id": 3, "user_id": 1} |] `shouldRespondWith` (
      if actualPgVersion >= pgVersion112 then
      [json|{"hint":null,"details":null,"code":"42501","message":"permission denied for view limited_article_stars"}|]
         else
      [json|{"hint":null,"details":null,"code":"42501","message":"permission denied for relation limited_article_stars"}|]
                                                                      )
        { matchStatus  = 401
        , matchHeaders = []
        }

    it "can insert in a table with no select and return=minimal" $ do
      request methodPost "/insertonly"
          [("Prefer", "return=minimal")]
          [json| { "v":"some value" } |]
        `shouldRespondWith`
          ""
          { matchStatus = 201
          , matchHeaders = [matchHeaderAbsent hContentType]
          }

  describe "Inserting into VIEWs" $ do
    context "requesting no representation" $ do
      it "succeeds with 201" $
        post "/compound_pk_view"
            [json|{"k1":1,"k2":"test","extra":2}|]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , matchHeaderAbsent hLocation ]
            }

      it "returns a location header with pks from both tables" $
        request methodPost "/with_multiple_pks" [("Prefer", "return=headers-only")]
            [json|{"pk1":1,"pk2":2}|]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Location" <:> "/with_multiple_pks?pk1=eq.1&pk2=eq.2"
                             , "Content-Range" <:> "*/*" ]
            }

    context "requesting header only representation" $ do
      it "returns a location header with a composite PK col" $
        request methodPost "/compound_pk_view" [("Prefer", "return=headers-only")]
            [json|{"k1":1,"k2":"test","extra":2}|]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Location" <:> "/compound_pk_view?k1=eq.1&k2=eq.test"
                             , "Content-Range" <:> "*/*" ]
            }

      it "should not throw and return location header when a PK is null" $
        request methodPost "/test_null_pk_competitors_sponsors" [("Prefer", "return=headers-only")]
            [json|{"id":1}|]
          `shouldRespondWith`
            ""
            { matchStatus  = 201
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Location" <:> "/test_null_pk_competitors_sponsors?id=eq.1&sponsor_id=is.null"
                             , "Content-Range" <:> "*/*" ]
            }

  -- Data representations for payload parsing requires Postgres 10 or above.
  when (actualPgVersion >= pgVersion100) $ do
    describe "Data representations" $ do
      context "on regular table" $ do
        it "parses values in POST body" $
          -- we don't check that the parsing is correct here, just that it's happening. If it doesn't happen we'll get a
          -- an "invalid input syntax for type integer:" error.
          request methodPost "/datarep_todos" [("Prefer", "return=headers-only")]
            [json| {"id":5, "name": "party", "label_color": "#001100", "due_at": "2018-01-03T11:00:00+00"} |]
            `shouldRespondWith`
            ""
              { matchStatus  = 201
              , matchHeaders = [ matchHeaderAbsent hContentType
                               , "Location" <:> "/datarep_todos?id=eq.5"
                               , "Content-Range" <:> "*/*" ]
              }

        it "parses values in POST body and formats individually selected values in return=representation" $
          request methodPost "/datarep_todos?select=id,label_color" [("Prefer", "return=representation")]
            [json| {"id":5, "name": "party", "label_color": "#001100", "due_at": "2018-01-03T11:00:00+00"} |]
            `shouldRespondWith`
            [json| [{"id":5, "label_color": "#001100"}] |]
              { matchStatus  = 201
              , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8",
                                "Content-Range" <:> "*/*"]
              }

        it "parses values in POST body and formats values in return=representation" $
          request methodPost "/datarep_todos" [("Prefer", "return=representation")]
            [json| {"id":5, "name": "party", "label_color": "#001100", "due_at": "2018-01-03T11:00:00+00", "icon_image": "3q2+7w", "created_at":-15, "budget": "-100000000000000.13"} |]
            `shouldRespondWith`
            [json| [{"id":5,"name": "party", "label_color": "#001100", "due_at":"2018-01-03T11:00:00Z", "icon_image": "3q2+7w==", "created_at":-15, "budget": "-100000000000000.13"}] |]
              { matchStatus  = 201
              , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8",
                                "Content-Range" <:> "*/*"]
              }

      context "with ?columns parameter" $ do
        it "ignores json keys not included in ?columns; parses only the ones specified" $
          request methodPost "/datarep_todos?columns=id,label_color&select=id,name,label_color,due_at" [("Prefer", "return=representation")]
            [json| {"id":5, "name": "party", "label_color": "#001100", "due_at": "invalid but should be ignored"} |]
            `shouldRespondWith`
            [json| [{"id":5, "name":null, "label_color": "#001100", "due_at": "2018-01-01T00:00:00Z"}] |]
              { matchStatus  = 201
              , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8",
                                "Content-Range" <:> "*/*"]
              }

        it "fails without parsing anything if at least one specified column doesn't exist" $
          request methodPost "/datarep_todos?columns=id,label_color,helicopters&select=id,name,label_color,due_at" [("Prefer", "return=representation")]
            [json| {"due_at": "2019-01-03T11:00:00+00", "smth": "here", "label_color": "invalid", "fake_id": 13} |]
            `shouldRespondWith`
            [json| {"code":"PGRST204","message":"Column 'helicopters' of relation 'datarep_todos' does not exist","details":null,"hint":null} |]
              { matchStatus  = 400
              , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
              }

      context "on updatable view" $ do
        it "parses values in POST body" $
          -- we don't check that the parsing is correct here, just that it's happening. If it doesn't happen we'll get a
          -- an "invalid input syntax for type integer:" error.
          request methodPost "/datarep_todos_computed" [("Prefer", "return=headers-only")]
            [json| {"id":5, "name": "party", "label_color": "#001100", "due_at": "2018-01-03T11:00:00+00"} |]
            `shouldRespondWith`
            ""
              { matchStatus  = 201
              , matchHeaders = [ matchHeaderAbsent hContentType
                               , "Location" <:> "/datarep_todos_computed?id=eq.5"
                               , "Content-Range" <:> "*/*" ]
              }

        it "parses values in POST body and formats individually selected values in return=representation" $
          request methodPost "/datarep_todos_computed?select=id,label_color" [("Prefer", "return=representation")]
            [json| {"id":5, "name": "party", "label_color": "#001100", "due_at": "2018-01-03T11:00:00+00"} |]
            `shouldRespondWith`
            [json| [{"id":5, "label_color": "#001100"}] |]
              { matchStatus  = 201
              , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8",
                                "Content-Range" <:> "*/*"]
              }

        it "parses values in POST body and formats values in return=representation" $
          request methodPost "/datarep_todos_computed" [("Prefer", "return=representation")]
            [json| {"id":5, "name": "party", "label_color": "#001100", "due_at": "2018-01-03T11:00:00+00"} |]
            `shouldRespondWith`
            [json| [{"id":5,"name": "party", "label_color": "#001100", "due_at":"2018-01-03T11:00:00Z", "dark_color":"#000880"}] |]
              { matchStatus  = 201
              , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8",
                                "Content-Range" <:> "*/*"]
              }

      context "on updatable views with ?columns parameter" $ do
        it "ignores json keys not included in ?columns; parses only the ones specified" $
          request methodPost "/datarep_todos_computed?columns=id,label_color&select=id,name,label_color,due_at" [("Prefer", "return=representation")]
            [json| {"id":5, "name": "party", "label_color": "#001100", "due_at": "invalid but should be ignored"} |]
            `shouldRespondWith`
            [json| [{"id":5, "name":null, "label_color": "#001100", "due_at": "2018-01-01T00:00:00Z"}] |]
              { matchStatus  = 201
              , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8",
                                "Content-Range" <:> "*/*"]
              }

        it "fails without parsing anything if at least one specified column doesn't exist" $
          request methodPost "/datarep_todos_computed?columns=id,label_color,helicopters&select=id,name,label_color,due_at" [("Prefer", "return=representation")]
            [json| {"due_at": "2019-01-03T11:00:00+00", "smth": "here", "label_color": "invalid", "fake_id": 13} |]
            `shouldRespondWith`
            [json| {"code":"PGRST204","message":"Column 'helicopters' of relation 'datarep_todos_computed' does not exist","details":null,"hint":null} |]
              { matchStatus  = 400
              , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
              }
