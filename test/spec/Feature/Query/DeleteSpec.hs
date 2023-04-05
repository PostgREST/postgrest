module Feature.Query.DeleteSpec where

import Data.Aeson.QQ

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

tblDataBeforeItem = [aesonQQ|[
                       { "id": 1, "name": "item-1" }
                     , { "id": 2, "name": "item-2" }
                     , { "id": 3, "name": "item-3" }
                     ]|]

tblDataBeforeBulk = [aesonQQ|[
                      { "id": 1, "name": "item-1", "observation": null }
                    , { "id": 2, "name": "item-2", "observation": null }
                    , { "id": 3, "name": "item-3", "observation": null }
                    ]|]

spec :: SpecWith ((), Application)
spec =
  describe "Deleting" $ do
    context "existing record" $ do
      it "succeeds with 204 and deletion count" $
        request methodDelete "/items?id=eq.1"
            []
            ""
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/*" ]
            }

      it "returns the deleted item and count if requested" $
        request methodDelete "/items?id=eq.2" [("Prefer", "return=representation"), ("Prefer", "count=exact")] ""
          `shouldRespondWith` [json|[{"id":2}]|]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "*/1"]
          }

      it "ignores ?select= when return not set or return=minimal" $ do
        request methodDelete "/items?id=eq.3&select=id"
            []
            ""
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/*" ]
            }
        request methodDelete "/items?id=eq.3&select=id"
            [("Prefer", "return=minimal")]
            ""
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/*" ]
            }

      it "returns the deleted item and shapes the response" $
        request methodDelete "/complex_items?id=eq.2&select=id,name" [("Prefer", "return=representation")] ""
          `shouldRespondWith` [json|[{"id":2,"name":"Two"}]|]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "*/*"]
          }

      it "can rename and cast the selected columns" $
        request methodDelete "/complex_items?id=eq.3&select=ciId:id::text,ciName:name" [("Prefer", "return=representation")] ""
          `shouldRespondWith` [json|[{"ciId":"3","ciName":"Three"}]|]

      it "can embed (parent) entities" $
        request methodDelete "/tasks?id=eq.8&select=id,name,project:projects(id)" [("Prefer", "return=representation")] ""
          `shouldRespondWith` [json|[{"id":8,"name":"Code OSX","project":{"id":4}}]|]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "*/*"]
          }

      it "embeds an O2O relationship after delete" $ do
        request methodDelete "/students?id=eq.1&select=name,students_info(address)"
                [("Prefer", "return=representation")] ""
          `shouldRespondWith`
          [json|[
            {
              "name": "John Doe",
              "students_info":{"address":"Street 1"}
            }
          ]|]
          { matchStatus  = 200,
            matchHeaders = [matchContentTypeJson]
          }
        request methodDelete "/students_info?id=eq.1&select=address,students(name)"
                [("Prefer", "return=representation")] ""
          `shouldRespondWith`
          [json|[
            {
              "address": "Street 1",
              "students":{"name": "John Doe"}
            }
          ]|]
          { matchStatus  = 200,
            matchHeaders = [matchContentTypeJson]
          }

      it "ignores the json body" $
        baseTable "body_delete_items" "id" tblDataBeforeItem
        `mutatesWith`
        requestMutation methodDelete "/body_delete_items?name=eq.item-2" mempty
        [json|[
          { "id": 1, "other": "value"},
          { "id": 2 },
          "value"
        ]|]
        `shouldMutateInto`
        [json|[
          { "id": 1, "name": "item-1" }
        , { "id": 3, "name": "item-3" }
        ]|]

      it "ignores the values in ?column" $
        baseTable "body_delete_items" "id" tblDataBeforeItem
        `mutatesWith`
        requestMutation methodDelete "/body_delete_items?name=eq.item-2&columns=id" mempty
        [json|[
          { "id": 1, "other": "value"}
        , { "id": 2 }
        , "value"
        ]|]
        `shouldMutateInto`
        [json|[
          { "id": 1, "name": "item-1" }
        , { "id": 3, "name": "item-3" }
        ]|]

    context "known route, no records matched" $
      it "includes [] body if return=rep" $
        request methodDelete "/items?id=eq.101"
          [("Prefer", "return=representation")] ""
          `shouldRespondWith` "[]"
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "*/*"]
          }

    context "totally unknown route" $
      it "fails with 404" $
        request methodDelete "/foozle?id=eq.101" [] "" `shouldRespondWith` 404

    context "table with limited privileges" $ do
      it "fails deleting the row when return=representation and selecting all the columns" $
        request methodDelete "/app_users?id=eq.1" [("Prefer", "return=representation")] mempty
            `shouldRespondWith` 401

      it "succeeds deleting the row when return=representation and selecting only the privileged columns" $
        request methodDelete "/app_users?id=eq.1&select=id,email" [("Prefer", "return=representation")]
          [json| { "password": "passxyz" } |]
            `shouldRespondWith` [json|[ { "id": 1, "email": "test@123.com" } ]|]
            { matchStatus  = 200
            , matchHeaders = ["Content-Range" <:> "*/*"]
            }

      it "suceeds deleting the row with no explicit select when using return=minimal" $
        request methodDelete "/app_users?id=eq.2"
            [("Prefer", "return=minimal")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus = 204
            , matchHeaders = [matchHeaderAbsent hContentType]
            }

      it "suceeds deleting the row with no explicit select by default" $
        request methodDelete "/app_users?id=eq.3"
            []
            mempty
          `shouldRespondWith`
            ""
            { matchStatus = 204
            , matchHeaders = [matchHeaderAbsent hContentType]
            }

    context "limited delete" $ do
      it "works with the limit and offset query params" $
        baseTable "limited_delete_items" "id" tblDataBeforeItem
        `mutatesWith`
        requestMutation methodDelete "/limited_delete_items?order=id&limit=1&offset=1" mempty mempty
        `shouldMutateInto`
        [json|[
          { "id": 1, "name": "item-1" }
        , { "id": 3, "name": "item-3" }
        ]|]

      it "works with the limit query param plus a filter" $
        baseTable "limited_delete_items" "id" tblDataBeforeItem
        `mutatesWith`
        requestMutation methodDelete "/limited_delete_items?order=id&limit=1&id=gt.1" mempty mempty
        `shouldMutateInto`
        [json|[
          { "id": 1, "name": "item-1" }
        , { "id": 3, "name": "item-3" }
        ]|]

      it "fails without an explicit order by" $
        request methodDelete "/limited_delete_items?limit=1&offset=1"
            [("Prefer", "tx=commit")]
            mempty
          `shouldRespondWith`
            [json| {
              "code":"PGRST109",
              "hint": "Apply an 'order' using unique column(s)",
              "details": null,
              "message": "A 'limit' was applied without an explicit 'order'"
              }|]
            { matchStatus  = 400 }

      it "fails when not ordering by a unique column" $
        request methodDelete "/limited_delete_items_wnonuniq_view?order=static&limit=1"
            [("Prefer", "tx=commit")]
            mempty
          `shouldRespondWith`
            [json| {
              "code":"PGRST110",
              "hint": null,
              "details":"Results contain 3 rows changed but the maximum number allowed is 1",
              "message":"The maximum number of rows allowed to change was surpassed"
              }|]
            { matchStatus  = 400 }

      it "works with views with an explicit order by unique col" $
        baseTable "limited_delete_items_view" "id" tblDataBeforeItem
        `mutatesWith`
        requestMutation methodDelete "/limited_delete_items_view?order=id&limit=1&offset=1" mempty mempty
        `shouldMutateInto`
        [json|[
          { "id": 1, "name": "item-1" }
        , { "id": 3, "name": "item-3" }
        ]|]

      it "works with views with an explicit order by composite pk" $
        baseTable "limited_delete_items_cpk_view" "id" tblDataBeforeItem
        `mutatesWith`
        requestMutation methodDelete "/limited_delete_items_cpk_view?order=id,name&limit=1&offset=1" mempty mempty
        `shouldMutateInto`
        [json|[
          { "id": 1, "name": "item-1" }
        , { "id": 3, "name": "item-3" }
        ]|]

      it "works on a table without a pk by ordering by 'ctid'" $
        baseTable "limited_delete_items_no_pk" "id" tblDataBeforeItem
        `mutatesWith`
        requestMutation methodDelete "/limited_delete_items_no_pk?order=ctid&limit=1&offset=1" mempty mempty
        `shouldMutateInto`
        [json|[
          { "id": 1, "name": "item-1" }
        , { "id": 3, "name": "item-3" }
        ]|]

      it "ignores the Range header" $ do
        baseTable "limited_delete_items" "id" tblDataBeforeItem
         `mutatesWith`
         requestMutation methodDelete "/limited_delete_items"
          (rangeHdrs (ByteRangeFromTo 0 0)) mempty
         `shouldMutateInto`
         [json|[]|]

        baseTable "limited_delete_items" "id" tblDataBeforeItem
         `mutatesWith`
         requestMutation methodDelete "/limited_delete_items?id=gte.2"
          (rangeHdrs (ByteRangeFromTo 0 0)) mempty
         `shouldMutateInto`
         [json|[ { "id": 1, "name": "item-1" } ]|]

      it "ignores the Range header and does not do a limited delete" $
        baseTable "limited_delete_items" "id" tblDataBeforeItem
         `mutatesWith`
         requestMutation methodDelete "/limited_delete_items?order=id"
          (rangeHdrs (ByteRangeFromTo 0 0)) mempty
         `shouldMutateInto`
         [json|[]|]

      it "ignores the Range header and does not throw an invalid range error" $
        baseTable "limited_delete_items" "id" tblDataBeforeItem
         `mutatesWith`
         requestMutation methodDelete "/limited_delete_items?order=id&limit=1&offset=1"
          (rangeHdrs (ByteRangeFromTo 0 0)) mempty
         `shouldMutateInto`
         [json|[
           { "id": 1, "name": "item-1" }
         , { "id": 3, "name": "item-3" }
         ]|]

      it "ignores the Range header but not the limit and offset params" $
        baseTable "limited_delete_items" "id" tblDataBeforeItem
         `mutatesWith`
         requestMutation methodDelete "/limited_delete_items?order=id&limit=2&offset=1"
          (rangeHdrs (ByteRangeFromTo 1 1)) mempty
         `shouldMutateInto`
         [json|[
           { "id": 1, "name": "item-1" }
         ]|]

    context "bulk deletes" $ do
      it "can delete tables with simple pk" $
        baseTable "bulk_delete_items" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items" [("Prefer", "params=multiple-objects")]
           [json| [
             { "id": 2 }
           , { "id": 3 }
           ]|]
         `shouldMutateInto`
         [json|[
           { "id": 1, "name": "item-1", "observation": null }
         ]|]

      it "can delete tables with composite pk" $
        baseTable "bulk_delete_items_cpk" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items_cpk" [("Prefer", "params=multiple-objects")]
           [json| [
             { "id": 2, "name": "item-2" }
           , { "id": 3, "name": "item-3" }
           ]|]
         `shouldMutateInto`
         [json|[
           { "id": 1, "name": "item-1", "observation": null }
         ]|]

      it "deletes ignoring columns different than the pk in the json body" $
        baseTable "bulk_delete_items" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items" [("Prefer", "params=multiple-objects")]
           [json| [
             { "id": 2, "name": "different value 2" }
           , { "id": 3, "name": "different value 3" }
           ]|]
         `shouldMutateInto`
         [json|[
           { "id": 1, "name": "item-1", "observation": null }
         ]|]

      it "deletes with filters in the query string" $
        baseTable "bulk_delete_items" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items?name=like.item-*&id=gte.2" [("Prefer", "params=multiple-objects")]
           [json| [
             { "id": 1 }
           , { "id": 3 }
           ]|]
         `shouldMutateInto`
         [json|[
           { "id": 1, "name": "item-1", "observation": null }
         , { "id": 2, "name": "item-2", "observation": null }
         ]|]

      it "fails when the simple pk is not present in the body" $
        request methodDelete "/bulk_delete_items" [("Prefer", "params=multiple-objects")]
             [json| [
               { "name": "item-2" }
             , { "name": "item-3" }
             ]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST121",
              "hint": null,
              "details": null,
              "message": "The payload or 'columns' query string parameter must include all primary key columns for bulk deletes"
            }|]
            { matchStatus  = 400 }

      it "fails when the composite pk is not present in the body" $
        request methodDelete "/bulk_delete_items_cpk" [("Prefer", "params=multiple-objects")]
             [json| [
               { "name": "item-2" }
             , { "name": "item-3" }
             ]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST121",
              "hint": null,
              "details": null,
              "message": "The payload or 'columns' query string parameter must include all primary key columns for bulk deletes"
            }|]
            { matchStatus  = 400 }

      it "fails with an empty body" $ do
        request methodDelete "/bulk_delete_items" [("Prefer", "params=multiple-objects")]
             [json| {}|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST121",
              "hint": null,
              "details": null,
              "message": "The payload or 'columns' query string parameter must include all primary key columns for bulk deletes"
            }|]
            { matchStatus  = 400 }

        request methodDelete "/bulk_delete_items" [("Prefer", "params=multiple-objects")]
             [json| []|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST121",
              "hint": null,
              "details": null,
              "message": "The payload or 'columns' query string parameter must include all primary key columns for bulk deletes"
            }|]
            { matchStatus  = 400 }

        request methodDelete "/bulk_delete_items" [("Prefer", "params=multiple-objects")]
             [json| [{}]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST121",
              "hint": null,
              "details": null,
              "message": "The payload or 'columns' query string parameter must include all primary key columns for bulk deletes"
            }|]
            { matchStatus  = 400 }

        request methodDelete "/bulk_delete_items" [("Prefer", "params=multiple-objects")]
             [json| [{}, {}]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST121",
              "hint": null,
              "details": null,
              "message": "The payload or 'columns' query string parameter must include all primary key columns for bulk deletes"
            }|]
            { matchStatus  = 400 }

      it "rejects json array that has objects with different keys" $
        request methodDelete "/bulk_delete_items" [("Prefer", "params=multiple-objects")]
             [json| [
               { "id": 1, "name": "a value", "observation": "New!" }
             , { "name": "item-2" }
             , { "id": 3, "done": true}
             ]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST102",
              "hint":null,
              "details":null,
              "message":"All object keys must match"
            }|]
            { matchStatus  = 400 }

      it "rejects json array that isn't exclusivily composed of objects" $
        request methodDelete "/bulk_delete_items" [("Prefer", "params=multiple-objects")]
             [json| [
               { "id": 1, "name": "a value", "observation": "New!" }
             , false
             , [ 1, 2, 3 ]
             , { "id": 3 }
             ]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST102",
              "hint":null,
              "details":null,
              "message":"All object keys must match"
            }|]
            { matchStatus  = 400 }

      it "deletes when the simple pk is specified in ?columns" $
        baseTable "bulk_delete_items" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items?columns=id" [("Prefer", "params=multiple-objects")]
           [json| [
             { "id": 1 }
           , { "id": 3 }
           ]|]
         `shouldMutateInto`
         [json|[
           { "id": 2, "name": "item-2", "observation": null }
         ]|]

      it "deletes when the composite pk is specified in ?columns" $
        baseTable "bulk_delete_items_cpk" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items_cpk?columns=id,name" [("Prefer", "params=multiple-objects")]
           [json| [
             { "id": 1, "name": "item-1" }
           , { "id": 3, "name": "item-3" }
           ]|]
         `shouldMutateInto`
         [json|[
           { "id": 2, "name": "item-2", "observation": null }
         ]|]

      it "deletes ignoring columns different than the pk in ?columns" $
        baseTable "bulk_delete_items" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items?columns=id,name" [("Prefer", "params=multiple-objects")]
           [json| [
             { "id": 1, "name": "different value 1" }
           , { "id": 3, "other": "value", "observation": "New!" }
           ]|]
         `shouldMutateInto`
         [json|[
           { "id": 2, "name": "item-2", "observation": null }
         ]|]

      it "does not delete any rows if the pk is present in ?column but not in the body" $ do
        baseTable "bulk_delete_items" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items?columns=id" [("Prefer", "params=multiple-objects")]
           [json| [
             { name: "item-1" }
           , { name: "item-3" }
           ]|]
         `shouldMutateInto`
         [json|[
           { "id": 1, "name": "item-1", "observation": null }
         , { "id": 2, "name": "item-2", "observation": null }
         , { "id": 3, "name": "item-3", "observation": null }
         ]|]

        baseTable "bulk_delete_items" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items?columns=id" [("Prefer", "params=multiple-objects")]
           [json| []|]
         `shouldMutateInto`
         [json|[
           { "id": 1, "name": "item-1", "observation": null }
         , { "id": 2, "name": "item-2", "observation": null }
         , { "id": 3, "name": "item-3", "observation": null }
         ]|]

      it "fails when there is no simple pk in ?columns" $
        request methodDelete "/bulk_delete_items?columns=name" [("Prefer", "params=multiple-objects")]
             [json| [
               { "name": "item-2" }
             , { "name": "item-3" }
             ]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST121",
              "hint": null,
              "details": null,
              "message": "The payload or 'columns' query string parameter must include all primary key columns for bulk deletes"
            }|]
            { matchStatus  = 400 }

      it "fails when there is no composite pk in ?columns" $
        request methodDelete "/bulk_delete_items_cpk?columns=name" [("Prefer", "params=multiple-objects")]
             [json| [
               { "name": "item-2" }
             , { "name": "item-3" }
             ]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST121",
              "hint": null,
              "details": null,
              "message": "The payload or 'columns' query string parameter must include all primary key columns for bulk deletes"
            }|]
            { matchStatus  = 400 }

      it "deletes the whole table if it does not have a pk" $ do
        baseTable "bulk_delete_items_no_pk" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items_no_pk" [("Prefer", "params=multiple-objects")]
           [json| [
             { "id": 1 }
           , { "id": 3 }
           ]|]
         `shouldMutateInto`
         [json|[]|]

        baseTable "bulk_delete_items_no_pk" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items_no_pk" [("Prefer", "params=multiple-objects")]
           [json|[]|]
         `shouldMutateInto`
         [json|[]|]

        baseTable "bulk_delete_items_no_pk" "id" tblDataBeforeBulk
         `mutatesWith`
         requestMutation methodDelete "/bulk_delete_items_no_pk?columns=id,name" [("Prefer", "params=multiple-objects")]
           [json| [
             { "id": 1, "name": "item-1" }
           , { "id": 3, "name": "item-3" }
           ]|]
         `shouldMutateInto`
         [json|[]|]

      it "returns the deleted item and count if requested" $
        request methodDelete "/bulk_delete_items"
          [("Prefer", "params=multiple-objects"), ("Prefer", "return=representation"), ("Prefer", "count=exact")]
          [json| [
            { "id": 1 }
          , { "id": 3 }
          ]|]
         `shouldRespondWith`
          [json|[
            { "id": 1, "name": "item-1", "observation": null }
          , { "id": 3, "name": "item-3", "observation": null }
          ]|]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "*/2"]
          }

      it "fails when limit is specified" $
        request methodDelete "/bulk_delete_items?limit=1" [("Prefer", "params=multiple-objects")]
             [json| [
               { "name": "item-2" }
             , { "name": "item-3" }
             ]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST122",
              "hint": null,
              "details": null,
              "message": "limit/offset query string parameters are not allowed for bulk deletes"
            }|]
            { matchStatus  = 400 }

      it "fails when offset is specified" $
        request methodDelete "/bulk_delete_items?offset=1" [("Prefer", "params=multiple-objects")]
             [json| [
               { "name": "item-2" }
             , { "name": "item-3" }
             ]|]
          `shouldRespondWith`
            [json| {
              "code":"PGRST122",
              "hint": null,
              "details": null,
              "message": "limit/offset query string parameters are not allowed for bulk deletes"
            }|]
            { matchStatus  = 400 }
