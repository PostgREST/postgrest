module Feature.Query.LimitedMutationSpec where

import Data.Aeson.QQ

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

tblDataBefore = [aesonQQ|[
                  { "id": 1, "name": "item-1" }
                , { "id": 2, "name": "item-2" }
                , { "id": 3, "name": "item-3" }
                ]|]

spec :: SpecWith ((), Application)
spec = do
  describe "limited delete" $ do
    it "works with the limit and offset query params" $
      baseTable "limited_delete_items" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodDelete "/limited_delete_items?order=id&limit=1&offset=1" mempty mempty
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "item-1" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "works with the limit query param plus a filter" $
      baseTable "limited_delete_items" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodDelete "/limited_delete_items?order=id&limit=1&id=gt.1" mempty mempty
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "item-1" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "fails without an explicit order by" $
      request methodDelete "/limited_delete_items?limit=1&offset=1"
          mempty
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
          mempty
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
      baseTable "limited_delete_items_view" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodDelete "/limited_delete_items_view?order=id&limit=1&offset=1" mempty mempty
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "item-1" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "works with views with an explicit order by composite pk" $
      baseTable "limited_delete_items_cpk_view" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodDelete "/limited_delete_items_cpk_view?order=id,name&limit=1&offset=1" mempty mempty
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "item-1" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "works on a table without a pk by ordering by 'ctid'" $
      baseTable "limited_delete_items_no_pk" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodDelete "/limited_delete_items_no_pk?order=ctid&limit=1&offset=1" mempty mempty
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "item-1" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "ignores the Range header" $ do
      baseTable "limited_delete_items" "id" tblDataBefore
       `mutatesWith`
       requestMutation methodDelete "/limited_delete_items"
        (rangeHdrs (ByteRangeFromTo 0 0)) mempty
       `shouldMutateInto`
       [json|[]|]

      baseTable "limited_delete_items" "id" tblDataBefore
       `mutatesWith`
       requestMutation methodDelete "/limited_delete_items?id=gte.2"
        (rangeHdrs (ByteRangeFromTo 0 0)) mempty
       `shouldMutateInto`
       [json|[ { "id": 1, "name": "item-1" } ]|]

    it "ignores the Range header and does not do a limited delete" $
      baseTable "limited_delete_items" "id" tblDataBefore
       `mutatesWith`
       requestMutation methodDelete "/limited_delete_items?order=id"
        (rangeHdrs (ByteRangeFromTo 0 0)) mempty
       `shouldMutateInto`
       [json|[]|]

    it "ignores the Range header and does not throw an invalid range error" $
      baseTable "limited_delete_items" "id" tblDataBefore
       `mutatesWith`
       requestMutation methodDelete "/limited_delete_items?order=id&limit=1&offset=1"
        (rangeHdrs (ByteRangeFromTo 0 0)) mempty
       `shouldMutateInto`
       [json|[
         { "id": 1, "name": "item-1" }
       , { "id": 3, "name": "item-3" }
       ]|]

    it "ignores the Range header but not the limit and offset params" $
      baseTable "limited_delete_items" "id" tblDataBefore
       `mutatesWith`
       requestMutation methodDelete "/limited_delete_items?order=id&limit=2&offset=1"
        (rangeHdrs (ByteRangeFromTo 1 1)) mempty
       `shouldMutateInto`
       [json|[
         { "id": 1, "name": "item-1" }
       ]|]

  describe "limited update" $ do
    it "works with the limit query param" $
      baseTable "limited_update_items" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodPatch "/limited_update_items?order=id&limit=2" mempty
        [json| {"name": "updated-item"} |]
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "updated-item" }
      , { "id": 2, "name": "updated-item" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "works with the limit query param plus a filter" $
      baseTable "limited_update_items" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodPatch "/limited_update_items?order=id&limit=1&id=gt.2" mempty
        [json| {"name": "updated-item"} |]
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "item-1" }
      , { "id": 2, "name": "item-2" }
      , { "id": 3, "name": "updated-item" }
      ]|]

    it "works with the limit and offset query params" $
      baseTable "limited_update_items" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodPatch "/limited_update_items?order=id&limit=1&offset=1" mempty
        [json| {"name": "updated-item"} |]
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "item-1" }
      , { "id": 2, "name": "updated-item" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "fails without an explicit order by" $
      request methodPatch "/limited_update_items?limit=1&offset=1"
          mempty
          [json| {"name": "updated-item"} |]
        `shouldRespondWith`
          [json| {
            "code":"PGRST109",
            "hint": "Apply an 'order' using unique column(s)",
            "details": null,
            "message": "A 'limit' was applied without an explicit 'order'"
            }|]
          { matchStatus  = 400 }

    it "fails when not ordering by a unique column" $
      request methodPatch "/limited_update_items_wnonuniq_view?order=static&limit=1"
          mempty
          [json| {"name": "updated-item"} |]
        `shouldRespondWith`
          [json| {
            "code":"PGRST110",
            "hint": null,
            "details":"Results contain 3 rows changed but the maximum number allowed is 1",
            "message":"The maximum number of rows allowed to change was surpassed"
            }|]
          { matchStatus  = 400 }

    it "works with views with an explicit order by unique col" $
      baseTable "limited_update_items_view" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodPatch "/limited_update_items_view?order=id&limit=1&offset=1" mempty
        [json| {"name": "updated-item"} |]
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "item-1" }
      , { "id": 2, "name": "updated-item" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "works with views with an explicit order by composite pk" $
      baseTable "limited_update_items_cpk_view" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodPatch "/limited_update_items_cpk_view?order=id,name&limit=1&offset=1" mempty
        [json| {"name": "updated-item"} |]
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "item-1" }
      , { "id": 2, "name": "updated-item" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "works on a table without a pk by ordering by 'ctid'" $
      baseTable "limited_update_items_no_pk" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodPatch "/limited_update_items_no_pk?order=ctid&limit=1" mempty
        [json| {"name": "updated-item"} |]
      `shouldMutateInto`
      [json|[
        { "id": 1, "name": "updated-item" }
      , { "id": 2, "name": "item-2" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "ignores the Range header" $ do
      baseTable "limited_update_items" "id" tblDataBefore
       `mutatesWith`
       requestMutation methodPatch "/limited_update_items"
        (rangeHdrs (ByteRangeFromTo 0 0))
        [json| {"name": "updated-item"} |]
       `shouldMutateInto`
       [json|[
         { "id": 1, "name": "updated-item" }
       , { "id": 2, "name": "updated-item" }
       , { "id": 3, "name": "updated-item" }
       ]|]

      baseTable "limited_update_items" "id" tblDataBefore
       `mutatesWith`
       requestMutation methodPatch "/limited_update_items?id=gte.2"
        (rangeHdrs (ByteRangeFromTo 0 0))
        [json| {"name": "updated-item"} |]
       `shouldMutateInto`
       [json|[
         { "id": 1, "name": "item-1" }
       , { "id": 2, "name": "updated-item" }
       , { "id": 3, "name": "updated-item" }
       ]|]

    it "ignores the Range header and does not do a limited update" $
      baseTable "limited_update_items" "id" tblDataBefore
       `mutatesWith`
       requestMutation methodPatch "/limited_update_items?order=id"
        (rangeHdrs (ByteRangeFromTo 0 0))
        [json| {"name": "updated-item"} |]
       `shouldMutateInto`
       [json|[
         { "id": 1, "name": "updated-item" }
       , { "id": 2, "name": "updated-item" }
       , { "id": 3, "name": "updated-item" }
       ]|]

    it "ignores the Range header and does not throw an invalid range error" $
      baseTable "limited_update_items" "id" tblDataBefore
      `mutatesWith`
      requestMutation methodPatch "/limited_update_items?order=id&limit=1&offset=1"
        (rangeHdrs (ByteRangeFromTo 0 0))
        [json| {"name": "updated-item"} |]
      `shouldMutateInto`
      [json|[
       { "id": 1, "name": "item-1" }
      , { "id": 2, "name": "updated-item" }
      , { "id": 3, "name": "item-3" }
      ]|]

    it "ignores the Range header but not the limit and offset params" $
      baseTable "limited_update_items" "id" tblDataBefore
       `mutatesWith`
       requestMutation methodPatch "/limited_update_items?order=id&limit=2&offset=1"
         (rangeHdrs (ByteRangeFromTo 1 1))
         [json| {"name": "updated-item"} |]
       `shouldMutateInto`
       [json|[
         { "id": 1, "name": "item-1" }
       , { "id": 2, "name": "updated-item" }
       , { "id": 3, "name": "updated-item" }
       ]|]
