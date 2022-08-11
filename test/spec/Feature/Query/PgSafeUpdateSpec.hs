module Feature.Query.PgSafeUpdateSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get, put)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "Enabling pg-safeupdate" $ do
    context "Full table update" $ do
      it "does not update and throws error if no condition is present" $
        request methodPatch "/safe_update_items"
            []
            [json| {"name": "updated-item"} |]
          `shouldRespondWith`
            [json|{
              "code": "21000",
              "details": null,
              "hint": null,
              "message": "UPDATE requires a WHERE clause"
            }|]
            { matchStatus  = 400 }

      it "does not do a bulk update and throws error if no filter is found in the array body" $
        request methodPatch "/safe_update_items"
            [("Prefer", "tx=commit")]
            [json|[
              { "name": "item-all", "observation": "Damaged item" }
            , { "name": "item-3 - 3rd", "observation": null }
            ]|]
          `shouldRespondWith`
            [json|{
              "code": "21000",
              "details": null,
              "hint": null,
              "message": "UPDATE requires a WHERE clause"
            }|]
            { matchStatus  = 400 }

      it "does not do a bulk update and throws error without filters and no pk in ?columns" $
        request methodPatch "/safe_update_items?columns=observation"
            [("Prefer", "tx=commit")]
            [json|[
              { "id": 1, "name": "item-4", "observation": "Damaged item" }
            , { "id": 3, "name": "item-3 - 3rd", "observation": null }
            ]|]
          `shouldRespondWith`
            [json|{
              "code": "21000",
              "details": null,
              "hint": null,
              "message": "UPDATE requires a WHERE clause"
            }|]
            { matchStatus  = 400 }

      it "does not do a bulk update and throws error without filters and no composite pk in ?columns" $
        request methodPatch "/safe_update_items_cpk?columns=id,observation"
            [("Prefer", "tx=commit")]
            [json|[
              { "id": 1, "name": "item-4", "observation": "Damaged item" }
            , { "id": 3, "name": "item-3 - 3rd", "observation": null }
            ]|]
          `shouldRespondWith`
            [json|{
              "code": "21000",
              "details": null,
              "hint": null,
              "message": "UPDATE requires a WHERE clause"
            }|]
            { matchStatus  = 400 }

      it "does not do a bulk update tables without pks and throws error" $
        request methodPatch "/safe_update_items_no_pk"
            [("Prefer", "tx=commit")]
            [json|[
              { "id": 1, "name": "item-1 - 1st", "observation": "Lost item" }
            , { "id": 3, "name": "item-3 - 3rd", "observation": null }
            ]|]
          `shouldRespondWith`
            [json|{
              "code": "21000",
              "details": null,
              "hint": null,
              "message": "UPDATE requires a WHERE clause"
            }|]
            { matchStatus  = 400 }

      it "allows full table update if a filter is present" $ do
        get "/safe_update_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodPatch "/safe_update_items?id=gt.0"
            [("Prefer", "tx=commit")]
            [json|{"observation": "Damaged item"}|]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "0-2/*"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/safe_update_items?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": "Damaged item" }
            , { "id": 2, "name": "item-2", "observation": "Damaged item" }
            , { "id": 3, "name": "item-3", "observation": "Damaged item" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "safe_update_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

    context "Full table delete" $ do
      it "does not delete and throws error if no condition is present" $
        request methodDelete "/safe_delete_items" [] mempty
          `shouldRespondWith`
            [json|{
              "code": "21000",
              "details": null,
              "hint": null,
              "message": "DELETE requires a WHERE clause"
            }|]
            { matchStatus  = 400 }

      it "allows full table delete if a filter is present" $ do
        get "/safe_delete_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodDelete "/safe_delete_items?id=gt.0"
          [("Prefer", "tx=commit"), ("Prefer", "count=exact")] mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/3"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/safe_delete_items?order=id"
          `shouldRespondWith`
            [json|[]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "safe_delete_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

disabledSpec :: SpecWith ((), Application)
disabledSpec =
  describe "Disabling pg-safeupdate" $ do
    context "Full table update" $ do
      it "works if no condition is present" $ do
        get "/unsafe_update_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodPatch "/unsafe_update_items"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json| {"name": "updated-item"} |]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "0-2/3"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/unsafe_update_items?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "updated-item", "observation": null }
            , { "id": 2, "name": "updated-item", "observation": null }
            , { "id": 3, "name": "updated-item", "observation": null }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "unsafe_update_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "works wihtout filters, taking only the first item in the json array body" $ do
        get "/unsafe_update_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodPatch "/unsafe_update_items"
            [("Prefer", "tx=commit")]
            [json|[
              { "name": "item-all", "observation": "Damaged item" }
            , { "name": "item-3 - 3rd", "observation": null }
            ]|]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "0-2/*"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/unsafe_update_items?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-all", "observation": "Damaged item" }
            , { "id": 2, "name": "item-all", "observation": "Damaged item" }
            , { "id": 3, "name": "item-all", "observation": "Damaged item" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "unsafe_update_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "works without filters and no pk in ?columns, taking only the first item in the json array body" $ do
        get "/unsafe_update_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodPatch "/unsafe_update_items?columns=observation"
            [("Prefer", "tx=commit")]
            [json|[
              { "id": 1, "name": "item-4", "observation": "Damaged item" }
            , { "id": 3, "name": "item-3 - 3rd", "observation": null }
            ]|]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "0-2/*"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/unsafe_update_items?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": "Damaged item" }
            , { "id": 2, "name": "item-2", "observation": "Damaged item" }
            , { "id": 3, "name": "item-3", "observation": "Damaged item" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "unsafe_update_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "works without filters and no composite pk in ?columns, taking only the first item in the json array body" $ do
        get "/unsafe_update_items_cpk"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodPatch "/unsafe_update_items_cpk?columns=id,observation"
            [("Prefer", "tx=commit")]
            [json|[
              { "id": 1, "name": "item-4", "observation": "Damaged item" }
            , { "id": 3, "name": "item-3 - 3rd", "observation": null }
            ]|]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "0-2/*"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/unsafe_update_items_cpk?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": "Damaged item" }
            , { "id": 1, "name": "item-2", "observation": "Damaged item" }
            , { "id": 1, "name": "item-3", "observation": "Damaged item" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "unsafe_update_items_cpk"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "works with no pk defined in the table, taking only the first item in the json array body" $ do
        get "/unsafe_update_items_no_pk"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodPatch "/unsafe_update_items_no_pk"
            [("Prefer", "tx=commit")]
            [json|[
              { "id": 1, "name": "item-1 - 1st", "observation": "Lost item" }
            , { "id": 3, "name": "item-3 - 3rd", "observation": null }
            ]|]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "0-2/*"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/unsafe_update_items_no_pk?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1 - 1st", "observation": "Lost item" }
            , { "id": 1, "name": "item-1 - 1st", "observation": "Lost item" }
            , { "id": 1, "name": "item-1 - 1st", "observation": "Lost item" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "unsafe_update_items_no_pk"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

    context "Full table delete" $ do
      it "works if no condition is present" $ do
        get "/unsafe_delete_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodDelete "/unsafe_delete_items"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/3"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/unsafe_delete_items?order=id"
          `shouldRespondWith`
            [json|[]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "unsafe_delete_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }
