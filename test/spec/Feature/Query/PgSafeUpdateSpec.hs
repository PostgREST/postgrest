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
            [("Prefer", "count=exact")]
            [json| {"name": "New name"} |]
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
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json| {"name": "updated-item"} |]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "0-2/3"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/safe_update_items?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "updated-item", "observation": null }
            , { "id": 2, "name": "updated-item", "observation": null }
            , { "id": 3, "name": "updated-item", "observation": null }
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
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            mempty
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
