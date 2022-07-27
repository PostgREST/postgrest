module Feature.Query.DeleteSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

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

      it "works with a simple primary key filter in the json body" $ do
        get "/body_delete_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodDelete "/body_delete_items"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json| { "id": 3 } |]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/1"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/body_delete_items?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "body_delete_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "works with a composite primary key filter in the json body" $ do
        get "/body_delete_items_cpk"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodDelete "/body_delete_items_cpk"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json| { "id": 3, "name": "item-3" } |]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/1"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/body_delete_items_cpk?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "body_delete_items_cpk"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      context "with ?columns parameter" $ do
        context "with simple pk" $ do
          it "ignores json keys not included in ?columns" $ do
            get "/body_delete_items"
              `shouldRespondWith`
                [json|[
                  { "id": 1, "name": "item-1", "observation": null }
                , { "id": 2, "name": "item-2", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodDelete "/body_delete_items?columns=name"
                [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
                [json|{ "id": 1, "name": "item-2", "observation": null, "other": "value" }|]
              `shouldRespondWith`
                ""
                { matchStatus  = 204
                , matchHeaders = [ matchHeaderAbsent hContentType
                                 , "Content-Range" <:> "*/1"
                                 , "Preference-Applied" <:> "tx=commit" ]
                }

            get "/body_delete_items?order=id"
              `shouldRespondWith`
                [json|[
                  { "id": 1, "name": "item-1", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodPost "/rpc/reset_items_tables"
              [("Prefer", "tx=commit")]
              [json| {"tbl_name": "body_delete_items"} |]
              `shouldRespondWith` ""
              { matchStatus  = 204 }

          it "ignores body and columns if a filter is present" $ do
            get "/body_delete_items"
              `shouldRespondWith`
                [json|[
                  { "id": 1, "name": "item-1", "observation": null }
                , { "id": 2, "name": "item-2", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodDelete "/body_delete_items?id=eq.2&columns=id"
                [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
                [json|{ "id": 1, "name": "item-1" }|]
              `shouldRespondWith`
                ""
                { matchStatus  = 204
                , matchHeaders = [ matchHeaderAbsent hContentType
                                 , "Content-Range" <:> "*/1"
                                 , "Preference-Applied" <:> "tx=commit" ]
                }

            get "/body_delete_items?order=id"
              `shouldRespondWith`
                [json|[
                  { "id": 1, "name": "item-1", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodPost "/rpc/reset_items_tables"
              [("Prefer", "tx=commit")]
              [json| {"tbl_name": "body_delete_items"} |]
              `shouldRespondWith` ""
              { matchStatus  = 204 }

        context "with composite pk" $ do
          it "ignores json keys not included in ?columns" $ do
            get "/body_delete_items_cpk"
              `shouldRespondWith`
                [json|[
                  { "id": 1, "name": "item-1", "observation": null }
                , { "id": 2, "name": "item-2", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodDelete "/body_delete_items_cpk?columns=id,name"
                [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
                [json|{ "id": 1, "name": "item-1", "observation": "an observation", "other": "value" }|]
              `shouldRespondWith`
                ""
                { matchStatus  = 204
                , matchHeaders = [ matchHeaderAbsent hContentType
                                 , "Content-Range" <:> "*/1"
                                 , "Preference-Applied" <:> "tx=commit" ]
                }

            get "/body_delete_items_cpk?order=id"
              `shouldRespondWith`
                [json|[
                  { "id": 2, "name": "item-2", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodPost "/rpc/reset_items_tables"
              [("Prefer", "tx=commit")]
              [json| {"tbl_name": "body_delete_items_cpk"} |]
              `shouldRespondWith` ""
              { matchStatus  = 204 }

          it "ignores body and columns if a filter is present" $ do
            get "/body_delete_items_cpk"
              `shouldRespondWith`
                [json|[
                  { "id": 1, "name": "item-1", "observation": null }
                , { "id": 2, "name": "item-2", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodDelete "/body_delete_items_cpk?id=eq.2&columns=id,name"
                [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
                [json|{ "id": 1, "name": "item-1" }|]
              `shouldRespondWith`
                ""
                { matchStatus  = 204
                , matchHeaders = [ matchHeaderAbsent hContentType
                                 , "Content-Range" <:> "*/1"
                                 , "Preference-Applied" <:> "tx=commit" ]
                }

            get "/body_delete_items_cpk?order=id"
              `shouldRespondWith`
                [json|[
                  { "id": 1, "name": "item-1", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodPost "/rpc/reset_items_tables"
              [("Prefer", "tx=commit")]
              [json| {"tbl_name": "body_delete_items_cpk"} |]
              `shouldRespondWith` ""
              { matchStatus  = 204 }

    context "deleting with an empty/missing body and without filters and limits" $ do
      it "makes no deletes and returns 422 without return= and without ?select=" $ do
        request methodDelete "/items"
            []
            mempty
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

        request methodDelete "/items"
            []
            [json| {} |]
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

        request methodDelete "/items"
            []
            [json| [] |]
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

        request methodDelete "/items"
            []
            [json| [{}] |]
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

      it "makes no deletes and returns 422 without return= and with ?select=" $ do
        request methodDelete "/items?select=id"
            []
            mempty
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

        request methodDelete "/items?select=id"
            []
            [json| {} |]
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

        request methodDelete "/items?select=id"
            []
            [json| [] |]
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

        request methodDelete "/items?select=id"
            []
            [json| [{}] |]
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

      it "makes no deletes and returns 422 with return=rep and without ?select=" $
        request methodDelete "/items" [("Prefer", "return=representation")] [json| {} |]
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

      it "makes no deletes and returns 422 with return=rep and with ?select=" $
        request methodDelete "/items?select=id" [("Prefer", "return=representation")] [json| {} |]
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

      it "makes no deletes and returns 422 with return=rep and with ?select= for overloaded computed columns" $
        request methodDelete "/items?select=id,computed_overload" [("Prefer", "return=representation")] [json| {} |]
          `shouldRespondWith`
            [json|{
              "code":"PGRST118",
              "hint":"Filter the request by sending primary keys in the body or by using filters or limits in the query string.",
              "details":null,
              "message":"A full delete without a body, filters or limits is not allowed"
              }|]
            { matchStatus  = 422
            , matchHeaders = [matchContentTypeJson]
            }

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
      it "works with the limit and offset query params" $ do
        get "/limited_delete_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 2, "name": "item-2" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodDelete "/limited_delete_items?order=id&limit=1&offset=1"
            [("Prefer", "tx=commit")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/limited_delete_items?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "limited_delete_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "works with the limit query param plus a filter" $ do
        get "/limited_delete_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 2, "name": "item-2" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodDelete "/limited_delete_items?order=id&limit=1&id=gt.1"
            [("Prefer", "tx=commit")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/limited_delete_items?order=id"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "limited_delete_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

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

      it "works with views with an explicit order by unique col" $ do
        get "/limited_delete_items_view"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 2, "name": "item-2" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodDelete "/limited_delete_items_view?order=id&limit=1&offset=1"
            [("Prefer", "tx=commit")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/limited_delete_items_view"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "limited_delete_items_view"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "works with views with an explicit order by composite pk" $ do
        get "/limited_delete_items_cpk_view"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 2, "name": "item-2" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodDelete "/limited_delete_items_cpk_view?order=id,name&limit=1&offset=1"
            [("Prefer", "tx=commit")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/limited_delete_items_cpk_view"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "limited_delete_items_cpk_view"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "works on a table without a pk by ordering by 'ctid'" $ do
        get "/limited_delete_items_no_pk"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 2, "name": "item-2" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodDelete "/limited_delete_items_no_pk?order=ctid&limit=1&offset=1"
            [("Prefer", "tx=commit")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/limited_delete_items_no_pk"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "limited_delete_items_no_pk"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "works ignoring the json body" $ do
        get "/limited_delete_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 2, "name": "item-2" }
            , { "id": 3, "name": "item-3" }
            ]|]

        request methodDelete "/limited_delete_items?limit=2&offset=1&order=id"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json|[
              { "id": 1 }
            , { "id": 2 }
            ]|]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/2"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/limited_delete_items?order=id"
          `shouldRespondWith`
            [json|[{ "id": 1, "name": "item-1" }]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "limited_delete_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

    context "bulk deletes" $ do
      it "can delete tables with simple pk" $ do
        get "/bulk_delete_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodDelete "/bulk_delete_items"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json|[
              { "id": 1 }
            , { "id": 3 }
            ]|]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/2"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/bulk_delete_items?order=id"
          `shouldRespondWith`
            [json|[{ "id": 2, "name": "item-2", "observation": null }]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "bulk_delete_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "can delete tables with composite pk" $ do
        get "/bulk_delete_items_cpk"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodDelete "/bulk_delete_items_cpk"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 2, "name": "item-2" }
            ]|]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/2"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/bulk_delete_items_cpk?order=id"
          `shouldRespondWith`
            [json|[{ "id": 3, "name": "item-3", "observation": null }]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "bulk_delete_items_cpk"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "deletes with filters and ignores the json array body" $ do
        get "/bulk_delete_items"
          `shouldRespondWith`
            [json|[
              { "id": 1, "name": "item-1", "observation": null }
            , { "id": 2, "name": "item-2", "observation": null }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]

        request methodDelete "/bulk_delete_items?name=in.(item-1,item-2)&observation=is.null"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json|[
              { "id": 2 }
            , { "id": 3 }
            ]|]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/2"
                             , "Preference-Applied" <:> "tx=commit" ]
            }

        get "/bulk_delete_items?order=id"
          `shouldRespondWith`
            [json|[ { "id": 3, "name": "item-3", "observation": null } ]|]

        request methodPost "/rpc/reset_items_tables"
          [("Prefer", "tx=commit")]
          [json| {"tbl_name": "bulk_delete_items"} |]
          `shouldRespondWith` ""
          { matchStatus  = 204 }

      it "rejects a json array that isn't exclusively composed of objects" $
        request methodDelete "/bulk_delete_items"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json|[
              { "id": 1 }
            , 2
            , "item-2"
            , { "id": 3, "name": "item-3" }
            ]|]
          `shouldRespondWith`
            [json|{
              "code":"PGRST102",
              "hint":null,
              "details":null,
              "message":"All object keys must match"
              }|]
            { matchStatus  = 400
            , matchHeaders = [matchContentTypeJson]
            }

      it "rejects a json array that has objects with different keys" $ do
        request methodDelete "/bulk_delete_items_cpk"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json|[
              { "id": 1, "name": "item-1" }
            , { "id": 2 }
            , { "other": "value" }
            , { "id": 3, "name": "item-3", "observation": null }
            ]|]
          `shouldRespondWith`
            [json|{
              "code":"PGRST102",
              "hint":null,
              "details":null,
              "message":"All object keys must match"
              }|]
            { matchStatus  = 400
            , matchHeaders = [matchContentTypeJson]
            }

      context "with ?columns parameter" $ do
        context "with simple pk" $ do
          it "ignores json keys not included in ?columns" $ do
            get "/bulk_delete_items"
              `shouldRespondWith`
                [json|[
                  { "id": 1, "name": "item-1", "observation": null }
                , { "id": 2, "name": "item-2", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodDelete "/bulk_delete_items?columns=name"
                [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
                [json|[
                  { "id": 2, "name": "item-1" }
                , { "id": 3 }
                , { "name": "item-2" }
                , { "observation": null }
                ]|]
              `shouldRespondWith`
                ""
                { matchStatus  = 204
                , matchHeaders = [ matchHeaderAbsent hContentType
                                 , "Content-Range" <:> "*/2"
                                 , "Preference-Applied" <:> "tx=commit" ]
                }

            get "/bulk_delete_items?order=id"
              `shouldRespondWith`
                [json|[{ "id": 3, "name": "item-3", "observation": null }]|]

            request methodPost "/rpc/reset_items_tables"
              [("Prefer", "tx=commit")]
              [json| {"tbl_name": "bulk_delete_items"} |]
              `shouldRespondWith` ""
              { matchStatus  = 204 }

        context "with composite pk" $ do

          it "ignores json keys not included in ?columns" $ do
            get "/bulk_delete_items_cpk"
              `shouldRespondWith`
                [json|[
                  { "id": 1, "name": "item-1", "observation": null }
                , { "id": 2, "name": "item-2", "observation": null }
                , { "id": 3, "name": "item-3", "observation": null }
                ]|]

            request methodDelete "/bulk_delete_items_cpk?columns=id"
                [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
                [json|[
                  { "id": 1, "name": "item-4", "observation": "an observation" }
                , { "id": 3, "name": "item-x" }
                , { "name": "item-2" }
                , { "observation": null }
                ]|]
              `shouldRespondWith`
                ""
                { matchStatus  = 204
                , matchHeaders = [ matchHeaderAbsent hContentType
                                 , "Content-Range" <:> "*/2"
                                 , "Preference-Applied" <:> "tx=commit" ]
                }

            get "/bulk_delete_items_cpk?order=id"
              `shouldRespondWith`
                [json|[{ "id": 2, "name": "item-2", "observation": null }]|]

            request methodPost "/rpc/reset_items_tables"
              [("Prefer", "tx=commit")]
              [json| {"tbl_name": "bulk_delete_items_cpk"} |]
              `shouldRespondWith` ""
              { matchStatus  = 204 }
