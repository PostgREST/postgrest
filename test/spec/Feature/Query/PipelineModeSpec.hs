module Feature.Query.PipelineModeSpec where

import Network.HTTP.Types
import Protolude hiding (get)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config (AppConfig (..), OpenAPIMode (..))
import SpecHelper

spec :: SpecWithConfig
spec withConfig = do
  withConfig baseCfg{configDbPipelineMode = True} $
    describe "db-pipeline-mode = true" $ do
      let singular = ("Accept", "application/vnd.pgrst.object+json")

      context "static-terminator path (no FailWhen steps)" $ do
        it "handles a simple GET" $
          get "/items?id=eq.5"
            `shouldRespondWith` [json| [{"id":5}] |]
              { matchHeaders = ["Content-Range" <:> "0-0/*"]
              }

        it "handles a HEAD" $
          request methodHead "/items?id=eq.5" [] ""
            `shouldRespondWith` ""
              { matchStatus = 200
              }

        it "handles a POST (rolled back by test config)" $
          request
            methodPost
            "/menagerie"
            [("Prefer", "return=representation")]
            [json| [{
              "integer": 42, "double": 3.14, "varchar": "pipeline"
            , "boolean": true, "date": "1900-01-01", "money": "$1.00"
            , "enum": "foo"
            }] |]
            `shouldRespondWith` 201

        it "handles a PATCH with return=representation" $
          request
            methodPatch
            "/items?id=eq.1"
            [("Prefer", "return=representation")]
            [json| { "id": 1 } |]
            `shouldRespondWith` [json| [{"id":1}] |]
              { matchStatus = 200
              }

        it "handles a DELETE" $
          request methodDelete "/items?id=eq.1" [] ""
            `shouldRespondWith` ""
              { matchStatus = 204
              }

        it "honors Prefer: tx=commit on a read (no-op commit, no mutation)" $
          request methodGet "/items?id=eq.5" [("Prefer", "tx=commit")] ""
            `shouldRespondWith` [json| [{"id":5}] |]
              { matchStatus = 200
              , matchHeaders = ["Preference-Applied" <:> "tx=commit"]
              }

        it "honors Prefer: tx=rollback explicitly on a read" $
          request methodGet "/items?id=eq.5" [("Prefer", "tx=rollback")] ""
            `shouldRespondWith` [json| [{"id":5}] |]
              { matchStatus = 200
              , matchHeaders = ["Preference-Applied" <:> "tx=rollback"]
              }

        it "propagates a routing error (no DB round trip)" $
          get "/faketable"
            `shouldRespondWith` 404

        it "invokes an RPC" $
          get "/rpc/is_superuser"
            `shouldRespondWith` "false"
              { matchStatus = 200
              }

        it "serves the OpenAPI root under pipeline mode" $
          request methodGet "/" (acceptHdrs "application/openapi+json") ""
            `shouldRespondWith` 200
              { matchHeaders = ["Content-Type" <:> "application/openapi+json; charset=utf-8"]
              }

      context "dynamic-terminator path (FailWhen steps inspect the result set)" $ do
        it "resolves singular JSON on a one-row GET" $
          request methodGet "/items?id=eq.5" [singular] ""
            `shouldRespondWith` [json|{"id":5}|]
              { matchStatus = 200
              , matchHeaders = [matchContentTypeSingular]
              }

        it "aborts on singular JSON when zero rows match" $
          request methodGet "/items?id=gt.0&id=lt.0" [singular] ""
            `shouldRespondWith` 406

        it "aborts on singular JSON when more than one row matches" $
          request methodGet "/items?id=lt.3" [singular] ""
            `shouldRespondWith` 406

        it "aborts a PATCH requesting singular when zero rows match" $
          request
            methodPatch
            "/items?id=gt.0&id=lt.0"
            [("Prefer", "return=representation"), singular]
            [json| { "id": 0 } |]
            `shouldRespondWith` 406

        it "accepts a PUT where the URL pk matches the payload pk" $ do
          get "/tiobe_pls?name=eq.Go"
            `shouldRespondWith` [json|[]|]
          request
            methodPut
            "/tiobe_pls?name=eq.Go"
            [("Prefer", "return=representation")]
            [json| [ { "name": "Go", "rank": 19 } ]|]
            `shouldRespondWith` [json| [ { "name": "Go", "rank": 19 } ]|]
              { matchStatus = 201
              }

        it "rejects a PUT where the URL pk does not match the payload pk" $
          request
            methodPut
            "/tiobe_pls?name=eq.MATLAB"
            []
            [json| [ { "name": "Perl", "rank": 17 } ]|]
            `shouldRespondWith` [json|{"message":"Payload values do not match URL in primary key column(s)","code":"PGRST115","details":null,"hint":null}|]
              { matchStatus = 400
              }

        it "aborts a DELETE when max-affected (strict) is exceeded" $
          request
            methodDelete
            "/items?id=lt.15"
            [("Prefer", "handling=strict, max-affected=10")]
            ""
            `shouldRespondWith` [json|{"code":"PGRST124","details":"The query affects 14 rows","hint":null,"message":"Query result exceeds max-affected preference constraint"}|]
              { matchStatus = 400
              }

        it "accepts a DELETE when max-affected (strict) is honored" $
          request
            methodDelete
            "/items?id=lt.10"
            [("Prefer", "handling=strict, max-affected=10")]
            ""
            `shouldRespondWith` ""
              { matchStatus = 204
              , matchHeaders = ["Preference-Applied" <:> "handling=strict, max-affected=10"]
              }

        it "aborts an RPC requesting singular when it yields no row (MaybeRollback is before the check)" $
          request methodGet "/rpc/is_superuser" [singular] ""
            `shouldRespondWith` [json|false|]
              { matchStatus = 200
              , matchHeaders = [matchContentTypeSingular]
              }

        it "aborts a set-returning RPC requesting singular when it yields zero rows" $
          -- Forces the CallReadPlan default `emptyResultSet (Just 0)` to be
          -- evaluated by the FailWhen singular check, exercising the
          -- non-Nothing branch of emptyResultSet that the other singular tests
          -- (one-row RPC, zero-row PATCH) do not reach.
          request methodGet "/rpc/test_empty_rowset" [singular] ""
            `shouldRespondWith` [json|{"details":"The result contains 0 rows","message":"Cannot coerce the result to a single JSON object","code":"PGRST116","hint":null}|]
              { matchStatus = 406
              }

      context "DB-level errors (surface PipelineError paths)" $ do
        it "surfaces a RAISE sqlstate error from an RPC" $
          get "/rpc/raise_pt402"
            `shouldRespondWith` [json| {"code":"PT402","details":"Quota exceeded","hint":"Upgrade your plan","message":"Payment Required"} |]
              { matchStatus = 402
              }

        it "surfaces a unique-constraint violation on INSERT" $
          post "/simple_pk" [json| { "k":"xyyx", "extra":"e1" } |]
            `shouldRespondWith` [json|{"hint":null,"details":"Key (k)=(xyyx) already exists.","code":"23505","message":"duplicate key value violates unique constraint \"simple_pk_pkey\""}|]
              { matchStatus = 409
              }

  withConfig baseCfg{configDbPipelineMode = True, configOpenApiMode = OADisabled} $
    describe "db-pipeline-mode = true with openapi-mode = disabled" $
      it "returns 404 for the OpenAPI root under pipeline mode" $
        request
          methodGet
          "/"
          [("Accept", "application/openapi+json")]
          ""
          `shouldRespondWith` [json| {"code":"PGRST126","details":null,"hint":null,"message":"Root endpoint metadata is disabled"} |]
            { matchStatus = 404
            }
