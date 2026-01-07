module Feature.Query.PreferencesSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "test prefer headers and preference-applied headers" $ do

    context "check behaviour of Prefer: handling=strict" $ do
      it "throws error when handling=strict and invalid prefs are given" $
        request methodGet  "/items" [("Prefer", "handling=strict, anything")] ""
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

      it "throw error when handling=strict and invalid prefs are given with multiples in separate prefers" $
        request methodGet  "/items" [("Prefer", "handling=strict"),("Prefer","something, else")] ""
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: something, else","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

      it "throws error with post request" $
        request methodPost  "/organizations?select=*"
          [("Prefer","return=representation, handling=strict, anything")]
          [json|{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}|]
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

      it "throws error with rpc" $
        request methodPost "/rpc/overloaded_unnamed_param"
          [("Content-Type", "application/json"), ("Prefer", "handling=strict, anything")]
          [json|{}|]
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

    context "check behaviour of Prefer: handling=lenient" $ do
      it "does not throw error when handling=lenient and invalid prefs" $
        request methodGet  "/items" [("Prefer", "handling=lenient, anything")] ""
          `shouldRespondWith` 200

      it "does not throw error when handling=lenient and invalid prefs in multiples prefers" $
        request methodGet  "/items" [("Prefer", "handling=lenient"), ("Prefer", "anything")] ""
          `shouldRespondWith` 200

      it "does not throw error with post request" $
        request methodPost  "/organizations?select=*"
          [("Prefer","return=representation, handling=lenient, anything")]
          [json|{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}|]
          `shouldRespondWith`
          [json|[{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}]|]
          { matchStatus  = 201
          , matchHeaders = [ matchContentTypeJson ]
          }

      it "does not throw error with rpc" $
        request methodPost "/rpc/overloaded_unnamed_param"
          [("Content-Type", "application/json"), ("Prefer", "handling=lenient, anything")]
          [json|{}|]
          `shouldRespondWith`
          [json| 1 |]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
           }

    context "test Prefer: timezone=America/Los_Angeles" $ do
      it "should change timezone with handling=strict" $
        request methodGet "/timestamps"
          [("Prefer", "handling=strict, timezone=America/Los_Angeles")]
          ""
          `shouldRespondWith`
          [json|[{"t":"2023-10-18T05:37:59.611-07:00"}, {"t":"2023-10-18T07:37:59.611-07:00"}, {"t":"2023-10-18T09:37:59.611-07:00"}]|]
          { matchStatus = 200
          , matchHeaders = [matchContentTypeJson
                           , "Preference-Applied" <:> "handling=strict, timezone=America/Los_Angeles"]}

      it "should change timezone without handling=strict" $
        request methodGet "/timestamps"
          [("Prefer", "timezone=America/Los_Angeles")]
          ""
          `shouldRespondWith`
          [json|[{"t":"2023-10-18T05:37:59.611-07:00"}, {"t":"2023-10-18T07:37:59.611-07:00"}, {"t":"2023-10-18T09:37:59.611-07:00"}]|]
          { matchStatus = 200
          , matchHeaders = [matchContentTypeJson
                           , "Preference-Applied" <:> "timezone=America/Los_Angeles"] }

    context "test Prefer: timezone=Invalid/Timezone" $ do
      it "should throw error with handling=strict" $
        request methodGet "/timestamps"
          [("Prefer", "handling=strict, timezone=Invalid/Timezone")]
          ""
          `shouldRespondWith`
          [json|{"code":"PGRST122","details":"Invalid preferences: timezone=Invalid/Timezone","hint":null,"message":"Invalid preferences given with handling=strict"}|]
          { matchStatus = 400 }

      it "should return with default timezone without handling or with handling=lenient" $ do
        request methodGet "/timestamps"
          [("Prefer", "timezone=Invalid/Timezone")]
          ""
          `shouldRespondWith`
          [json|[{"t":"2023-10-18T12:37:59.611+00:00"}, {"t":"2023-10-18T14:37:59.611+00:00"}, {"t":"2023-10-18T16:37:59.611+00:00"}]|]
          { matchStatus = 200
          , matchHeaders = [matchContentTypeJson]}

        request methodGet "/timestamps"
          [("Prefer", "handling=lenient, timezone=Invalid/Timezone")]
          ""
          `shouldRespondWith`
          [json|[{"t":"2023-10-18T12:37:59.611+00:00"}, {"t":"2023-10-18T14:37:59.611+00:00"}, {"t":"2023-10-18T16:37:59.611+00:00"}]|]
          { matchStatus = 200
          , matchHeaders = [matchContentTypeJson
                           , "Preference-Applied" <:> "handling=lenient"]}

    context "test Prefer: max-affected with handling=strict" $ do
      it "should fail if items deleted more than 10" $
        request methodDelete "/items?id=lt.15"
          [("Prefer", "handling=strict, max-affected=10")]
          ""
          `shouldRespondWith`
          [json|{"code":"PGRST124","details":"The query affects 14 rows","hint":null,"message":"Query result exceeds max-affected preference constraint"}|]
          { matchStatus = 400 }

      it "should succeed if items deleted less than 10" $
        request methodDelete "/items?id=lt.10"
          [("Prefer", "handling=strict, max-affected=10")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=strict, max-affected=10"]}

      it "should fail if items updated more than 0" $
        request methodPatch "/tiobe_pls?name=eq.Java"
          [("Prefer", "handling=strict, max-affected=0")]
          [json| [{"name":"Java", "rank":19}] |]
          `shouldRespondWith`
          [json|{"code":"PGRST124","details":"The query affects 1 rows","hint":null,"message":"Query result exceeds max-affected preference constraint"}|]
          { matchStatus = 400 }

      it "should succeed if items updated equal 1" $
        request methodDelete "/tiobe_pls?name=eq.Java"
          [("Prefer", "handling=strict, max-affected=1")]
          [json| [{"name":"Java", "rank":19}] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=strict, max-affected=1"]}

    context "test Prefer: max-affected with handling=lenient" $ do
      it "should not fail" $
        request methodDelete "/items?id=lt.15"
          [("Prefer", "handling=lenient, max-affected=10")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]}

      it "should succeed if items deleted less than 10" $
        request methodDelete "/items?id=lt.10"
          [("Prefer", "handling=lenient, max-affected=10")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]}

      it "should not fail" $
        request methodPatch "/tiobe_pls?name=eq.Java"
          [("Prefer", "handling=lenient, max-affected=0")]
          [json| [{"name":"Java", "rank":19}] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]}

      it "should succeed if items updated equal 1" $
        request methodDelete "/tiobe_pls?name=eq.Java"
          [("Prefer", "handling=lenient, max-affected=1")]
          [json| [{"name":"Java", "rank":19}] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]}

    context "test Prefer: max-affected with rpc" $ do
      it "should fail with rpc when deleting rows more than prefered with returns setof" $
        request methodPost "/rpc/delete_items_returns_setof"
          [("Prefer", "handling=strict, max-affected=10")]
          ""
          `shouldRespondWith`
          [json| {"code":"PGRST124","details":"The query affects 15 rows","hint":null,"message":"Query result exceeds max-affected preference constraint"} |]
          { matchStatus = 400 }

      it "should fail with rpc when deleting rows more than prefered with returns table" $
        request methodPost "/rpc/delete_items_returns_table"
          [("Prefer", "handling=strict, max-affected=10")]
          ""
          `shouldRespondWith`
          [json| {"code":"PGRST124","details":"The query affects 15 rows","hint":null,"message":"Query result exceeds max-affected preference constraint"} |]
          { matchStatus = 400 }

      it "should succeed with rpc deleting rows less than prefered with returns setof" $
        request methodPost "/rpc/delete_items_returns_setof"
          [("Prefer", "handling=strict, max-affected=20")]
          ""
          `shouldRespondWith`
          [json|[{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},
                 {"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},
                 {"id":14},{"id":15}]|]
          { matchStatus = 200 }

      it "should succeed with rpc deleting rows less than prefered with returns table" $
        request methodPost "/rpc/delete_items_returns_table"
          [("Prefer", "handling=strict, max-affected=20")]
          ""
          `shouldRespondWith`
          [json|[{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},
                 {"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},
                 {"id":14},{"id":15}]|]
          { matchStatus = 200 }

      it "should fail with rpc when returns void with handling=strict" $
        request methodPost "/rpc/delete_items_returns_void"
          [("Prefer", "handling=strict, max-affected=20")]
          ""
          `shouldRespondWith`
          [json| {"code":"PGRST128","details":null,"hint":null,"message":"Function must return SETOF or TABLE when max-affected preference is used with handling=strict"} |]
          { matchStatus = 400 }

-- | Test Prefer: timeout, we created a separate function for this because
--   it used configRoleSettings with statement_timeout set to different values.
timeoutSpec :: SpecWith ((), Application)
timeoutSpec =
  describe "Prefer: timeout" $ do
    context "Prefer: timeout and handling=strict" $ do
      it "should fail when timeout is less than the query time" $
        request methodGet "/rpc/sleep?seconds=2"
          [("Prefer", "handling=strict, timeout=1")]
          ""
          `shouldRespondWith`
          [json| {"code":"57014","details":null,"hint":null,"message":"canceling statement due to statement timeout"} |]
          { matchStatus = 500
          , matchHeaders = [ matchContentTypeJson ]
          }

      it "should fail when timeout is equal to the query time" $
        request methodGet "/rpc/sleep?seconds=2"
          [("Prefer", "handling=strict, timeout=2")]
          ""
          `shouldRespondWith`
          [json| {"code":"57014","details":null,"hint":null,"message":"canceling statement due to statement timeout"} |]
          { matchStatus = 500
          , matchHeaders = [ matchContentTypeJson ]
          }

      it "should succeed when timeout is more than the query time" $
        request methodGet "/rpc/sleep?seconds=2"
          [("Prefer", "handling=strict, timeout=3")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=strict, timeout=3"]
          }

      context "should fail when timeout is more than role's statement_timeout" $ do
        it "when role timeout is in micro seconds" $ do
          let roleClaim = [json| { "role": "postgrest_test_timeout_us" } |]
              auth = authHeaderJWT $ generateJWT roleClaim
          request methodGet "/rpc/sleep?seconds=2"
            [auth, ("Prefer", "handling=strict, timeout=15")] -- role timeout is 10us, so this should fail
            ""
            `shouldRespondWith`
            [json| {
              "code":"PGRST129",
              "message":"Timeout preference value cannot exceed statement_timeout of role",
              "details":"Timeout preferred: 15s, statement_timeout of role 'postgrest_test_timeout_us': 10us",
              "hint":null
            } |]
            { matchStatus = 400
            , matchHeaders = [matchContentTypeJson]
            }

        it "when role timeout is in milli seconds" $ do
          let roleClaim = [json| { "role": "postgrest_test_timeout_ms" } |]
              auth = authHeaderJWT $ generateJWT roleClaim
          request methodGet "/rpc/sleep?seconds=2"
            [auth, ("Prefer", "handling=strict, timeout=15")] -- role timeout is 10ms, so this should fail
            ""
            `shouldRespondWith`
            [json| {
              "code":"PGRST129",
              "message":"Timeout preference value cannot exceed statement_timeout of role",
              "details":"Timeout preferred: 15s, statement_timeout of role 'postgrest_test_timeout_ms': 10ms",
              "hint":null
            } |]
            { matchStatus = 400
            , matchHeaders = [matchContentTypeJson]
            }

        it "when role timeout is in seconds" $ do
          let roleClaim = [json| { "role": "postgrest_test_timeout_s" } |]
              auth = authHeaderJWT $ generateJWT roleClaim
          request methodGet "/rpc/sleep?seconds=2"
            [auth, ("Prefer", "handling=strict, timeout=15")] -- role timeout is 10s, so this should fail
            ""
            `shouldRespondWith`
            [json| {
              "code":"PGRST129",
              "message":"Timeout preference value cannot exceed statement_timeout of role",
              "details":"Timeout preferred: 15s, statement_timeout of role 'postgrest_test_timeout_s': 10s",
              "hint":null
            } |]
            { matchStatus = 400
            , matchHeaders = [matchContentTypeJson]
            }

        it "when role timeout is in minutes" $ do
          let roleClaim = [json| { "role": "postgrest_test_timeout_min" } |]
              auth = authHeaderJWT $ generateJWT roleClaim
          request methodGet "/rpc/sleep?seconds=2"
            [auth, ("Prefer", "handling=strict, timeout=65")] -- role timeout is 1min, so this should fail
            ""
            `shouldRespondWith`
            [json| {
              "code":"PGRST129",
              "message":"Timeout preference value cannot exceed statement_timeout of role",
              "details":"Timeout preferred: 65s, statement_timeout of role 'postgrest_test_timeout_min': 1min",
              "hint":null
            } |]
            { matchStatus = 400
            , matchHeaders = [matchContentTypeJson]
            }

        it "when role timeout is in hours" $ do
          let roleClaim = [json| { "role": "postgrest_test_timeout_h" } |]
              auth = authHeaderJWT $ generateJWT roleClaim
          request methodGet "/rpc/sleep?seconds=2"
            [auth, ("Prefer", "handling=strict, timeout=3605")] -- role timeout is 1h, so this should fail
            ""
            `shouldRespondWith`
            [json| {
              "code":"PGRST129",
              "message":"Timeout preference value cannot exceed statement_timeout of role",
              "details":"Timeout preferred: 3605s, statement_timeout of role 'postgrest_test_timeout_h': 1h",
              "hint":null
            } |]
            { matchStatus = 400
            , matchHeaders = [matchContentTypeJson]
            }

        it "when role timeout is in days" $ do
          let roleClaim = [json| { "role": "postgrest_test_timeout_d" } |]
              auth = authHeaderJWT $ generateJWT roleClaim
          request methodGet "/rpc/sleep?seconds=2"
            [auth, ("Prefer", "handling=strict, timeout=86405")] -- role timeout is 1d, so this should fail
            ""
            `shouldRespondWith`
            [json| {
              "code":"PGRST129",
              "message":"Timeout preference value cannot exceed statement_timeout of role",
              "details":"Timeout preferred: 86405s, statement_timeout of role 'postgrest_test_timeout_d': 1d",
              "hint":null
            } |]
            { matchStatus = 400
            , matchHeaders = [matchContentTypeJson]
            }

    context "Prefer: timeout and handling=lenient" $
      it "statement timeout is not applied when handling=lenient, so it should succeed in all cases" $ do
        request methodGet "/rpc/sleep?seconds=2"
          [("Prefer", "handling=lenient, timeout=1")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]
          }

        request methodGet "/rpc/sleep?seconds=2"
          [("Prefer", "handling=lenient, timeout=2")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]
          }

        request methodGet "/rpc/sleep?seconds=2"
          [("Prefer", "handling=lenient, timeout=3")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]
          }
