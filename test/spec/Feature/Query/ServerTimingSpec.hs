module Feature.Query.ServerTimingSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "Show Duration on Server-Timing header" $ do

    context "responds with Server-Timing header" $ do
      it "works with get request" $ do
        request methodGet "/organizations?id=eq.6" [] ""
          `shouldRespondWith`
          [json|[{"id":6,"name":"Oscorp","referee":3,"auditor":4,"manager_id":6}]|]
          { matchStatus  = 200
          , matchHeaders = matchContentTypeJson : map matchServerTimingHasTiming ["jwt","parse","plan","transaction","response"]
          }

        request methodGet "/organizations?id=eq.6"
          [("Prefer", "metrics=timings")] ""
          `shouldRespondWith`
          [json|[{"id":6,"name":"Oscorp","referee":3,"auditor":4,"manager_id":6}]|]
          { matchStatus  = 200
          , matchHeaders = ("Preference-Applied" <:> "metrics=timings")
                         : matchContentTypeJson
                         : map matchServerTimingHasTiming ["jwt","parse","plan","transaction","response"]
          }

      it "works with post request" $ do
        request methodPost  "/organizations?select=*"
          [("Prefer","return=representation")]
          [json|{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}|]
          `shouldRespondWith`
          [json|[{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}]|]
          { matchStatus  = 201
          , matchHeaders = matchContentTypeJson : map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
          }

        request methodPost  "/organizations?select=*"
          [("Prefer","return=representation"), ("Prefer", "metrics=timings")]
          [json|{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}|]
          `shouldRespondWith`
          [json|[{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}]|]
          { matchStatus  = 201
          , matchHeaders = ("Preference-Applied" <:> "return=representation, metrics=timings")
                         : matchContentTypeJson
                         : map matchServerTimingHasTiming ["jwt","parse","plan","transaction","response"]
          }

      it "works with patch request" $ do
        request methodPatch "/no_pk?b=eq.0" mempty
            [json| { b: "1" } |]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = matchHeaderAbsent hContentType
                           : map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

        request methodPatch "/no_pk?b=eq.0" [("Prefer", "metrics=timings")]
            [json| { b: "1" } |]
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = ("Preference-Applied" <:> "metrics=timings")
                           : matchHeaderAbsent hContentType
                           : map matchServerTimingHasTiming ["jwt","parse","plan","transaction","response"]
            }

      it "works with put request" $ do
        request methodPut "/tiobe_pls?name=eq.Python"
            [("Prefer", "return=representation")]
            [json| [ { "name": "Python", "rank": 19 } ]|]
          `shouldRespondWith`
            [json| [ { "name": "Python", "rank": 19 } ]|]
            { matchStatus  = 200
            , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

        request methodPut "/tiobe_pls?name=eq.Python"
            [("Prefer", "return=representation"),("Prefer", "metrics=timings")]
            [json| [ { "name": "Python", "rank": 19 } ]|]
          `shouldRespondWith`
            [json| [ { "name": "Python", "rank": 19 } ]|]
            { matchStatus  = 200
            , matchHeaders = ("Preference-Applied" <:> "return=representation, metrics=timings")
                           : map matchServerTimingHasTiming ["jwt","parse","plan","transaction","response"]
            }

      it "works with delete request" $ do
        request methodDelete "/items?id=eq.1" [] ""
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = matchHeaderAbsent hContentType : map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

        request methodDelete "/items?id=eq.1"
          [("Prefer", "metrics=timings")] ""
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = ("Preference-Applied" <:> "metrics=timings")
                           : matchHeaderAbsent hContentType
                           : map matchServerTimingHasTiming ["jwt","parse","plan","transaction","response"]
            }

      it "works with rpc call" $ do
        request methodPost "/rpc/ret_point_overloaded"
          []
          [json|{"x": 1, "y": 2}|]
          `shouldRespondWith`
          [json|{"x": 1, "y": 2}|]
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
          }

        request methodPost "/rpc/ret_point_overloaded"
          [("Prefer", "metrics=timings")]
          [json|{"x": 1, "y": 2}|]
          `shouldRespondWith`
          [json|{"x": 1, "y": 2}|]
          { matchStatus  = 200
          , matchHeaders = ("Preference-Applied" <:> "metrics=timings")
                         : map matchServerTimingHasTiming ["jwt","parse","plan","transaction","response"]
          }

      it "works with root spec" $
        request methodHead "/" [] ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
          }

      it "works with OPTIONS method" $ do
        request methodOptions "/organizations" [] ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "response"]
          }

        request methodOptions "/rpc/getallprojects" [] ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "response"]
          }

        request methodOptions "/" [] ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "response"]
          }
