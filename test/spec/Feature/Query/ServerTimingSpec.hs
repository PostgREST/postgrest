module Feature.Query.ServerTimingSpec where

import Network.HTTP.Types
import Protolude hiding (get)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config (AppConfig (..))
import SpecHelper

spec :: SpecWithConfig
spec withConfig = withConfig (baseCfg{configDbPlanEnabled = True}) $
  describe "Show Duration on Server-Timing header" $ do
    context "responds with Server-Timing header" $ do
      it "works with get request" $ do
        request
          methodGet
          "/organizations?id=eq.6"
          []
          ""
          `shouldRespondWith` [json|[{"id":6,"name":"Oscorp","referee":3,"auditor":4,"manager_id":6}]|]
            { matchStatus = 200
            , matchHeaders = matchContentTypeJson : map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

      it "works with post request" $
        request
          methodPost
          "/organizations?select=*"
          [("Prefer", "return=representation")]
          [json|{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}|]
          `shouldRespondWith` [json|[{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}]|]
            { matchStatus = 201
            , matchHeaders = matchContentTypeJson : map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

      it "works with patch request" $
        request
          methodPatch
          "/no_pk?b=eq.0"
          mempty
          [json| { b: "1" } |]
          `shouldRespondWith` ""
            { matchStatus = 204
            , matchHeaders = matchHeaderAbsent hContentType : map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

      it "works with put request" $
        request
          methodPut
          "/tiobe_pls?name=eq.Python"
          [("Prefer", "return=representation")]
          [json| [ { "name": "Python", "rank": 19 } ]|]
          `shouldRespondWith` [json| [ { "name": "Python", "rank": 19 } ]|]
            { matchStatus = 200
            , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

      it "works with delete request" $
        request
          methodDelete
          "/items?id=eq.1"
          []
          ""
          `shouldRespondWith` ""
            { matchStatus = 204
            , matchHeaders = matchHeaderAbsent hContentType : map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

      it "works with rpc call" $
        request
          methodPost
          "/rpc/ret_point_overloaded"
          []
          [json|{"x": 1, "y": 2}|]
          `shouldRespondWith` [json|{"x": 1, "y": 2}|]
            { matchStatus = 200
            , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

      it "works with root spec" $
        request
          methodHead
          "/"
          []
          ""
          `shouldRespondWith` ""
            { matchStatus = 200
            , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
            }

      it "works with OPTIONS method" $ do
        request
          methodOptions
          "/organizations"
          []
          ""
          `shouldRespondWith` ""
            { matchStatus = 200
            , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "response"]
            }
        request
          methodOptions
          "/rpc/getallprojects"
          []
          ""
          `shouldRespondWith` ""
            { matchStatus = 200
            , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "response"]
            }
        request
          methodOptions
          "/"
          []
          ""
          `shouldRespondWith` ""
            { matchStatus = 200
            , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "response"]
            }

    context "responds with Server-Timing header on errors" $ do
      it "has the timings of the steps run up to a JWT error" $
        request
          methodGet
          "/organizations"
          [authHeaderJWT "invalid"]
          ""
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 401
            , matchBody = MatchBody (\_ _ -> Nothing) -- match any body
            , matchHeaders =
                matchServerTimingHasTiming "jwt"
                  : map matchServerTimingHasNoTiming ["parse", "plan", "transaction", "response"]
            }

      it "has the timings of the steps run up to a parse error" $
        request
          methodGet
          "/organizations?id=invalid"
          []
          ""
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 400
            , matchBody = MatchBody (\_ _ -> Nothing) -- match any body
            , matchHeaders =
                map matchServerTimingHasTiming ["jwt", "parse"]
                  <> map matchServerTimingHasNoTiming ["plan", "transaction", "response"]
            }

      it "has the timings of the steps run up to a database error" $
        request
          methodGet
          "/organizations?id=eq.invalid"
          []
          ""
          `shouldRespondWith` ResponseMatcher
            { matchStatus = 400
            , matchBody = MatchBody (\_ _ -> Nothing) -- match any body
            , matchHeaders =
                map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction"]
                  <> [matchServerTimingHasNoTiming "response"]
            }
