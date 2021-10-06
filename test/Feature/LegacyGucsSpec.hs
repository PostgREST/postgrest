module Feature.LegacyGucsSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "remote procedure call with legacy gucs disabled" $ do
    it "custom header is set" $
      request methodPost "/rpc/get_guc_value" [("Custom-Header", "test")]
        [json| { "prefix": "request.headers", "name": "custom-header" } |]
          `shouldRespondWith`
          [json|"test"|]
          { matchStatus  = 200
          , matchHeaders = [ matchContentTypeJson ]
          }

    it "standard header is set" $
      request methodPost "/rpc/get_guc_value" [("Origin", "http://example.com")]
        [json| { "prefix": "request.headers", "name": "origin" } |]
          `shouldRespondWith`
          [json|"http://example.com"|]
          { matchStatus  = 200
          , matchHeaders = [ matchContentTypeJson ]
          }

    it "current role is available as GUC claim" $
      request methodPost "/rpc/get_guc_value" []
        [json| { "prefix": "request.jwt.claims", "name": "role" } |]
          `shouldRespondWith`
          [json|"postgrest_test_anonymous"|]
          { matchStatus  = 200
          , matchHeaders = [ matchContentTypeJson ]
          }

    it "single cookie ends up as claims" $
      request methodPost "/rpc/get_guc_value" [("Cookie","acookie=cookievalue")]
        [json| {"prefix": "request.cookies", "name":"acookie"} |]
          `shouldRespondWith`
          [json|"cookievalue"|]
          { matchStatus = 200
          , matchHeaders = []
          }

    it "multiple cookies ends up as claims" $
      request methodPost "/rpc/get_guc_value" [("Cookie","acookie=cookievalue;secondcookie=anothervalue")]
        [json| {"prefix": "request.cookies", "name":"secondcookie"} |]
          `shouldRespondWith`
          [json|"anothervalue"|]
          { matchStatus = 200
          , matchHeaders = []
          }

    it "gets the Authorization value" $
      request methodPost "/rpc/get_guc_value" [authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIn0.Xod-F15qsGL0WhdOCr2j3DdKuTw9QJERVgoFD3vGaWA"]
        [json| {"prefix": "request.headers", "name":"authorization"} |]
          `shouldRespondWith`
          [json|"Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIn0.Xod-F15qsGL0WhdOCr2j3DdKuTw9QJERVgoFD3vGaWA"|]
          { matchStatus = 200
          , matchHeaders = []
          }
