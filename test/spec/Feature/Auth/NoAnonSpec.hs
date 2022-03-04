module Feature.Auth.NoAnonSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "server started without anonymous role" $ do
  it "behaves normally on attempted auth" $ do
    -- token body: { "role": "postgrest_test_author" }
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIn0.Xod-F15qsGL0WhdOCr2j3DdKuTw9QJERVgoFD3vGaWA"
    request methodGet "/authors_only"
        [auth]
        ""
      `shouldRespondWith`
        200

  it "responds with error when user does not attempt auth" $
    get "/items"
      `shouldRespondWith`
        [json|
          {"hint": null,
           "details": null,
           "code": "PGRST302",
           "message":"Anonymous access is disabled"}|]
        { matchStatus  = 401
        , matchHeaders = ["WWW-Authenticate" <:> "Bearer"]
        }
