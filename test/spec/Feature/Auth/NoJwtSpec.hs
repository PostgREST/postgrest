module Feature.Auth.NoJwtSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "server started without JWT secret" $ do

  it "responds with error on attempted auth" $ do
    -- token body: { "role": "postgrest_test_author" }
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIn0.Xod-F15qsGL0WhdOCr2j3DdKuTw9QJERVgoFD3vGaWA"
    request methodGet "/authors_only"
        [auth]
        ""
      `shouldRespondWith`
        [json|
          {"hint": null,
           "details": null,
           "code": "PGRST300",
           "message": "Server lacks JWT secret"}|]
        { matchStatus  = 500 }

  it "behaves normally when user does not attempt auth" $
    get "/items" `shouldRespondWith` 200
