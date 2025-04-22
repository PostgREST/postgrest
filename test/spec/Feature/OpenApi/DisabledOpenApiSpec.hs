module Feature.OpenApi.DisabledOpenApiSpec where

import Network.HTTP.Types
import Network.Wai        (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude

spec :: SpecWith ((), Application)
spec =
  describe "Disabled OpenApi" $ do
    it "responds with 404" $
      request methodGet "/"
        [("Accept","application/openapi+json")] ""
        `shouldRespondWith`
        [json| {"code":"PGRST126","details":null,"hint":null,"message":"Root endpoint metadata is disabled"} |]
        { matchStatus = 404
        , matchHeaders = ["Content-Length" <:> "93"]}
