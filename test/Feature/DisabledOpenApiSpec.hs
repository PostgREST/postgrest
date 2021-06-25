module Feature.DisabledOpenApiSpec where

import Network.HTTP.Types
import Network.Wai        (Application)

import Test.Hspec     hiding (pendingWith)
import Test.Hspec.Wai

import Protolude

spec :: SpecWith ((), Application)
spec =
  describe "Disabled OpenApi" $ do
    it "does not accept application/openapi+json and responds with 415" $
      request methodGet "/"
        [("Accept","application/openapi+json")] "" `shouldRespondWith` 415

    it "accepts application/json and responds with 404" $
      request methodGet "/"
        [("Accept","application/json")] "" `shouldRespondWith` 404
