module Feature.OpenApi.DisabledOpenApiSpec where

import Network.HTTP.Types
import Network.Wai        (Application)

import Test.Hspec     hiding (pendingWith)
import Test.Hspec.Wai

import Protolude

spec :: SpecWith ((), Application)
spec =
  describe "Disabled OpenApi" $ do
    it "responds with 404" $
      request methodGet "/"
        [("Accept","application/openapi+json")] "" `shouldRespondWith` 404
