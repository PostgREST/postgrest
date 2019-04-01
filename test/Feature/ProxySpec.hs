module Feature.ProxySpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai

import SpecHelper

import Network.Wai (Application)

import Protolude

spec :: SpecWith Application
spec =
  describe "GET / with proxy" $
    it "returns a valid openapi spec with proxy" $ do
      pendingWith "Test timing out frequently on CI, please run locally"
      validateOpenApiResponse [("Accept", "application/openapi+json")]
