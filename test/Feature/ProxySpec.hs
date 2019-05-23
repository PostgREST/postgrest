module Feature.ProxySpec where

import Network.Wai (Application)
import Test.Hspec  hiding (pendingWith)

import Test.Hspec.Wai

import Protolude
import SpecHelper

spec :: SpecWith Application
spec =
  describe "GET / with proxy" $
    it "returns a valid openapi spec with proxy" $ do
      pendingWith "Test timing out frequently on CI, please run locally"
      validateOpenApiResponse [("Accept", "application/openapi+json")]
