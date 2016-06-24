module Feature.ProxySpec where

import Test.Hspec hiding (pendingWith)

import SpecHelper

import Network.Wai (Application)

spec :: SpecWith Application
spec =
  describe "GET / with proxy" $
    it "returns a valid openapi spec with proxy" $
      validateOpenApiResponse [("Accept", "application/openapi+json")]
