module Feature.ProxySpec where

import Test.Hspec

import SpecHelper

import Network.Wai (Application)

import Protolude

spec :: SpecWith Application
spec =
  describe "GET / with proxy" $
    it "returns a valid openapi spec with proxy" $
      validateOpenApiResponse [("Accept", "application/openapi+json")]
