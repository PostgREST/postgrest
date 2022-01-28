module Feature.OpenApi.ProxySpec where

import Network.Wai (Application)
import Test.Hspec  hiding (pendingWith)

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "GET / with proxy" $
    it "returns a valid openapi spec with proxy" $
      validateOpenApiResponse [("Accept", "application/openapi+json")]
