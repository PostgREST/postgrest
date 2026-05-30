module Feature.OpenApi.ProxySpec where

import Test.Hspec hiding (pendingWith)

import PostgREST.Config (AppConfig (..))

import Protolude
import SpecHelper

spec :: SpecWithConfig
spec withConfig = withConfig (baseCfg { configOpenApiServerProxyUri = Just "https://postgrest.com/openapi.json" }) $
  describe "GET / with proxy" $
    it "returns a valid openapi spec with proxy" $
      validateOpenApiResponse [("Accept", "application/openapi+json")]
