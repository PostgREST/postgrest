module Feature.ObservabilitySpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude

spec :: SpecWith ((), Application)
spec =
  describe "Observability" $ do
    it "includes the server trace header on the response" $ do
      request methodHead "/"
          [ ("X-Request-Id", "1") ]
          ""
        `shouldRespondWith`
          ""
          { matchHeaders = [ "X-Request-Id" <:> "1"] }

      request methodHead "/projects"
          [ ("X-Request-Id", "2") ]
          ""
        `shouldRespondWith`
          ""
          { matchHeaders = [ "X-Request-Id" <:> "2"] }

      request methodHead "/rpc/add_them?a=2&b=4"
          [ ("X-Request-Id", "3") ]
          ""
        `shouldRespondWith`
          ""
          { matchHeaders = [ "X-Request-Id" <:> "3"] }
