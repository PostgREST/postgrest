module Feature.RawOutputTypesSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude
import SpecHelper (acceptHdrs)

spec :: SpecWith Application
spec = describe "When raw-output-media-types config variable is missing or left empty" $ do
  let firefoxAcceptHdrs = acceptHdrs "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
      chromeAcceptHdrs = acceptHdrs "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3"
  it "responds successfully to a request with Firefox Accept headers" $
    request methodGet "/" firefoxAcceptHdrs ""
      `shouldRespondWith` 200
        { matchHeaders= ["Content-Type" <:> "application/openapi+json; charset=utf-8"] }

  it "responds successfully to a request with Chrome Accept headers" $
    request methodGet "/" chromeAcceptHdrs ""
      `shouldRespondWith` 200
        { matchHeaders= ["Content-Type" <:> "application/openapi+json; charset=utf-8"] }
