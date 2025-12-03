module Feature.Query.RawOutputTypesSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude
import SpecHelper (acceptHdrs)

spec :: SpecWith ((), Application)
spec = describe "When raw-media-types config variable is missing or left empty" $ do
  let firefoxAcceptHdrs = acceptHdrs "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
      chromeAcceptHdrs = acceptHdrs "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3"
  it "responds json to a GET request with Firefox Accept headers" $
    request methodGet "/items?id=eq.1" firefoxAcceptHdrs ""
      `shouldRespondWith` [json| [{"id":1}] |]
        { matchHeaders= ["Content-Type" <:> "application/json; charset=utf-8"] }
  it "responds json to a GET request with Chrome Accept headers" $
    request methodGet "/items?id=eq.1" chromeAcceptHdrs ""
      `shouldRespondWith` [json| [{"id":1}] |]
        { matchHeaders= ["Content-Type" <:> "application/json; charset=utf-8"] }

  it "TODO" $
    request methodGet "/rpc/get_projects_below?id=3"
      firefoxAcceptHdrs ""
      `shouldRespondWith`
      "id\tname\tclient_id\n1\tWindows 7\t1\n2\tWindows 10\t1\n"
      { matchStatus = 200
      , matchHeaders = ["Content-Type" <:> "text/tab-separated-values"]
      }

  it "TODO" $
    request methodGet "/rpc/get_projects_below?id=3"
      chromeAcceptHdrs ""
      `shouldRespondWith`
      "id\tname\tclient_id\n1\tWindows 7\t1\n2\tWindows 10\t1\n"
      { matchStatus = 200
      , matchHeaders = ["Content-Type" <:> "text/tab-separated-values"]
      }
