module Feature.Auth.JwtCacheSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude hiding (get)

spec :: SpecWith ((), Application)
spec = describe "jwt cache enabled" $ do
  it "denies access to tables that anonymous does not own" $
    get "/authors_only" `shouldRespondWith`
      [json| {
        "hint":null,
        "details":null,
        "code":"42501",
        "message":"permission denied for table authors_only"} |]
      { matchStatus = 401
      , matchHeaders = ["WWW-Authenticate" <:> "Bearer"]
      }
