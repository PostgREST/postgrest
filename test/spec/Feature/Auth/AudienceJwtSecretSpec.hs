module Feature.Auth.AudienceJwtSecretSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "test handling of aud claims in JWT" $ do

  -- this test will stop working 9999999999s after the UNIX EPOCH
  it "succeeds with jwt token containing with an audience claim" $ do
    {-  This is the decoded contents of authHeaderJWT

        {
          "exp": 9999999999,
          "role": "postgrest_test_author",
          "id": "jdoe",
          "aud": "youraudience"
        }

    -}
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjk5OTk5OTk5OTksInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdF9hdXRob3IiLCJpZCI6Impkb2UiLCJhdWQiOiJ5b3VyYXVkaWVuY2UifQ.fJ4tLKSmolWGWehWN20qiU9dMO-WY0RI2VvacL7-ZGo"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  it "succeeds with jwt token that does not contain an audience claim" $ do
    {- This is the decoded contents of authHeaderJWT

        {
          "exp": 9999999999,
          "role": "postgrest_test_author",
          "id": "jdoe"
        }
    -}
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjk5OTk5OTk5OTksInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdF9hdXRob3IiLCJpZCI6Impkb2UifQ.Dpss-QoLYjec5OTsOaAc3FNVsSjA89wACoV-0ra3ClA"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  it "requests without JWT token should work" $
    get "/has_count_column" `shouldRespondWith` 200
