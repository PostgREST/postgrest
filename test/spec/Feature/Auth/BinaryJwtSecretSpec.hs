module Feature.Auth.BinaryJwtSecretSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "server started with binary JWT secret" $

  -- this test will stop working 9999999999s after the UNIX EPOCH
  it "succeeds with jwt token encoded with a binary secret" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjk5OTk5OTk5OTksInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdF9hdXRob3IiLCJpZCI6Impkb2UifQ.Dpss-QoLYjec5OTsOaAc3FNVsSjA89wACoV-0ra3ClA"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200
