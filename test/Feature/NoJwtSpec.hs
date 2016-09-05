module Feature.NoJwtSpec where

-- {{{ Imports
import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types

import SpecHelper
import Network.Wai (Application)
-- }}}

spec :: SpecWith Application
spec = describe "server started without JWT secret" $ do

  -- this test will stop working 9999999999s after the UNIX EPOCH
  it "responds with error on attempted auth" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjk5OTk5OTk5OTksInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdF9hdXRob3IiLCJpZCI6Impkb2UifQ.QaPPLWTuyydMu_q7H4noMT7Lk6P4muet1OpJXF6ofhc"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 500

  it "behaves normally when user does not attempt auth" $ do
    request methodGet "/items" [] ""
      `shouldRespondWith` 200
