module Feature.AudienceJwtSecretSpec where

-- {{{ Imports
import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types

import SpecHelper
import Network.Wai (Application)

import Protolude hiding (get)
-- }}}

spec :: SpecWith Application
spec = describe "server started with binary JWT secret" $

  -- this test will stop working 9999999999s after the UNIX EPOCH
  it "succeeds with jwt token encoded with a binary secret and an audience claim" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjk5OTk5OTk5OTksInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdF9hdXRob3IiLCJpZCI6Impkb2UiLCJhdWQiOiJ5b3VyYXVkaWVuY2UifQ.fJ4tLKSmolWGWehWN20qiU9dMO-WY0RI2VvacL7-ZGo"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200
