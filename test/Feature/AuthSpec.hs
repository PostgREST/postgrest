{-# LANGUAGE QuasiQuotes #-}
module Feature.AuthSpec where

-- {{{ Imports
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types

import SpecHelper
-- }}}

spec :: Spec
spec = beforeAll resetDb $ around withApp $
  describe "authorization" $ do
    it "hides tables that anonymous does not own" $
      get "/authors_only" `shouldRespondWith` 404
    it "indicates login failure" $ do
      let auth = authHeader "postgrest_test_author" "fakefake"
      request methodGet "/authors_only" [auth] ""
        `shouldRespondWith` 401
    it "allows users with permissions to see their tables" $ do
      _ <- post "/postgrest/users" [json| { "id":"jdoe", "pass": "1234", "role": "postgrest_test_author" } |]
      let auth = authHeader "jdoe" "1234"
      request methodGet "/authors_only" [auth] ""
        `shouldRespondWith` 200
