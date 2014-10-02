{-# LANGUAGE OverloadedStrings #-}
module Feature.AuthSpec where

-- {{{ Imports
import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types

import SpecHelper
-- }}}

spec :: Spec
spec = around appWithFixture $
  describe "authorization" $ do
    it "hides tables that anonymous does not own" $
      get "/authors_only" `shouldRespondWith` 400 -- TODO: should be 404
    it "indicates login failure" $ do
      let auth = authHeader "dbapi_test_author_a" "fakefake"
      request methodGet "/authors_only" [auth] ""
        `shouldRespondWith` 401
    it "allows users with permissions to see their tables" $ do
      let auth = authHeader "dbapi_test_author_a" ""
      request methodGet "/authors_only" [auth] ""
        `shouldRespondWith` 400
