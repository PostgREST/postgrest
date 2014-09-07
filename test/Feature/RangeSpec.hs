{-# LANGUAGE OverloadedStrings #-}
module Feature.RangeSpec where

import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types

import SpecHelper

spec :: Spec
spec = around appWithFixture $
  describe "GET /items" $ do
    context "without range headers" $
      context "with response under server size limit" $
        it "returns whole range with status 200" $
          get "/items" `shouldRespondWith` 200
    context "with range headers" $
      context "of acceptable range" $
        it "succeeds with partial content" $
          request methodGet  "/items"
                  (rangeHdrs $ ByteRangeFromTo 0 1) ""
            `shouldRespondWith` 206
