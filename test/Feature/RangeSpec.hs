{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Feature.RangeSpec where

import Test.Hspec
import Test.Hspec.Wai

import SpecHelper

spec :: Spec
spec = around appWithFixture $ do
  describe "GET /view" $
    context "without range headers" $
      context "with response under server size limit" $
        it "returns whole range with status 200" $
          get "/auto_incrementing_pk" `shouldRespondWith` 206
