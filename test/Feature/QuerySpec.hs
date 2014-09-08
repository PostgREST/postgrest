
{-# LANGUAGE OverloadedStrings #-}
module Feature.QuerySpec where

import Test.Hspec
import Test.Hspec.Wai

import SpecHelper

spec :: Spec
spec = around appWithFixture $ do
  describe "Filtering response" $
    context "column equality" $

      it "matches the predicate" $
        get "/items?id=eq.5"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just "[{\"id\":5}]"
          , matchStatus  = 200
          , matchHeaders = [("Content-Range", "0-0/1")]
          }

  describe "Canonical location" $
    it "Sets Content-Location with alphabetized params" $
      get "/no_pk?b=eq.1&a=eq.1"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Nothing
        , matchStatus  = 204
        , matchHeaders = [("Content-Location", "/no_pk?a=eq.1&b=eq.1")]
        }
