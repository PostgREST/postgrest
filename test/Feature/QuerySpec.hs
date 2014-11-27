module Feature.QuerySpec where

import Test.Hspec
import Test.Hspec.Wai

import SpecHelper

spec :: Spec
spec = around withApp $ do
  describe "Querying a nonexistent table" $
    it "causes a 404" $
      get "/faketable" `shouldRespondWith` 404

  describe "Filtering response" $
    context "column equality" $

      it "matches the predicate" $
        get "/items?id=eq.5"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just "[{\"id\":5}]"
          , matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "0-0/1"]
          }

  describe "ordering response" $ do
    it "by a column asc" $
      get "/items?id=lte.2&order=asc.id"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just "[{\"id\":1},{\"id\":2}]"
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/2"]
        }
    it "by a column desc" $
      get "/items?id=lte.2&order=desc.id"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just "[{\"id\":2},{\"id\":1}]"
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/2"]
        }

  describe "Canonical location" $ do
    it "Sets Content-Location with alphabetized params" $
      get "/no_pk?b=eq.1&a=eq.1"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just "[]"
        , matchStatus  = 200
        , matchHeaders = ["Content-Location" <:> "/no_pk?a=eq.1&b=eq.1"]
        }

    it "Omits question mark when there are no params" $
      get "/no_pk"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just "[]"
        , matchStatus  = 200
        , matchHeaders = ["Content-Location" <:> "/no_pk"]
        }
