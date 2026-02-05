module Feature.Self.SelfEdgeSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "_self edge cases" $ do
  it "returns a _self URL for a table with composite PK" $ do
    -- the fixture provides a row for compound_pk; just GET and assert
    get "/compound_pk?select=k1,k2,_self" `shouldRespondWith`
      "[{\"k1\":1,\"k2\":\"a\",\"_self\":\"/compound_pk?k1=eq.1&k2=eq.a\"}]"
      { matchStatus = 200
      , matchHeaders = [matchContentTypeJson]
      }

  it "returns a _self URL for a view backed by a composite PK table" $ do
    -- the view selects from compound_pk; ensure the view also exposes _self
    get "/compound_pk_view?select=k1,k2,_self" `shouldRespondWith`
      "[{\"k1\":1,\"k2\":\"a\",\"_self\":\"/compound_pk_view?k1=eq.1&k2=eq.a\"}]"
      { matchStatus = 200
      , matchHeaders = [matchContentTypeJson]
      }
