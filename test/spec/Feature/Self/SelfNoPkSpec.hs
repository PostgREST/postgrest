module Feature.Self.SelfNoPkSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "_self on tables without primary key" $ do
  it "returns 400 when _self is requested on a table without PK" $ do
    get "/no_pk?select=a,b,_self" `shouldRespondWith`
      [json| { "message":"The _self pseudo-column requires a primary key on the target table: no_pk", "code":"PGRST129", "details":null, "hint":"Add a primary key to the target table or remove '_self' from the ?select parameter." } |]
      { matchStatus = 400
      , matchHeaders = [matchContentTypeJson]
      }

