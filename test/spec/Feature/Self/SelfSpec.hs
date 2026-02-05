module Feature.Self.SelfSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "_self links" $ do
  it "returns a _self URL for each top-level row" $ do
    get "/projects?select=id,name,_self" `shouldRespondWith`
      [json|[{"id":1,"name":"Windows 7","_self":"/projects?id=eq.1"},{"id":2,"name":"Windows 10","_self":"/projects?id=eq.2"},{"id":3,"name":"IOS","_self":"/projects?id=eq.3"},{"id":4,"name":"OSX","_self":"/projects?id=eq.4"},{"id":5,"name":"Orphan","_self":"/projects?id=eq.5"}]|]
      { matchStatus  = 200
      , matchHeaders = [matchContentTypeJson]
      }

  it "returns a _self URL for nested related resources" $ do
    get "/projects?select=id,clients(name,_self)&order=id" `shouldRespondWith`
      [json|[{"id":1,"clients":{"name":"Microsoft","_self":"/clients?id=eq.1"}},{"id":2,"clients":{"name":"Microsoft","_self":"/clients?id=eq.1"}},{"id":3,"clients":{"name":"Apple","_self":"/clients?id=eq.2"}},{"id":4,"clients":{"name":"Apple","_self":"/clients?id=eq.2"}},{"id":5,"clients":null}]|]
      { matchStatus  = 200
      , matchHeaders = [matchContentTypeJson]
      }
