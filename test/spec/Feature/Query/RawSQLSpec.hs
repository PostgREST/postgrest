module Feature.Query.RawSQLSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Network.Wai.Test    (SResponse (simpleHeaders, simpleStatus))
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude
import SpecHelper (acceptHdrs)

spec :: SpecWith ((), Application)
spec = describe "When accept header is set to application/json+sql" $ do
  let mtApplicationJSONSQLHdrs = acceptHdrs "application/json+sql"

  it "responds raw sql to a GET request" $ do
    r <- request methodGet "/items?id=eq.1" mtApplicationJSONSQLHdrs ""
    liftIO $ do
      simpleStatus r `shouldBe` status200
      simpleHeaders r `shouldContain` [("Content-Type", "application/json; charset=utf-8")]

  it "responds raw sql to a POST request for insert" $ do
    r <- request methodPost "/projects"
             mtApplicationJSONSQLHdrs [json|{"id":100, "name": "Project 100"}|]
    liftIO $ do
      simpleStatus r `shouldBe` status200
      simpleHeaders r `shouldContain` [("Content-Type", "application/json; charset=utf-8")]

  it "responds raw sql to a create POST for update" $ do
    r <- request methodPatch "/projects?id=eq.3"
             mtApplicationJSONSQLHdrs [json|{"name": "Patched Project"}|]
    liftIO $ do
      simpleStatus r `shouldBe` status200
      simpleHeaders r `shouldContain` [("Content-Type", "application/json; charset=utf-8")]

  it "responds raw sql to a create POST for delete" $ do
    r <- request methodDelete "/projects?id=in.(1,2,3)"
             mtApplicationJSONSQLHdrs ""
    liftIO $ do
      simpleStatus r `shouldBe` status200
      simpleHeaders r `shouldContain` [("Content-Type", "application/json; charset=utf-8")]
