module Feature.OptionsSpec where

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "Allow header" $ do
  it "includes read/write verbs for writeable table" $ do
    r <- request methodOptions "/items" [] ""
    liftIO $
      simpleHeaders r `shouldSatisfy`
        matchHeader "Allow" "GET,POST,PATCH,DELETE"

  it "includes read verbs for read-only table" $ do
    r <- request methodOptions "/has_count_column" [] ""
    liftIO $
      simpleHeaders r `shouldSatisfy`
        matchHeader "Allow" "GET"

  it "includes read verbs for non auto updatable views" $ do
    r <- request methodOptions "/projects_view_without_triggers" [] ""
    liftIO $
      simpleHeaders r `shouldSatisfy`
        matchHeader "Allow" "GET"

  it "includes read/write verbs for insertable, updatable and deletable views" $ do
    r <- request methodOptions "/projects_view_with_all_triggers" [] ""
    liftIO $
      simpleHeaders r `shouldSatisfy`
        matchHeader "Allow" "GET,POST,PATCH,DELETE"

  it "includes read and insertable verbs for insertable views" $ do
    r <- request methodOptions "/projects_view_with_insert_trigger" [] ""
    liftIO $
      simpleHeaders r `shouldSatisfy`
        matchHeader "Allow" "GET,POST"

  it "includes read and updatable verbs for updatable views" $ do
    r <- request methodOptions "/projects_view_with_update_trigger" [] ""
    liftIO $
      simpleHeaders r `shouldSatisfy`
        matchHeader "Allow" "GET,PATCH"

  it "includes read and insertable verbs for deletable views" $ do
    r <- request methodOptions "/projects_view_with_delete_trigger" [] ""
    liftIO $
      simpleHeaders r `shouldSatisfy`
        matchHeader "Allow" "GET,DELETE"
