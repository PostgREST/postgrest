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
  context "a table" $ do
    it "includes read/write verbs for writeable table" $ do
      r <- request methodOptions "/items" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PUT,PATCH,DELETE"

  context "a partitioned table" $ do
    it "includes read/write verbs for writeable partitioned tables" $ do
      r <- request methodOptions "/partitioned_a" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PUT,PATCH,DELETE"

  context "a view" $ do
    context "auto updatable" $ do
      it "includes read/write verbs for auto updatable views with pk" $ do
        r <- request methodOptions "/projects_auto_updatable_view_with_pk" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PUT,PATCH,DELETE"

      it "includes read/write verbs for auto updatable views without pk" $ do
        r <- request methodOptions "/projects_auto_updatable_view_without_pk" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PATCH,DELETE"

    context "non auto updatable" $ do
      it "includes read verbs for non auto updatable views" $ do
        r <- request methodOptions "/projects_view_without_triggers" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD"

      it "includes read/write verbs for insertable, updatable and deletable views with pk" $ do
        r <- request methodOptions "/projects_view_with_all_triggers_with_pk" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PUT,PATCH,DELETE"

      it "includes read/write verbs for insertable, updatable and deletable views without pk" $ do
        r <- request methodOptions "/projects_view_with_all_triggers_without_pk" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PATCH,DELETE"

      it "includes read and insert verbs for insertable views" $ do
        r <- request methodOptions "/projects_view_with_insert_trigger" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST"

      it "includes read and update verbs for updatable views" $ do
        r <- request methodOptions "/projects_view_with_update_trigger" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,PATCH"

      it "includes read and delete verbs for deletable views" $ do
        r <- request methodOptions "/projects_view_with_delete_trigger" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,DELETE"
