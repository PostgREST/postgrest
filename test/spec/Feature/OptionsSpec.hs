module Feature.OptionsSpec where

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import PostgREST.Config.PgVersion (PgVersion, pgVersion100,
                                   pgVersion110)

import Protolude
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec actualPgVersion = describe "Allow header" $ do
  context "a table" $ do
    it "includes read/write methods for writeable table" $ do
      r <- request methodOptions "/items" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PUT,PATCH,DELETE"

    it "fails with 404 for an unknown table" $
      request methodOptions "/unknown" [] "" `shouldRespondWith` 404

  when (actualPgVersion >= pgVersion100) $
    context "a partitioned table" $ do
      it "includes read/write methods for writeable partitioned tables" $ do
        r <- request methodOptions "/car_models" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" (
              if actualPgVersion >= pgVersion110 then
                "OPTIONS,GET,HEAD,POST,PUT,PATCH,DELETE"
              else
                "OPTIONS,GET,HEAD,POST,PATCH,DELETE"
            )

  context "a view" $ do
    context "auto updatable" $ do
      it "includes read/write methods for auto updatable views with pk" $ do
        r <- request methodOptions "/projects_auto_updatable_view_with_pk" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PUT,PATCH,DELETE"

      it "includes read/write methods for auto updatable views without pk" $ do
        r <- request methodOptions "/projects_auto_updatable_view_without_pk" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PATCH,DELETE"

    context "non auto updatable" $ do
      it "includes read methods for non auto updatable views" $ do
        r <- request methodOptions "/projects_view_without_triggers" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD"

      it "includes read/write methods for insertable, updatable and deletable views with pk" $ do
        r <- request methodOptions "/projects_view_with_all_triggers_with_pk" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PUT,PATCH,DELETE"

      it "includes read/write methods for insertable, updatable and deletable views without pk" $ do
        r <- request methodOptions "/projects_view_with_all_triggers_without_pk" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST,PATCH,DELETE"

      it "includes read and insert methods for insertable views" $ do
        r <- request methodOptions "/projects_view_with_insert_trigger" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,POST"

      it "includes read and update methods for updatable views" $ do
        r <- request methodOptions "/projects_view_with_update_trigger" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,PATCH"

      it "includes read and delete methods for deletable views" $ do
        r <- request methodOptions "/projects_view_with_delete_trigger" [] ""
        liftIO $
          simpleHeaders r `shouldSatisfy`
            matchHeader "Allow" "OPTIONS,GET,HEAD,DELETE"

  context "a function" $ do
    it "includes the POST method for a volatile function" $ do
      r <- request methodOptions "/rpc/reset_table" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,POST"

    it "includes the GET/HEAD/POST method for a stable function" $ do
      r <- request methodOptions "/rpc/getallusers" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,GET,HEAD,POST"

    it "includes the GET/HEAD/POST method for a immutable function" $ do
      r <- request methodOptions "/rpc/jwt_test" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,GET,HEAD,POST"

  context "root endpoint" $ do
    it "includes the GET/HEAD method " $ do
      r <- request methodOptions "/" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,GET,HEAD"
