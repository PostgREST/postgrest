{-# LANGUAGE OverloadedStrings #-}

module Unit.PgQuerySpec where

import Test.Hspec

import Database.HDBC (IConnection, SqlValue, toSql, prepare,
                      execute, seState, fetchAllRowsAL)

import PgQuery (insert, addUser, signInRole)
import Types (SqlRow(SqlRow))
import TestTypes (fromList, incStr, incNullableStr, incInsert, incId)
import Data.Map (toList)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Control.Arrow

import SpecHelper(dbWithSchema)

quickALQuery :: IConnection conn => conn ->
                String ->
                [SqlValue] ->
                IO [[(String, SqlValue)]]
quickALQuery conn q bind = do
  sth <- prepare conn q
  _ <- execute sth bind
  fetchAllRowsAL sth

spec :: Spec
spec = around dbWithSchema $ do
  describe "insert" $
    describe "with an auto-increment key" $ do
      it "inserts and responds with a full object description" $ \conn -> do
        r <- insert "1" "auto_incrementing_pk" (SqlRow [
            ("non_nullable_string", toSql ("a string"::String))]) conn
        let returnRow = fromList . toList $ r
        incStr returnRow `shouldBe` "a string"
        incNullableStr returnRow `shouldBe` Nothing
        incInsert returnRow `shouldSatisfy` not . null
        incId returnRow `shouldSatisfy` (>= 0)
        tRows <- quickALQuery conn "select * from \"1\".auto_incrementing_pk" []
        [returnRow] `shouldBe` map fromList tRows

      it "throws an exception if the PK is not unique" $ \conn -> do
        r <- insert "1" "auto_incrementing_pk" (SqlRow [
            ("non_nullable_string", toSql ("a string"::String))]) conn
        let row = SqlRow .  map (Control.Arrow.first cs) . toList $ r
        insert "1" "auto_incrementing_pk" row conn `shouldThrow` \e ->
          seState e == "23505" -- uniqueness violation code

      it "throws an exception if a required value is missing" $ \conn ->
        insert "1" "auto_incrementing_pk" (SqlRow [
          ("nullable_string", toSql ("a string"::String))]) conn
          `shouldThrow` \e -> seState e == "23502"

  describe "addUser and signInRole" $ do
    it "adds a correct user to the right table" $ \conn -> do
      let {user = "jdoe"; pass = "secret"; role = "test_default_role"}

      r <- signInRole user pass conn
      r `shouldBe` Nothing

      addUser user pass role conn

      r2 <- signInRole user pass conn
      r2 `shouldBe` Just role

      r3 <- signInRole user (pass <> "crap") conn
      r3 `shouldBe` Nothing

    it "will not add a user with an unknown role" $ \conn -> do
      let {user = "jdoe"; pass = "secret"; role = "fake_role"}

      addUser user pass role conn `shouldThrow` anyException
