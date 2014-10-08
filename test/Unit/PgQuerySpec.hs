{-# LANGUAGE OverloadedStrings #-}

module Unit.PgQuerySpec where

import Test.Hspec

import Database.HDBC (IConnection, SqlValue, toSql, prepare,
  quickQuery, fromSql, execute, seState, fetchAllRowsAL)

import PgQuery (LoginAttempt(..), insert, addUser, signInRole, checkPass)
import Types (SqlRow(SqlRow))
import TestTypes (fromList, incStr, incNullableStr, incInsert, incId)
import Data.Map (toList)
import Data.String.Conversions (cs)
import Data.Monoid ((<>))
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

  let {user = "jdoe"; pass = "secret"; role = "test_default_role"}
  describe "addUser" $ do
    it "adds a correct user to the right table" $ \conn -> do
      addUser user pass role conn
      [r] <- quickQuery conn "select * from dbapi.auth" []
      let [newUser, newRole, encryptedPass] = map fromSql r :: [String]
      cs newUser `shouldBe` user
      cs newRole `shouldBe` role
      checkPass (cs encryptedPass) pass `shouldBe` True

    it "will not add a user with an unknown role" $ \conn ->
      addUser user pass "not-a-real-role" conn `shouldThrow` \e ->
        take 2 (seState e) == "23" --integrity constraint violation


  describe "signInRole" $ beforeWith (\conn -> do
    addUser user pass role conn
    return conn) $ do
    it "accepts correct credentials and return the role" $ \conn ->
      signInRole user pass conn `shouldReturn` LoginSuccess role

    it "returns nothing with bad creds" $ \conn -> do
      signInRole "not-a-user" pass conn `shouldReturn` LoginFailed
      signInRole user (pass <> "crap") conn `shouldReturn` LoginFailed
