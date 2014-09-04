{-# LANGUAGE OverloadedStrings #-}

module Unit.PgQuerySpec where

import Test.Hspec

import Database.HDBC (IConnection, SqlValue, toSql, prepare, execute,
                     SqlError, seState, fetchAllRowsAL)

import PgQuery (insert)
import Types (SqlRow(SqlRow))
import TestTypes (fromList, incStr, incNullableStr, incInsert, incId)
import Data.Map (toList)
import Data.Text(pack)

import SpecHelper(dbWithSchema)

quickALQuery :: IConnection conn => conn -> String -> [SqlValue] -> IO [[(String, SqlValue)]]
quickALQuery conn q bind = do
  sth <- prepare conn q
  _ <- execute sth bind
  fetchAllRowsAL sth

spec :: Spec
spec = around dbWithSchema $ do
  describe "insert" $
    describe "with an auto-increment key" $ do
      it "inserts and responds with a full object description" $ \conn -> do
        r <- insert 1 "auto_incrementing_pk" (SqlRow [
            ("non_nullable_string", toSql ("a string"::String))]) conn
        let returnRow = fromList . toList $ r
        incStr returnRow `shouldBe` "a string"
        incNullableStr returnRow `shouldBe` Nothing
        incInsert returnRow `shouldSatisfy` not . null
        incId returnRow `shouldSatisfy` (>= 0)
        tRows <- quickALQuery conn "select * from \"1\".auto_incrementing_pk" []
        [returnRow] `shouldBe` map fromList tRows

      it "throws an exception if the PK is not unique" $ \conn -> do
        r <- insert 1 "auto_incrementing_pk" (SqlRow [
            ("non_nullable_string", toSql ("a string"::String))]) conn
        let row = SqlRow .  map (\(k, v) -> (pack k, v)) . toList $ r
        insert 1 "auto_incrementing_pk" row conn `shouldThrow` \e ->
          seState e == "23505" -- uniqueness violation code

      it "throws an exception if a required value is missing" $ \conn -> do
        insert 1 "auto_incrementing_pk" (SqlRow [
          ("nullable_string", toSql ("a string"::String))]) conn
          `shouldThrow` \e -> seState e == "23502"
