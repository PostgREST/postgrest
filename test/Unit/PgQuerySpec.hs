{-# LANGUAGE OverloadedStrings #-}

module Unit.PgQuerySpec where

import Test.Hspec

import Database.HDBC (toSql, quickQuery)

import PgQuery (insert)
import Types (SqlRow(..))

import SpecHelper

spec :: Spec
spec = around dbWithSchema $ do
  describe "insert" $
    it "can insert into an empty table" $ \conn -> do
      _ <- insert 1 "auto_incrementing_pk" (SqlRow [
          ("non_nullable_string", toSql ("a string"::String))]) conn
      r <- quickQuery conn "select count(1) from \"1\".auto_incrementing_pk" []
      [[toSql (1 :: Int)]] `shouldBe` r
