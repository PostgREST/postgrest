{-# LANGUAGE OverloadedStrings #-}

module Unit.PgQuerySpec where

import Test.Hspec

import Database.HDBC

import PgQuery (insert, getRows, RangedResult(..))
import Types (SqlRow(..))

import SpecHelper

spec :: Spec
spec = around dbWithSchema $ do
  describe "insert" $
    it "can insert into an empty table" $ \conn -> do
      _ <- insert 1 "auto_incrementing_pk" (SqlRow [
          ("non_nullable_string", toSql ("a string that isn't null" :: String))
        ]) conn
      r <- quickQuery conn "select count(1) from auto_incrementing_pk" []
      [[toSql (1 :: Int)]] `shouldBe` r
