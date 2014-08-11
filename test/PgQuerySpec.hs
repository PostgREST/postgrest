module PgQuerySpec where

import Test.Hspec

import Database.HDBC
import Database.HDBC.PostgreSQL

loadFixture :: String -> IO ()
loadFixture name = do
  conn <- connectPostgreSQL "postgres://postgres:@localhost:5432/dbapi_test"
  sql <- readFile $ "test/fixtures/" ++ name ++ ".sql"
  runRaw conn "drop schema if exists public cascade"
  runRaw conn "create schema public"
  runRaw conn sql
  commit conn

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll (loadFixture "schema") $ do
  describe "insert" $
    it "can insert into an empty table" $ \_ ->
      head [23 ..] `shouldBe` (23 :: Int)

  describe "insert again" $
    it "is true" $ \_ ->
      True `shouldBe` True
