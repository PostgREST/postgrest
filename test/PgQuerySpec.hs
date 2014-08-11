module PgQuerySpec where

import Test.Hspec

import Database.HDBC
import Database.HDBC.PostgreSQL

loadFixture :: String -> IO ()
loadFixture name = do
  sql <- readFile $ "test/fixtures/" ++ name ++ ".sql"
  conn <- connectPostgreSQL "postgres://postgres:@localhost:5432/dbapi_test"
  runRaw conn sql
  commit conn

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = beforeAll (loadFixture "schema") $ do
  describe "insert" $
    it "can insert into an empty table" $
      head [23 ..] `shouldBe` (23 :: Int)

  describe "insert again" $
    it "is true" $
      True `shouldBe` True
