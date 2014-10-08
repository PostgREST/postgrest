module Unit.PgStructureSpec where

import Test.Hspec
import PgStructure (Table(..), tables, Column(..), columns, ForeignKey(..),
  foreignKeys)

import Database.HDBC (quickQuery)
import SpecHelper(dbWithSchema)

spec :: Spec
spec = around dbWithSchema $ beforeWith setRole $ do
  describe "tables" $
    it "shows all the tables" $ \conn -> do
      ts <- tables "1" conn
      map tableName ts `shouldBe` ["auto_incrementing_pk","compound_pk",
        "has_fk","items","menagerie","no_pk", "simple_pk"]

  describe "columns" $
    it "responds with each column for the table" $ \conn -> do
      cs <- columns "1" "auto_incrementing_pk" conn
      map colName cs `shouldBe` ["id","nullable_string","non_nullable_string",
        "inserted_at"]

  describe "foreignKeys" $
    it "has a description of the foreign key columns" $ \conn ->
      foreignKeys "1" "has_fk" conn `shouldReturn` [
        ForeignKey { fkCol="auto_inc_fk",
                   fkTableReferred="auto_incrementing_pk", fkColReferred="id"},
        ForeignKey { fkCol="simple_fk", fkTableReferred="simple_pk",
                   fkColReferred="k"}
        ]
  where setRole conn = quickQuery conn "set role dbapi_test" [] >> return conn
