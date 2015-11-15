module Unit.DbStructureSpec where

import Test.Hspec
import DbStructure (Table(..), tables, Column(..), columns, ForeignKey(..),
  foreignKeys)

import Database.HDBC (quickQuery)
import SpecHelper(dbWithSchema)
import qualified Data.Map as M;

spec :: Spec
spec = around dbWithSchema $ beforeWith setRole $ do
  describe "tables" $
    it "shows all the tables" $ \conn -> do
      ts <- tables "test" conn
      map tableName ts `shouldBe` ["authors_only","auto_incrementing_pk",
        "compound_pk","has_fk","insertable_view_with_join","items","menagerie","no_pk", "simple_pk"]

  describe "columns" $ do
    it "responds with each column for the table" $ \conn -> do
      cs <- columns "test" "auto_incrementing_pk" conn
      map colName cs `shouldBe` ["id","nullable_string","non_nullable_string",
        "inserted_at"]

    it "includes foreign key data" $ \conn -> do
      cs <- columns "test" "has_fk" conn
      map colFK cs `shouldBe` [Nothing,
        Just $ ForeignKey "auto_incrementing_pk" "id",
        Just $ ForeignKey "simple_pk" "k"]

  describe "foreignKeys" $
    it "has a description of the foreign key columns" $ \conn ->
      foreignKeys "test" "has_fk" conn `shouldReturn` M.fromList [
        ("auto_inc_fk", ForeignKey {fkTable="auto_incrementing_pk", fkCol="id"}),
        ("simple_fk", ForeignKey { fkTable="simple_pk", fkCol="k"})]

  where setRole conn = quickQuery conn "set role postgrest_test" [] >> return conn
