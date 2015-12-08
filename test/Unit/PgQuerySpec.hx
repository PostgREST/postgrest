module Unit.PgQuerySpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Database.HDBC (IConnection, SqlValue, toSql, prepare,
  quickQuery, fromSql, execute, seState, fetchAllRowsAL)

import PgQuery (LoginAttempt(..), insert, addUser, signInRole, checkPass
              , pgFmtIdent, pgFmtLit)
import Types (SqlRow(SqlRow))
import TestTypes (incFromList, incStr, incNullableStr, incInsert, incId)
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
        r <- insert "test" "auto_incrementing_pk" (SqlRow [
            ("non_nullable_string", toSql ("a string"::String))]) conn
        let returnRow = incFromList . toList $ r
        incStr returnRow `shouldBe` "a string"
        incNullableStr returnRow `shouldBe` Nothing
        incInsert returnRow `shouldSatisfy` not . null
        incId returnRow `shouldSatisfy` (>= 0)
        tRows <- quickALQuery conn "select * from \"1\".auto_incrementing_pk" []
        [returnRow] `shouldBe` map incFromList tRows

      it "throws an exception if the PK is not unique" $ \conn -> do
        r <- insert "test" "auto_incrementing_pk" (SqlRow [
            ("non_nullable_string", toSql ("a string"::String))]) conn
        let row = SqlRow .  map (Control.Arrow.first cs) . toList $ r
        insert "test" "auto_incrementing_pk" row conn `shouldThrow` \e ->
          seState e == "23505" -- uniqueness violation code

      it "throws an exception if a required value is missing" $ \conn ->
        insert "test" "auto_incrementing_pk" (SqlRow [
          ("nullable_string", toSql ("a string"::String))]) conn
          `shouldThrow` \e -> seState e == "23502"

      it "generates a default values query if no data is provided" $ \c -> do
        r <- insert "test" "items" (SqlRow []) c
        let [row] = toList r
        quickALQuery c "select * from \"1\".items where id = ?" [snd row]
          `shouldReturn` [[row]]

  let {user = "jdoe"; pass = "secret"; role = "postgrest_test_default_role"}
  describe "addUser" $ do
    it "adds a correct user to the right table" $ \conn -> do
      addUser user pass role conn
      [r] <- quickQuery conn "select * from postgrest.auth" []
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
      signInRole user pass conn `shouldReturn` LoginSuccess role user

    it "returns nothing with bad creds" $ \conn -> do
      signInRole "not-a-user" pass conn `shouldReturn` LoginFailed
      signInRole user (pass <> "crap") conn `shouldReturn` LoginFailed

  describe "pgFmtIdent" $
    it "Does what format %I would do" $ \conn -> property $ \fuzz ->
      monadicIO $ do
        [[row]] <- run $ quickALQuery conn "select format('%I', ? :: varchar)" [toSql (fuzz :: String)]
        assert $ fromSql (snd row) == pgFmtIdent (cs fuzz)

  describe "pgFmtLit" $
    it "Does what format %L would do" $ \conn ->
      property $ monadicIO $ do
        fuzz <- pick arbitrary
        [[row]] <- run $ quickALQuery conn "select format('%L', ? :: varchar)" [toSql (fuzz :: String)]
        assert $ fromSql (snd row) == pgFmtLit (cs fuzz)
