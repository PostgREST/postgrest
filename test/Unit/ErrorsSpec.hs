{-# LANGUAGE OverloadedStrings #-}
module Unit.ErrorsSpec where

import Test.Hspec

import Database.HDBC (runRaw, quickQuery, fromSql, SqlError)
import SpecHelper (dbWithSchema)
import Middleware (withSavepoint)
import PgQuery (insert)
import Types(SqlRow(..))
import Control.Exception(catch)
import Control.Monad(void)
import Network.Wai (defaultRequest, responseLBS)
import Network.HTTP.Types.Status (ok200)

spec :: Spec
spec = let
  dbErrApp conn _ res = do
    putStrLn "In fake app"
    _ <- insert "1" "items" (SqlRow []) conn
    runRaw conn "select 1/0"
    _ <- insert "1" "items" (SqlRow []) conn
    res $ responseLBS ok200 [("Content-Type", "application/json")] "{}"
  in around dbWithSchema $

    describe "withSavepoint" $
      it "allows partial rollback of request" $ \c -> do
        let app = withSavepoint dbErrApp c
        [[beforeCount]] <- quickQuery c "select count(*) from \"1\".items" []
        runRaw c "set role dbapi_anonymous"
        _ <- insert "1" "items" (SqlRow []) c
        catch (void $ app defaultRequest (const undefined) ) $
          \e -> let _ = (e::SqlError) in do
          _ <- insert "1" "items" (SqlRow []) c
          [[afterCount]] <- quickQuery c "select count(*) from \"1\".items" []
          fromSql afterCount `shouldBe` (fromSql beforeCount::Int) + 2
