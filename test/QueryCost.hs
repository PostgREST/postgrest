module Main where

import           Control.Lens                      ((^?))
import qualified Data.Aeson.Lens                   as L
import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Snippet   as H
import qualified Hasql.DynamicStatements.Statement as H
import qualified Hasql.Pool                        as P
import qualified Hasql.Statement                   as H
import qualified Hasql.Transaction                 as HT
import qualified Hasql.Transaction.Sessions        as HT
import           Text.Heredoc

import Protolude      hiding (get, toS)
import Protolude.Conv (toS)

import PostgREST.ApiRequest       (PayloadJSON (..))
import PostgREST.DbStructureTypes
import PostgREST.Preferences
import PostgREST.QueryBuilder     (requestToCallProcQuery)

import SpecHelper (getEnvVarWithDefault)

import Test.Hspec

main :: IO ()
main = do
  testDbConn <- getEnvVarWithDefault "PGRST_DB_URI" "postgres://postgrest_test@localhost/postgrest_test"
  pool <- P.acquire (3, 10, toS testDbConn)

  hspec $ describe "QueryCost" $
    context "call proc query" $ do
      it "should not exceed cost when calling setof composite proc" $ do
        cost <- exec pool $
          requestToCallProcQuery (QualifiedIdentifier "test" "get_projects_below") [PgArg "id" "int" True False]
          (Just $ RawJSON [str| {"id": 3} |]) False Nothing []
        liftIO $
          cost `shouldSatisfy` (< Just 40)

      it "should not exceed cost when calling setof composite proc with empty params" $ do
        cost <- exec pool $
          requestToCallProcQuery (QualifiedIdentifier "test" "getallprojects") [] Nothing False Nothing []
        liftIO $
          cost `shouldSatisfy` (< Just 30)

      it "should not exceed cost when calling scalar proc" $ do
        cost <- exec pool $
          requestToCallProcQuery (QualifiedIdentifier "test" "add_them") [PgArg "a" "int" True False, PgArg "b" "int" True False]
          (Just $ RawJSON [str| {"a": 3, "b": 4} |]) True Nothing []
        liftIO $
          cost `shouldSatisfy` (< Just 10)

      context "params=multiple-objects" $ do
        it "should not exceed cost when calling setof composite proc" $ do
          cost <- exec pool $
            requestToCallProcQuery (QualifiedIdentifier "test" "get_projects_below") [PgArg "id" "int" True False]
            (Just $ RawJSON [str| [{"id": 1}, {"id": 4}] |]) False (Just MultipleObjects) []
          liftIO $ do
            -- lower bound needed for now to make sure that cost is not Nothing
            cost `shouldSatisfy` (> Just 2000)
            cost `shouldSatisfy` (< Just 2100)

        it "should not exceed cost when calling scalar proc" $ do
          cost <- exec pool $
            requestToCallProcQuery (QualifiedIdentifier "test" "add_them") [PgArg "a" "int" True False, PgArg "b" "int" True False]
            (Just $ RawJSON [str| [{"a": 3, "b": 4}, {"a": 1, "b": 2}, {"a": 8, "b": 7}] |]) True Nothing []
          liftIO $
            cost `shouldSatisfy` (< Just 10)


exec :: P.Pool -> H.Snippet -> IO (Maybe Int64)
exec pool query =
  join . rightToMaybe <$>
  P.use pool (HT.transaction HT.ReadCommitted HT.Read $ HT.statement mempty $ explainCost query)

explainCost :: H.Snippet -> H.Statement () (Maybe Int64)
explainCost query =
  H.dynamicallyParameterized snippet decodeExplain False
 where
   snippet = "EXPLAIN (FORMAT JSON) " <> query
   decodeExplain :: HD.Result (Maybe Int64)
   decodeExplain =
     let row = HD.singleRow $ HD.column $ HD.nonNullable HD.bytea in
     (^? L.nth 0 . L.key "Plan" .  L.key "Total Cost" . L._Integral) <$> row
