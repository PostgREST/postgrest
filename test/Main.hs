module Main where

import Test.Hspec
import SpecHelper

import qualified Hasql.Pool as P

import PostgREST.DbStructure (getDbStructure)
import PostgREST.App (postgrest)
import Data.String.Conversions (cs)

import qualified Feature.AuthSpec
import qualified Feature.ConcurrentSpec
import qualified Feature.CorsSpec
import qualified Feature.DeleteSpec
import qualified Feature.InsertSpec
import qualified Feature.QueryLimitedSpec
import qualified Feature.QuerySpec
import qualified Feature.RangeSpec
import qualified Feature.StructureSpec
import qualified Feature.UnicodeSpec

main :: IO ()
main = do
  setupDb

  pool <- P.acquire (3, 10, cs testDbConn)

  result <- P.use pool $ getDbStructure "test"
  let dbStructure = either (error.show) id result
      withApp = return $ postgrest testCfg dbStructure pool
      ltdApp  = return $ postgrest testLtdRowsCfg dbStructure pool
      unicodeApp = return $ postgrest testUnicodeCfg dbStructure pool

  hspec $ do
    mapM_ (beforeAll_ resetDb . before withApp) specs

    -- this test runs with a different server flag
    beforeAll_ resetDb . before ltdApp $
      describe "Feature.QueryLimitedSpec" Feature.QueryLimitedSpec.spec

    -- this test runs with a different schema
    beforeAll_ resetDb . before unicodeApp $
      describe "Feature.UnicodeSpec" Feature.UnicodeSpec.spec

 where
  specs = map (uncurry describe) [
      ("Feature.AuthSpec"         , Feature.AuthSpec.spec)
    , ("Feature.ConcurrentSpec"   , Feature.ConcurrentSpec.spec)
    , ("Feature.CorsSpec"         , Feature.CorsSpec.spec)
    , ("Feature.DeleteSpec"       , Feature.DeleteSpec.spec)
    , ("Feature.InsertSpec"       , Feature.InsertSpec.spec)
    , ("Feature.QuerySpec"        , Feature.QuerySpec.spec)
    , ("Feature.RangeSpec"        , Feature.RangeSpec.spec)
    , ("Feature.StructureSpec"    , Feature.StructureSpec.spec)
    ]
