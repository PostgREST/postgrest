module Main where

import Test.Hspec
import SpecHelper

import PostgREST.Types (DbStructure(..))

import qualified Feature.AuthSpec
import qualified Feature.CorsSpec
import qualified Feature.DeleteSpec
import qualified Feature.InsertSpec
import qualified Feature.QueryLimitedSpec
import qualified Feature.QuerySpec
import qualified Feature.RangeSpec
import qualified Feature.StructureSpec

main :: IO ()
main = do
  setupDb

  pool <- specDbPool
  dbStructure <- specDbStructure pool

  -- Not using hspec-discover because we want to precompute
  -- the db structure and pass it to specs for speed
  mapM_ (hspec . ($ pool) . ($ dbStructure)) specs

 where
  specs = [
      Feature.AuthSpec.spec
    , Feature.CorsSpec.spec
    , Feature.DeleteSpec.spec
    , Feature.InsertSpec.spec
    , Feature.QueryLimitedSpec.spec
    , Feature.QuerySpec.spec
    , Feature.RangeSpec.spec
    , Feature.StructureSpec.spec
    ]
