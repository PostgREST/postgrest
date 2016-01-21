module Main where

import Test.Hspec
import SpecHelper

--import PostgREST.Types (DbStructure(..))

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

  pool <- testPool
  dbStructure <- specDbStructure pool

  -- Not using hspec-discover because we want to precompute
  -- the db structure and pass it to specs for speed
  hspec $ specs dbStructure pool

 where
  specs dbStructure pool = do
    describe "Feature.AuthSpec" $ Feature.AuthSpec.spec dbStructure pool
    describe "Feature.CorsSpec" $ Feature.CorsSpec.spec dbStructure pool
    describe "Feature.DeleteSpec" $ Feature.DeleteSpec.spec dbStructure pool
    describe "Feature.InsertSpec" $ Feature.InsertSpec.spec dbStructure pool
    describe "Feature.QueryLimitedSpec" $ Feature.QueryLimitedSpec.spec dbStructure pool
    describe "Feature.QuerySpec" $ Feature.QuerySpec.spec dbStructure pool
    describe "Feature.RangeSpec" $ Feature.RangeSpec.spec dbStructure pool
    describe "Feature.StructureSpec" $ Feature.StructureSpec.spec dbStructure pool
