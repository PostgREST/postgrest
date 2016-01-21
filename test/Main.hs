module Main where

import Test.Hspec
import SpecHelper

import Data.Pool
import qualified Hasql.Session as H

import PostgREST.DbStructure (getDbStructure)

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
  withResource pool $ \case
    Left err -> error $ show err
    Right c -> do
      dbOrErr <- H.run (getDbStructure "test") c
      -- Not using hspec-discover because we want to precompute
      -- the db structure and pass it to specs for speed
      either (error.show) (hspec . specs c) dbOrErr

 where
  specs conn dbStructure = do
    describe "Feature.AuthSpec" $ Feature.AuthSpec.spec dbStructure conn
    describe "Feature.CorsSpec" $ Feature.CorsSpec.spec dbStructure conn
    describe "Feature.DeleteSpec" $ Feature.DeleteSpec.spec dbStructure conn
    describe "Feature.InsertSpec" $ Feature.InsertSpec.spec dbStructure conn
    describe "Feature.QueryLimitedSpec" $ Feature.QueryLimitedSpec.spec dbStructure conn
    describe "Feature.QuerySpec" $ Feature.QuerySpec.spec dbStructure conn
    describe "Feature.RangeSpec" $ Feature.RangeSpec.spec dbStructure conn
    describe "Feature.StructureSpec" $ Feature.StructureSpec.spec dbStructure conn
