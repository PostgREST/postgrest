module Main where

import Database.PostgreSQL.Simple
import Test.Hspec
import Spec
import SpecHelper (openConnection, loadFixture)

main :: IO ()
main = do
  c <-openConnection
  _ <- execute_ c "drop schema if exists \"1\" cascade"
  _ <- execute_ c "drop schema if exists private cascade"
  _ <- execute_ c "drop schema if exists dbapi cascade"
  loadFixture "roles" c
  loadFixture "schema" c
  close c
  hspec spec
