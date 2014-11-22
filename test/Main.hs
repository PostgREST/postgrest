{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad (void)
import qualified Hasql as H
import System.Process
import Test.Hspec
import SpecHelper
import Spec

main :: IO ()
main = do
  H.session pgSettings testSettings $
    H.tx Nothing $ do
      H.unit [H.q| drop schema if exists "1" cascade |]
      H.unit [H.q| drop schema if exists private cascade |]
      H.unit [H.q| drop schema if exists dbapi cascade |]

  loadFixture "roles"
  loadFixture "schema"

  hspec spec

loadFixture :: FilePath -> IO()
loadFixture name =
  void $ readProcess "psql" ["-U", "postgres", "-d", "dbapi_test", "-a", "-f", "test/fixtures/" ++ name ++ ".sql"] []
