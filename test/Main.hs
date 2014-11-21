{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Hasql as H
import qualified Hasql.Backend as H
import qualified Hasql.Postgres as H
import qualified Data.ByteString.Char8 as BS
import Test.Hspec
import SpecHelper
import Spec

main :: IO ()
main = do
  roles <- loadFixture "roles"
  schema <- loadFixture "schema"
  H.session pgSettings testSettings $
    H.tx Nothing $ do
      H.unit [H.q| drop schema if exists "1" cascade |]
      H.unit [H.q| drop schema if exists private cascade |]
      H.unit [H.q| drop schema if exists dbapi cascade |]
      H.unit roles
      H.unit schema

  hspec spec

loadFixture :: FilePath -> IO(H.Statement H.Postgres)
loadFixture name = do
  query <- BS.readFile $ "test/fixtures/" ++ name ++ ".sql"
  return (query, [], False)
