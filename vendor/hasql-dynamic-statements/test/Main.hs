module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (assert)

import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8

import Hasql.Decoders qualified as Decoders
import Hasql.DynamicStatements.Snippet qualified as Snippet
import Hasql.DynamicStatements.Statement qualified as Statement
import Hasql.Statement qualified as Statement

main :: IO ()
main =
  defaultMain tree

tree :: TestTree
tree =
  testGroup
    "All tests"
    [ testGroup
        "Regression"
        [ testCase "Missing $ for 1000th parameter string #2" $
            let
              snippet =
                "SELECT 1 " <> foldMap @[] ("," <>) (replicate 1001 $ Snippet.param (10 :: Int64))
              statement =
                Statement.dynamicallyParameterized snippet Decoders.noResult True
              sql =
                case statement of
                  Statement.Statement x _ _ _ -> x
            in
              do
                assertBool (ByteStringChar8.unpack sql) (ByteString.isInfixOf "$1000" sql)
        ]
    ]
