module Main where

import Test.Hspec
import Spec

main :: IO ()
main = do
  putStrLn "before spec"
  hspec spec
  putStrLn "after spec"
