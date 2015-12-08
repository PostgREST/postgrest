module Main where

import Test.Hspec
import SpecHelper
import Spec

main :: IO ()
main = setupDb >> hspec spec
