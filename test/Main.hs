module Main where

import Test.Hspec
import SpecHelper
import Spec

main :: IO ()
main = resetDb >> hspec spec
