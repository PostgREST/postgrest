module Main where

import Protolude
import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "postgrest" =<< getArgs
