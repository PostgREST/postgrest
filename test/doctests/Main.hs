module Main (main) where

import Test.DocTest (doctest)

import Protolude


main :: IO ()
main =
  doctest
    [ "--verbose"
    , "-XOverloadedStrings"
    , "-XNoImplicitPrelude"
    , "-XStandaloneDeriving"
    , "-isrc"
    , "src/PostgREST/Request/Preferences.hs"
    ]
