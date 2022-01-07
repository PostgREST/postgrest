module Main (main) where

import Test.DocTest (doctest)

import Protolude


main :: IO ()
main =
  doctest
    [ "-XOverloadedStrings"
    , "-XNoImplicitPrelude"
    , "-XStandaloneDeriving"
    , "-isrc"
    , "src/PostgREST/Request/Preferences.hs"
    , "src/PostgREST/Request/QueryParams.hs"
    ]
