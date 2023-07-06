module Main (main) where

import Test.DocTest (doctest)

import Protolude


main :: IO ()
main =
  doctest
    [ "-XOverloadedStrings"
    , "-XNoImplicitPrelude"
    , "-XStandaloneDeriving"
    , "-XDuplicateRecordFields"
    , "-isrc"
    , "src/PostgREST/Query/SqlFragment.hs"
    , "src/PostgREST/ApiRequest/Preferences.hs"
    , "src/PostgREST/ApiRequest/QueryParams.hs"
    , "src/PostgREST/Error.hs"
    , "src/PostgREST/MediaType.hs"
    , "src/PostgREST/Config.hs"
    , "src/PostgREST/Plan.hs"
    ]
