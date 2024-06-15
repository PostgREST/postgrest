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
    , "src/PostgREST/ApiRequest/Preferences.hs"
    , "src/PostgREST/ApiRequest/QueryParams.hs"
    , "src/PostgREST/Config.hs"
    , "src/PostgREST/Error.hs"
    , "src/PostgREST/MediaType.hs"
    , "src/PostgREST/Plan.hs"
    , "src/PostgREST/Query/SqlFragment.hs"
    , "src/PostgREST/Response.hs"
    , "src/PostgREST/Response/Performance.hs"
    ]
