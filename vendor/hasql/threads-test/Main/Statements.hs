module Main.Statements where

import Prelude

import Hasql.Statement

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

selectSleep :: Statement Double ()
selectSleep =
  Statement sql encoder decoder True
  where
    sql =
      "select pg_sleep($1)"
    encoder =
      E.param (E.nonNullable E.float8)
    decoder =
      D.noResult
