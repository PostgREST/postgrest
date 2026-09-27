{-# LANGUAGE CApiFFI #-}

module Hasql.LibPq14.Ffi where

import Foreign.C.Types (CInt (..))

import Hasql.Prelude

foreign import capi "libpq-fe.h PQresultStatus"
  resultStatus :: Ptr () -> IO CInt
