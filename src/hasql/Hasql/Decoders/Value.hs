module Hasql.Decoders.Value where

import PostgreSQL.Binary.Decoding qualified as A

import Hasql.Prelude

newtype Value a
  = Value (Bool -> A.Value a)
  deriving (Functor)

{-# INLINE run #-}
run :: Value a -> Bool -> A.Value a
run (Value imp) = imp

{-# INLINE decoder #-}
decoder :: (Bool -> A.Value a) -> Value a
decoder =
  {-# SCC "decoder" #-}
  Value
