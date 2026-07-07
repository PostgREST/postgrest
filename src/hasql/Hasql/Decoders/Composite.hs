module Hasql.Decoders.Composite where

import Hasql.Prelude
import PostgreSQL.Binary.Decoding qualified as A

newtype Composite a
  = Composite (ReaderT Bool A.Composite a)
  deriving (Functor, Applicative, Monad, MonadFail)

{-# INLINE run #-}
run :: Composite a -> Bool -> A.Value a
run (Composite imp) env =
  A.composite (runReaderT imp env)

{-# INLINE value #-}
value :: (Bool -> A.Value a) -> Composite (Maybe a)
value decoder' =
  Composite $ ReaderT $ A.nullableValueComposite . decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: (Bool -> A.Value a) -> Composite a
nonNullValue decoder' =
  Composite $ ReaderT $ A.valueComposite . decoder'
