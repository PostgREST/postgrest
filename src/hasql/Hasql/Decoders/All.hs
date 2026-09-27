{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

-- |
-- A DSL for declaration of result decoders.
module Hasql.Decoders.All where

import PostgreSQL.Binary.Decoding qualified as A

import Hasql.Prelude hiding
  ( init
  , maybe
  , replicateM
  )

import Hasql.Decoders.Array qualified as Array
import Hasql.Decoders.Composite qualified as Composite
import Hasql.Decoders.Result qualified as Result
import Hasql.Decoders.Results qualified as Results
import Hasql.Decoders.Row qualified as Row
import Hasql.Decoders.Value qualified as Value
import Hasql.Prelude qualified as Prelude

-- * Result

-- |
-- Decoder of a query result.
newtype Result a = Result (Results.Results a) deriving (Filterable, Functor)

-- |
-- Decode no value from the result.
--
-- Useful for statements like @INSERT@ or @CREATE@.
{-# INLINEABLE noResult #-}
noResult :: Result ()
noResult = Result (Results.single Result.noResult)

-- |
-- Exactly one row.
-- Will raise the 'Errors.UnexpectedAmountOfRows' error if it's any other.
{-# INLINEABLE singleRow #-}
singleRow :: Row a -> Result a
singleRow (Row row) = Result (Results.single (Result.single row))

refineResult :: (a -> Either Text b) -> Result a -> Result b
refineResult refiner (Result results) = Result (Results.refine refiner results)

-- ** Multi-row traversers

-- |
-- Foldr multiple rows.
{-# INLINEABLE foldrRows #-}
foldrRows :: (b -> a -> a) -> a -> Row b -> Result a
foldrRows step init (Row row) = Result (Results.single (Result.foldr step init row))

-- ** Specialized multi-row results

-- |
-- Maybe one row or none.
{-# INLINEABLE rowMaybe #-}
rowMaybe :: Row a -> Result (Maybe a)
rowMaybe (Row row) = Result (Results.single (Result.maybe row))

-- |
-- Zero or more rows packed into the list.
{-# INLINEABLE rowList #-}
rowList :: Row a -> Result [a]
rowList = foldrRows strictCons []

-- * Row

-- |
-- Decoder of an individual row,
-- which gets composed of column value decoders.
--
-- E.g.:
--
-- @
-- x :: 'Row' (Maybe Int64, Text, TimeOfDay)
-- x = (,,) '<$>' ('column' . 'nullable') 'int8' '<*>' ('column' . 'nonNullable') 'text' '<*>' ('column' . 'nonNullable') 'time'
-- @
newtype Row a = Row (Row.Row a)
  deriving (Applicative, Functor, Monad, MonadFail)

-- |
-- Lift an individual value decoder to a composable row decoder.
{-# INLINEABLE column #-}
column :: NullableOrNot Value a -> Row a
column = \case
  NonNullable (Value imp) -> Row (Row.nonNullValue imp)
  Nullable (Value imp) -> Row (Row.value imp)

-- * Nullability

-- |
-- Extensional specification of nullability over a generic decoder.
data NullableOrNot decoder a where
  NonNullable :: decoder a -> NullableOrNot decoder a
  Nullable :: decoder a -> NullableOrNot decoder (Maybe a)

-- |
-- Specify that a decoder produces a non-nullable value.
nonNullable :: decoder a -> NullableOrNot decoder a
nonNullable = NonNullable

-- |
-- Specify that a decoder produces a nullable value.
nullable :: decoder a -> NullableOrNot decoder (Maybe a)
nullable = Nullable

-- * Value

-- |
-- Decoder of a value.
newtype Value a = Value (Value.Value a)
  deriving (Filterable, Functor)

type role Value representational

-- |
-- Decoder of the @BOOL@ values.
{-# INLINEABLE bool #-}
bool :: Value Bool
bool = Value (Value.decoder (const A.bool))

-- |
-- Decoder of the @INT4@ values.
{-# INLINEABLE int4 #-}
int4 :: Value Int32
int4 = Value (Value.decoder (const A.int))

-- |
-- Decoder of the @INT8@ values.
{-# INLINEABLE int8 #-}
int8 :: Value Int64
int8 =
  {-# SCC "int8" #-}
  Value (Value.decoder (const ({-# SCC "int8.int" #-} A.int)))

-- |
-- Decoder of the @CHAR@ values.
-- Note that it supports Unicode values.
{-# INLINEABLE char #-}
char :: Value Char
char = Value (Value.decoder (const A.char))

-- |
-- Decoder of the @TEXT@ values.
{-# INLINEABLE text #-}
text :: Value Text
text = Value (Value.decoder (const A.text_strict))

-- |
-- Decoder of the @BYTEA@ values.
{-# INLINEABLE bytea #-}
bytea :: Value ByteString
bytea = Value (Value.decoder (const A.bytea_strict))

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINEABLE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn (Value v) = Value (Value.refine fn v)

-- |
-- Lift an 'Array' decoder to a 'Value' decoder.
{-# INLINEABLE array #-}
array :: Array a -> Value a
array (Array imp) = Value (Value.decoder (Array.run imp))

-- |
-- Lift a value decoder of element into a unidimensional array decoder producing a list.
--
-- This function is merely a shortcut to the following expression:
--
-- @
-- ('array' . 'dimension' Control.Monad.'replicateM' . 'element')
-- @
--
-- Please notice that in case of multidimensional arrays nesting 'listArray' decoder
-- won't work. You have to explicitly construct the array decoder using 'array'.
{-# INLINE listArray #-}
listArray :: NullableOrNot Value element -> Value [element]
listArray = array . dimension Prelude.replicateM . element

-- |
-- Lift a 'Composite' decoder to a 'Value' decoder.
{-# INLINEABLE composite #-}
composite :: Composite a -> Value a
composite (Composite imp) = Value (Value.decoder (Composite.run imp))

-- * Array decoders

-- |
-- A generic array decoder.
--
-- Here's how you can use it to produce a specific array value decoder:
--
-- @
-- x :: 'Value' [[Text]]
-- x = 'array' ('dimension' 'replicateM' ('dimension' 'replicateM' ('element' ('nonNullable' 'text'))))
-- @
newtype Array a = Array (Array.Array a)
  deriving (Functor)

-- |
-- A function for parsing a dimension of an array.
-- Provides support for multi-dimensional arrays.
--
-- Accepts:
--
-- * An implementation of the @replicateM@ function
-- (@Control.Monad.'Control.Monad.replicateM'@, @Data.Vector.'Data.Vector.replicateM'@),
-- which determines the output value.
--
-- * A decoder of its components, which can be either another 'dimension' or 'element'.
{-# INLINEABLE dimension #-}
dimension :: (forall m. (Monad m) => Int -> m a -> m b) -> Array a -> Array b
dimension replicateM (Array imp) = Array (Array.dimension replicateM imp)

-- |
-- Lift a 'Value' decoder into an 'Array' decoder for parsing of leaf values.
{-# INLINEABLE element #-}
element :: NullableOrNot Value a -> Array a
element = \case
  NonNullable (Value imp) -> Array (Array.nonNullValue (Value.run imp))
  Nullable (Value imp) -> Array (Array.value (Value.run imp))

-- * Composite decoders

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a = Composite (Composite.Composite a)
  deriving (Applicative, Functor, Monad, MonadFail)

-- |
-- Lift a 'Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot Value a -> Composite a
field = \case
  NonNullable (Value imp) -> Composite (Composite.nonNullValue (Value.run imp))
  Nullable (Value imp) -> Composite (Composite.value (Value.run imp))
