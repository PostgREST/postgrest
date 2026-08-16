{-# LANGUAGE GADTs #-}

-- |
-- A DSL for declaration of query parameter encoders.
module Hasql.Encoders.All where

import Data.ByteString.Lazy qualified as LazyByteString
import PostgreSQL.Binary.Encoding qualified as A
import TextBuilder qualified as C

import Hasql.Prelude hiding (foldl, print)

import Hasql.Encoders.Array qualified as Array
import Hasql.Encoders.Params qualified as Params
import Hasql.Encoders.Value qualified as Value
import Hasql.PostgresTypeInfo qualified as PTI

-- * Parameters Product Encoder

-- |
-- Encoder of some representation of a parameters product.
--
-- Has instances of 'Contravariant', 'Divisible' and 'Monoid',
-- which you can use to compose multiple parameters together.
-- E.g.,
--
-- @
-- someParamsEncoder :: 'Params' (Int64, Maybe Text)
-- someParamsEncoder =
--   ('fst' '>$<' 'param' ('nonNullable' 'int8')) '<>'
--   ('snd' '>$<' 'param' ('nullable' 'text'))
-- @
--
-- As a general solution for tuples of any arity, instead of 'fst' and 'snd',
-- consider the functions of the @contrazip@ family
-- from the \"contravariant-extras\" package.
-- E.g., here's how you can achieve the same as the above:
--
-- @
-- someParamsEncoder :: 'Params' (Int64, Maybe Text)
-- someParamsEncoder =
--   'contrazip2' ('param' ('nonNullable' 'int8')) ('param' ('nullable' 'text'))
-- @
--
-- Here's how you can implement encoders for custom composite types:
--
-- @
-- data Person = Person { name :: Text, gender :: Gender, age :: Int }
--
-- data Gender = Male | Female
--
-- personParams :: 'Params' Person
-- personParams =
--   (name '>$<' 'param' ('nonNullable' 'text')) '<>'
--   (gender '>$<' 'param' ('nonNullable' genderValue)) '<>'
--   ('fromIntegral' . age '>$<' 'param' ('nonNullable' 'int8'))
--
-- genderValue :: 'Value' Gender
-- genderValue = 'enum' genderText 'text' where
--   genderText gender = case gender of
--     Male -> "male"
--     Female -> "female"
-- @
newtype Params a = Params (Params.Params a)
  deriving (Contravariant, Monoid, Semigroup)

-- |
-- No parameters. Same as `mempty` and `conquered`.
noParams :: Params ()
noParams = mempty

-- |
-- Lift a single parameter encoder, with its nullability specified,
-- associating it with a single placeholder.
param :: NullableOrNot Value a -> Params a
param = \case
  NonNullable (Value valueEnc) -> Params (Params.value valueEnc)
  Nullable (Value valueEnc) -> Params (Params.nullableValue valueEnc)

-- * Nullability

-- |
-- Extensional specification of nullability over a generic encoder.
data NullableOrNot encoder a where
  NonNullable :: encoder a -> NullableOrNot encoder a
  Nullable :: encoder a -> NullableOrNot encoder (Maybe a)

-- |
-- Specify that an encoder produces a non-nullable value.
nonNullable :: encoder a -> NullableOrNot encoder a
nonNullable = NonNullable

-- |
-- Specify that an encoder produces a nullable value.
nullable :: encoder a -> NullableOrNot encoder (Maybe a)
nullable = Nullable

-- * Value

-- |
-- Value encoder.
newtype Value a = Value (Value.Value a)

-- |
-- Encoder of @INT4@ values.
{-# INLINEABLE int4 #-}
int4 :: Value Int32
int4 = Value (Value.unsafePTIWithShow PTI.int4 (const A.int4_int32))

-- |
-- Encoder of @TEXT@ values.
{-# INLINEABLE text #-}
text :: Value Text
text = Value (Value.unsafePTIWithShow PTI.text (const A.text_strict))

-- |
-- Encoder of @BYTEA@ values.
{-# INLINEABLE bytea #-}
bytea :: Value ByteString
bytea = Value (Value.unsafePTIWithShow PTI.bytea (const A.bytea_strict))

-- |
-- Encoder of @JSON@ values from raw JSON as lazy ByteString.
{-# INLINEABLE jsonLazyBytes #-}
jsonLazyBytes :: Value LazyByteString.ByteString
jsonLazyBytes = Value (Value.unsafePTIWithShow PTI.json (const A.json_bytes_lazy))

-- |
-- Encoder of @JSONB@ values from raw JSON as lazy ByteString.
{-# INLINEABLE jsonbLazyBytes #-}
jsonbLazyBytes :: Value LazyByteString.ByteString
jsonbLazyBytes = Value (Value.unsafePTIWithShow PTI.jsonb (const A.jsonb_bytes_lazy))

-- |
-- Identifies the value with the PostgreSQL's \"unknown\" type,
-- thus leaving it up to Postgres to infer the actual type of the value.
--
-- The value transimitted is any value encoded in the Postgres' Text data format.
-- For reference, see the
-- <https://www.postgresql.org/docs/10/static/protocol-overview.html#PROTOCOL-FORMAT-CODES Formats and Format Codes>
-- section of the Postgres' documentation.
--
-- __Warning:__ Do not use this as part of composite encoders like 'array' since
-- it is the only encoder that doesn't use the binary format.
{-# INLINEABLE unknown #-}
unknown :: Value ByteString
unknown = Value (Value.unsafePTIWithShow PTI.textUnknown (const A.bytea_strict))

-- |
-- Lift an array encoder into a value encoder.
array :: Array a -> Value a
array (Array (Array.Array valueOID arrayOID arrayEncoder renderer)) =
  let encoder env input = A.array (PTI.oidWord32 valueOID) (arrayEncoder env input)
  in  Value (Value.Value arrayOID arrayOID encoder renderer)

-- |
-- Lift a value encoder of element into a unidimensional array encoder of a foldable value.
--
-- This function is merely a shortcut to the following expression:
--
-- @
-- ('array' . 'dimension' 'foldl'' . 'element')
-- @
--
-- You can use it like this:
--
-- @
-- vectorOfInts :: Value (Vector Int64)
-- vectorOfInts = 'foldableArray' ('nonNullable' 'int8')
-- @
--
-- Please notice that in case of multidimensional arrays nesting 'foldableArray' encoder
-- won't work. You have to explicitly construct the array encoder using 'array'.
{-# INLINE foldableArray #-}
foldableArray :: (Foldable foldable) => NullableOrNot Value element -> Value (foldable element)
foldableArray = array . dimension foldl' . element

-- * Array

-- |
-- Generic array encoder.
--
-- Here's an example of its usage:
--
-- @
-- someParamsEncoder :: 'Params' [[Int64]]
-- someParamsEncoder = 'param' ('nonNullable' ('array' ('dimension' 'foldl'' ('dimension' 'foldl'' ('element' ('nonNullable' 'int8'))))))
-- @
--
-- Please note that the PostgreSQL @IN@ keyword does not accept an array, but rather a syntactical list of
-- values, thus this encoder is not suited for that. Use a @value = ANY($1)@ condition instead.
newtype Array a = Array (Array.Array a)

-- |
-- Lifts a 'Value' encoder into an 'Array' encoder.
element :: NullableOrNot Value a -> Array a
element = \case
  NonNullable (Value (Value.Value elementOID arrayOID encoder renderer)) ->
    Array (Array.value elementOID arrayOID encoder renderer)
  Nullable (Value (Value.Value elementOID arrayOID encoder renderer)) ->
    Array (Array.nullableValue elementOID arrayOID encoder renderer)

-- |
-- Encoder of an array dimension,
-- which thus provides support for multidimensional arrays.
--
-- Accepts:
--
-- * An implementation of the left-fold operation,
-- such as @Data.Foldable.'foldl''@,
-- which determines the input value.
--
-- * A component encoder, which can be either another 'dimension' or 'element'.
{-# INLINEABLE dimension #-}
dimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array b -> Array c
dimension foldl (Array imp) = Array (Array.dimension foldl imp)

-- * Composite

-- |
-- Composite or row-types encoder.
data Composite a
  = Composite
      (a -> Bool -> A.Composite)
      (a -> [C.TextBuilder])
