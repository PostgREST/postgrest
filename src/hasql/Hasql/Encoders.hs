-- |
-- A DSL for declaration of statement parameter encoders.
--
-- For compactness of names all the types defined here imply being an encoder.
-- E.g., the `Array` type is an __encoder__ of arrays, not the data-structure itself.
module Hasql.Encoders
  ( -- * Parameters product
    Params
  , noParams
  , param

    -- * Nullability
  , NullableOrNot
  , nonNullable
  , nullable

    -- * Value
  , Value
  , int4
  , text
  , bytea
  , jsonLazyBytes
  , jsonbLazyBytes
  , unknown
  , array
  , foldableArray

    -- * Array
  , Array
  , element
  , dimension

    -- * Composite
  , Composite
  )
where

import Hasql.Encoders.All
