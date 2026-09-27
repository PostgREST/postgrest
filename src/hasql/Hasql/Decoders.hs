-- |
-- A DSL for declaration of result decoders.
module Hasql.Decoders
  ( -- * Result
    Result
  , noResult
  , singleRow

    -- ** Specialized multi-row results
  , rowMaybe
  , rowList

    -- ** Multi-row traversers
  , foldrRows

    -- * Row
  , Row
  , column

    -- * Nullability
  , NullableOrNot
  , nonNullable
  , nullable

    -- * Value
  , Value
  , bool
  , int4
  , int8
  , char
  , text
  , bytea
  , array
  , listArray
  , composite

    -- * Array
  , Array
  , dimension
  , element

    -- * Composite
  , Composite
  , field
  )
where

import Hasql.Decoders.All
