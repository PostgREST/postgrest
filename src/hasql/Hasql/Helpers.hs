-- |
-- Hasql helper functions module
module Hasql.Helpers
  ( arrayColumn
  , arrayParam
  , column
  , compositeArrayColumn
  , compositeField
  , compositeFieldArray
  , nullableColumn
  , nullableCompositeField
  , param
  )
where

import Hasql.Prelude

import Hasql.Decoders qualified as HD
import Hasql.Encoders qualified as HE

arrayColumn :: HD.Value a -> HD.Row [a]
arrayColumn = column . HD.listArray . HD.nonNullable

arrayParam :: HE.Value a -> HE.Params [a]
arrayParam = param . HE.foldableArray . HE.nonNullable

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

compositeArrayColumn :: HD.Composite a -> HD.Row [a]
compositeArrayColumn = arrayColumn . HD.composite

compositeField :: HD.Value a -> HD.Composite a
compositeField = HD.field . HD.nonNullable

compositeFieldArray :: HD.Value a -> HD.Composite [a]
compositeFieldArray = HD.field . HD.nonNullable . HD.listArray . HD.nonNullable

nullableColumn :: HD.Value a -> HD.Row (Maybe a)
nullableColumn = HD.column . HD.nullable

nullableCompositeField :: HD.Value a -> HD.Composite (Maybe a)
nullableCompositeField = HD.field . HD.nullable

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable
