{-|
Module      : PostgREST.Common
Description : Common helper functions.
-}
module PostgREST.Private.Common where

import           Data.Maybe
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import           Protolude

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

nullableColumn :: HD.Value a -> HD.Row (Maybe a)
nullableColumn = HD.column . HD.nullable

element :: HD.Value a -> HD.Array a
element = HD.element . HD.nonNullable

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable

arrayParam :: HE.Value a -> HE.Params [a]
arrayParam = param . HE.array . HE.dimension foldl' . HE.element . HE.nonNullable
