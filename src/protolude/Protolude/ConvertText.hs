{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

-- | Non-partial text conversion typeclass and functions.
-- For an alternative with partial conversions import 'Protolude.Conv'.
module Protolude.ConvertText (
  ConvertText (toS)
, toUtf8
, toUtf8Lazy
) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT

import Data.Function (id, (.))
import Data.String (String)
import Data.Text.Encoding (encodeUtf8)

-- | Convert from one Unicode textual type to another. Not for serialization/deserialization,
-- so doesn't have instances for bytestrings.
class ConvertText a b where
  toS :: a -> b

instance ConvertText String String where toS = id
instance ConvertText String T.Text where toS = T.pack
instance ConvertText String LT.Text where toS = LT.pack

instance ConvertText T.Text String where toS = T.unpack
instance ConvertText T.Text LT.Text where toS = LT.fromStrict
instance ConvertText T.Text T.Text where toS = id

instance ConvertText LT.Text String where toS = LT.unpack
instance ConvertText LT.Text T.Text where toS = LT.toStrict
instance ConvertText LT.Text LT.Text where toS = id

instance ConvertText LB.ByteString B.ByteString where toS = LB.toStrict
instance ConvertText LB.ByteString LB.ByteString where toS = id

instance ConvertText B.ByteString B.ByteString where toS = id
instance ConvertText B.ByteString LB.ByteString where toS = LB.fromStrict

toUtf8 :: ConvertText a T.Text => a -> B.ByteString
toUtf8 =
  encodeUtf8 . toS

toUtf8Lazy :: ConvertText a T.Text => a -> LB.ByteString
toUtf8Lazy =
  LB.fromStrict . encodeUtf8 . toS
