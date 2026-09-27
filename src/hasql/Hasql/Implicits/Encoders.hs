{-# LANGUAGE CPP #-}

module Hasql.Implicits.Encoders where

import Hasql.Encoders
import Hasql.Implicits.Prelude hiding (bool)

-- | Provides a default implementation of parameter encoder.
class DefaultParamEncoder a where
  -- | Default parameter encoder with nullability specified.
  defaultParam :: NullableOrNot Value a

#define INSTANCES(VALUE, ENCODER) \
instance DefaultParamEncoder VALUE where { \
  defaultParam = nonNullable ENCODER; \
}; \
instance DefaultParamEncoder [VALUE] where { \
  defaultParam = (nonNullable . array . dimension foldlStrict . element . nonNullable) ENCODER; \
}; \
instance DefaultParamEncoder [Maybe VALUE] where { \
  defaultParam = (nonNullable . array . dimension foldlStrict . element . nullable) ENCODER; \
}; \
instance DefaultParamEncoder [[VALUE]] where { \
  defaultParam = (nonNullable . array . dimension foldlStrict . dimension foldlStrict . element . nonNullable) ENCODER; \
}; \
instance DefaultParamEncoder [[Maybe VALUE]] where { \
  defaultParam = (nonNullable . array . dimension foldlStrict . dimension foldlStrict . element . nullable) ENCODER; \
}; \
instance DefaultParamEncoder (Maybe VALUE) where { \
  defaultParam = nullable ENCODER; \
}; \
instance DefaultParamEncoder (Maybe [VALUE]) where { \
  defaultParam = (nullable . array . dimension foldlStrict . element . nonNullable) ENCODER; \
}; \
instance DefaultParamEncoder (Maybe [Maybe VALUE]) where { \
  defaultParam = (nullable . array . dimension foldlStrict . element . nullable) ENCODER; \
}; \
instance DefaultParamEncoder (Maybe [[VALUE]]) where { \
  defaultParam = (nullable . array . dimension foldlStrict . dimension foldlStrict . element . nonNullable) ENCODER; \
}; \
instance DefaultParamEncoder (Maybe [[Maybe VALUE]]) where { \
  defaultParam = (nullable . array . dimension foldlStrict . dimension foldlStrict . element . nullable) ENCODER; \
}; \

{- ORMOLU_DISABLE -}
INSTANCES(Int32, int4)
INSTANCES(ByteString, bytea)
INSTANCES(Text, text)
{- ORMOLU_ENABLE -}

#undef INSTANCES
