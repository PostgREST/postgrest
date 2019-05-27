{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module:      Data.Configurator.FromValue
-- Copyright:   (c) 2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>

module Data.Configurator.FromValue
     ( MaybeParser
     , runMaybeParser
     , FromMaybeValue(..)
     , optionalValue
     , requiredValue
     , ValueParser
     , runValueParser
     , FromValue(..)
     , ListParser
     , FromListValue(..)
     , listValue
     , listValue'
     , listElem
     , ConversionError(..)
     , ConversionErrorWhy(..)
     , defaultConversionError
     -- * Assorted primitive value parsers
     , boolValue
     , boundedIntegerValue
     , integralValue
     , fractionalValue
     , realFloatValue
     , fixedValue
     , scientificValue
     , textValue
     , charValue
     , typeError
     , valueError
     , extraValuesError
     , missingValueError
     ) where

import Data.Configurator.FromValue.Implementation
import Data.Configurator.Types
