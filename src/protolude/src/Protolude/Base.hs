{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Protolude.Base (
  module Base,
  ($!),
) where

-- Glorious Glasgow Haskell Compiler
#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ >= 600 )

-- Base GHC types
import GHC.Num as Base (
    Num(
      (+),
      (-),
      (*),
      negate,
      abs,
      signum,
      fromInteger
    )
  , Integer
  , subtract
  )
import GHC.Enum as Base (
    Bounded(minBound, maxBound)
  , Enum(
      succ,
      pred,
      toEnum,
      fromEnum,
      enumFrom,
      enumFromThen,
      enumFromTo,
      enumFromThenTo
    )
  , boundedEnumFrom
  , boundedEnumFromThen
  )
import GHC.Real as Base (
    (%)
  , (/)
  , Fractional
  , Integral
  , Ratio
  , Rational
  , Real
  , RealFrac
  , (^)
  , (^%^)
  , (^^)
  , (^^%^^)
  , ceiling
  , denominator
  , div
  , divMod
#if MIN_VERSION_base(4,7,0)
  , divZeroError
#endif
  , even
  , floor
  , fromIntegral
  , fromRational
  , gcd
#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,15,0)
#if defined(MIN_VERSION_integer_gmp)
  , gcdInt'
  , gcdWord'
#endif
#endif
  , infinity
  , integralEnumFrom
  , integralEnumFromThen
  , integralEnumFromThenTo
  , integralEnumFromTo
  , lcm
  , mod
  , notANumber
  , numerator
  , numericEnumFrom
  , numericEnumFromThen
  , numericEnumFromThenTo
  , numericEnumFromTo
  , odd
#if MIN_VERSION_base(4,7,0)
  , overflowError
#endif
  , properFraction
  , quot
  , quotRem
  , ratioPrec
  , ratioPrec1
#if MIN_VERSION_base(4,7,0)
  , ratioZeroDenominatorError
#endif
  , realToFrac
  , recip
  , reduce
  , rem
  , round
  , showSigned
  , toInteger
  , toRational
  , truncate
#if MIN_VERSION_base(4,12,0)
  , underflowError
#endif
  )
import GHC.Float as Base (
    Float(F#)
  , Double(D#)
  , Floating (..)
  , RealFloat(..)
  , showFloat
  , showSignedFloat
  )
import GHC.Show as Base (
    Show(showsPrec, show, showList)
  )
import GHC.Exts as Base (
    Constraint
  , Ptr
  , FunPtr
  )
import GHC.Base as Base (
    (++)
  , seq
  , asTypeOf
  , ord
  , maxInt
  , minInt
  , until
  )

-- Exported for lifting into new functions.
import System.IO as Base (
    print
  , putStr
  , putStrLn
  )

import GHC.Types as Base (
    Bool
  , Char
  , Int
  , Word
  , Ordering
  , IO
#if ( __GLASGOW_HASKELL__ >= 710 )
  , Coercible
#endif
  )

#if ( __GLASGOW_HASKELL__ >= 710 )
import GHC.StaticPtr as Base (StaticPtr)
#endif

#if ( __GLASGOW_HASKELL__ >= 800 )
import GHC.OverloadedLabels as Base (
    IsLabel(fromLabel)
  )

import GHC.ExecutionStack as Base (
    Location(Location, srcLoc, objectName, functionName)
  , SrcLoc(SrcLoc, sourceColumn, sourceLine, sourceColumn)
  , getStackTrace
  , showStackTrace
  )

import GHC.Stack as Base (
    CallStack
  , type HasCallStack
  , callStack
  , prettySrcLoc
  , currentCallStack
  , getCallStack
  , prettyCallStack
  , withFrozenCallStack
  )
#endif

#if ( __GLASGOW_HASKELL__ >= 710 )
import GHC.TypeLits as Base (
    Symbol
  , SomeSymbol(SomeSymbol)
  , Nat
  , SomeNat(SomeNat)
  , CmpNat
  , KnownSymbol
  , KnownNat
  , natVal
  , someNatVal
  , symbolVal
  , someSymbolVal
  )
#endif

#if ( __GLASGOW_HASKELL__ >= 802 )
import GHC.Records as Base (
    HasField(getField)
  )
#endif

#if ( __GLASGOW_HASKELL__ >= 800 )
import Data.Kind as Base (
  type Type
#if ( __GLASGOW_HASKELL__ < 805 )
  , type (*)
#endif
  , type Type
  )
#endif

-- Default Prelude defines this at the toplevel module, so we do as well.
infixr 0 $!

($!) :: (a -> b) -> a -> b
f $! x  = let !vx = x in f vx

#endif
