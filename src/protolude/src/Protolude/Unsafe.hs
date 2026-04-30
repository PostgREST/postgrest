{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Unsafe (
  unsafeHead,
  unsafeTail,
  unsafeInit,
  unsafeLast,
  unsafeFromJust,
  unsafeIndex,
  unsafeThrow,
  unsafeRead,
) where

import Protolude.Base (Int)

#if ( __GLASGOW_HASKELL__ >= 800 )
import Protolude.Base (HasCallStack)
#endif
import Data.Char (Char)
import Text.Read (Read, read)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Control.Exception as Exc

unsafeThrow :: Exc.Exception e => e -> a
unsafeThrow = Exc.throw

#if ( __GLASGOW_HASKELL__ >= 800 )
unsafeHead :: HasCallStack => [a] -> a
unsafeHead = List.head

unsafeTail :: HasCallStack => [a] -> [a]
unsafeTail = List.tail

unsafeInit :: HasCallStack => [a] -> [a]
unsafeInit = List.init

unsafeLast :: HasCallStack => [a] -> a
unsafeLast = List.last

unsafeFromJust :: HasCallStack => Maybe.Maybe a -> a
unsafeFromJust = Maybe.fromJust

unsafeIndex :: HasCallStack => [a] -> Int -> a
unsafeIndex = (List.!!)

unsafeRead :: (HasCallStack, Read a) => [Char] -> a
unsafeRead = Text.Read.read
#endif


#if ( __GLASGOW_HASKELL__ < 800 )
unsafeHead :: [a] -> a
unsafeHead = List.head

unsafeTail :: [a] -> [a]
unsafeTail = List.tail

unsafeInit :: [a] -> [a]
unsafeInit = List.init

unsafeLast :: [a] -> a
unsafeLast = List.last

unsafeFromJust :: Maybe.Maybe a -> a
unsafeFromJust = Maybe.fromJust

unsafeIndex :: [a] -> Int -> a
unsafeIndex = (List.!!)

unsafeRead :: Read a => [Char] -> a
unsafeRead = Text.Read.read
#endif
