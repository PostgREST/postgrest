{-# LANGUAGE CPP, FlexibleInstances, GeneralizedNewtypeDeriving #-}
-- |
-- Module      :  Data.CritBit.Types.Internal
-- Copyright   :  (c) Bryan O'Sullivan and others 2013-2014
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Data.CritBit.Types.Internal
    (
      CritBitKey(..)
    , CritBit(..)
    , Set(..)
    , BitMask
    , Node(..)
    , foldlWithKey
    , foldlWithKey'
    , foldrWithKey
    , foldrWithKey'
    , toList
    ) where

import Prelude
import Control.DeepSeq (NFData(..))
import Data.Bits (Bits, (.|.), (.&.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Monoid (Monoid(..))
import Data.Text ()
import Data.Text.Internal (Text(..))
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.Array as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

type BitMask = Word16

data Node k v =
    Internal {
      ileft, iright :: !(Node k v)
    , ibyte         :: !Int
    -- ^ The byte at which the left and right subtrees differ.
    , iotherBits    :: !BitMask
    -- ^ The bitmask representing the critical bit within the
    -- differing byte. If the critical bit is e.g. 0x8, the bitmask
    -- will have every bit other than 0x8 set, hence 0x1F7
    -- (the ninth bit is set because we're using 9 bits for representing
    -- bytes).
    }
    | Leaf k v
    | Empty
    -- ^ Logically, the 'Empty' constructor is a property of the tree,
    -- rather than a node (a non-empty tree will never contain any
    -- 'Empty' constructors). In practice, turning 'CritBit' from a
    -- newtype into an ADT with an 'Empty' constructor adds a
    -- pattern-match and a memory indirection to every function, which
    -- slows them all down.
      deriving (Eq, Show)

instance (NFData k, NFData v) => NFData (Node k v) where
    rnf (Internal l r _ _) = rnf l `seq` rnf r
    rnf (Leaf k v)         = rnf k `seq` rnf v
    rnf Empty              = ()

instance Functor (Node k) where
    fmap f i@(Internal l r _ _) = i { ileft = fmap f l, iright = fmap f r }
    fmap f (Leaf k v)           = Leaf k (f v)
    fmap _ Empty                = Empty

instance Foldable (Node k) where
    foldl f z m = foldlWithKey (\a _ v -> f a v) z (CritBit m)
    foldr f z m = foldrWithKey (\_ v a -> f v a) z (CritBit m)

    foldMap f (Internal l r _ _) = mappend (foldMap f l) (foldMap f r)
    foldMap f (Leaf _ v)         = f v
    foldMap _ Empty              = mempty
    {-# INLINABLE foldMap #-}

-- | /O(n)/. Fold the keys and values in the map using the given
-- left-associative function, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- Examples:
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ show k ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [("a",5), ("b",3)]) == "Map: (b:3)(a:5)"
foldlWithKey :: (a -> k -> v -> a) -> a -> CritBit k v -> a
foldlWithKey f z m = foldlWithKeyWith (\_ b -> b) f z m
{-# INLINABLE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of
-- the function is evaluated before using the result in the next
-- application. This function is strict in the starting value.
foldlWithKey' :: (a -> k -> v -> a) -> a -> CritBit k v -> a
foldlWithKey' f z m = foldlWithKeyWith seq f z m
{-# INLINABLE foldlWithKey' #-}

-- | /O(n)/. Fold the keys and values in the map using the given
-- right-associative function, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- Examples:
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ show k ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [("a",5), ("b",3)]) == "Map: (a:5)(b:3)"
foldrWithKey :: (k -> v -> a -> a) -> a -> CritBit k v -> a
foldrWithKey f z m = foldrWithKeyWith (\_ b -> b) f z m
{-# INLINABLE foldrWithKey #-}

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of
-- the function is evaluated before using the result in the next
-- application. This function is strict in the starting value.
foldrWithKey' :: (k -> v -> a -> a) -> a -> CritBit k v -> a
foldrWithKey' f z m = foldrWithKeyWith seq f z m
{-# INLINABLE foldrWithKey' #-}

foldlWithKeyWith :: (a -> a -> a) -> (a -> k -> v -> a) -> a -> CritBit k v -> a
foldlWithKeyWith maybeSeq f z0 (CritBit root) = go z0 root
  where
    go z (Internal left right _ _) = let z' = go z left
                                     in z' `maybeSeq` go z' right
    go z (Leaf k v)                = f z k v
    go z Empty                     = z
{-# INLINE foldlWithKeyWith #-}

foldrWithKeyWith :: (a -> a -> a) -> (k -> v -> a -> a) -> a -> CritBit k v -> a
foldrWithKeyWith maybeSeq f z0 (CritBit root) = go root z0
  where
    go (Internal left right _ _) z = let z' = go right z
                                     in z' `maybeSeq` go left z'
    go (Leaf k v) z                = f k v z
    go Empty z                     = z
{-# INLINE foldrWithKeyWith #-}

-- | A crit-bit tree.
newtype CritBit k v = CritBit (Node k v)
                      deriving (Eq, NFData, Functor, Foldable)

instance (Show k, Show v) => Show (CritBit k v) where
    show t = "fromList " ++ show (toList t)

-- | A type that can be used as a key in a crit-bit tree.
--
-- We use 9 bits to represent 8-bit bytes so that we can distinguish
-- between an interior byte that is zero (which must have the 9th bit
-- set) and a byte past the end of the input (which must /not/ have
-- the 9th bit set).
--
-- Without this trick, the critical bit calculations would fail on
-- zero bytes /within/ a string, and our tree would be unable to
-- handle arbitrary binary data.
class (Eq k) => CritBitKey k where
    -- | Return the number of bytes used by this key.
    --
    -- For reasonable performance, implementations must be inlined and
    -- /O(1)/.
    byteCount :: k -> Int

    -- | Return the byte at the given offset (counted in bytes) of
    -- this key, bitwise-ORed with 256. If the offset is past the end
    -- of the key, return zero.
    --
    -- For reasonable performance, implementations must be inlined and
    -- /O(1)/.
    getByte :: k -> Int -> Word16

instance CritBitKey ByteString where
    byteCount = B.length
    {-# INLINE byteCount #-}

    getByte bs n
        | n < B.length bs = fromIntegral (B.unsafeIndex bs n) .|. 256
        | otherwise       = 0
    {-# INLINE getByte #-}

instance CritBitKey Text where
    byteCount (Text _ _ len) = len `shiftL` 1
    {-# INLINE byteCount #-}

    getByte (Text arr off len) n
        | n < len `shiftL` 1 =
            let word       = T.unsafeIndex arr (off + (n `shiftR` 1))
                byteInWord = (word `shiftR` ((n .&. 1) `shiftL` 3)) .&. 0xff
            in byteInWord .|. 256
        | otherwise       = 0
    {-# INLINE getByte #-}

#if WORD_SIZE_IN_BITS == 64
# define WORD_SHIFT 3
#else
# define WORD_SHIFT 2
#endif

instance CritBitKey (U.Vector Word8) where
    byteCount = G.length
    getByte   = getByteV 0

instance CritBitKey (U.Vector Word16) where
    byteCount = (`shiftL` 1) . G.length
    getByte   = getByteV 1

instance CritBitKey (U.Vector Word32) where
    byteCount = (`shiftL` 2) . G.length
    getByte   = getByteV 2

instance CritBitKey (U.Vector Word64) where
    byteCount = (`shiftL` 3) . G.length
    getByte   = getByteV 3

instance CritBitKey (U.Vector Word) where
    byteCount = (`shiftL` WORD_SHIFT) . G.length
    getByte   = getByteV WORD_SHIFT

instance CritBitKey (U.Vector Char) where
    byteCount = (`shiftL` 2) . G.length
    getByte   = getByteV_ fromEnum 2

instance CritBitKey (V.Vector Word8) where
    byteCount = G.length
    getByte   = getByteV 0

instance CritBitKey (V.Vector Word16) where
    byteCount = (`shiftL` 1) . G.length
    getByte   = getByteV 1

instance CritBitKey (V.Vector Word32) where
    byteCount = (`shiftL` 2) . G.length
    getByte   = getByteV 2

instance CritBitKey (V.Vector Word64) where
    byteCount = (`shiftL` 3) . G.length
    getByte   = getByteV 3

instance CritBitKey (V.Vector Word) where
    byteCount = (`shiftL` WORD_SHIFT) . G.length
    getByte   = getByteV WORD_SHIFT

instance CritBitKey (V.Vector Char) where
    byteCount = (`shiftL` 2) . G.length
    getByte   = getByteV_ fromEnum 2

getByteV :: (Bits a, Integral a, G.Vector v a) => Int -> v a -> Int -> Word16
getByteV = getByteV_ id
{-# INLINE getByteV #-}

getByteV_ :: (Bits a, Integral a, G.Vector v b) =>
             (b -> a) -> Int -> v b -> Int -> Word16
getByteV_ convert shiftSize = \v n ->
  if n < G.length v `shiftL` shiftSize
  then reindex shiftSize n $ \wordOffset shiftRight ->
       let word       = convert (G.unsafeIndex v wordOffset)
           byteInWord = (word `shiftR` shiftRight) .&. 255
       in fromIntegral byteInWord .|. 256
  else 0
{-# INLINE getByteV_ #-}

reindex :: Int -> Int -> (Int -> Int -> r) -> r
reindex shiftSize n f = f wordOffset shiftRight
  where
    wordOffset = n `shiftR` shiftSize
    shiftRight = (size - (n .&. size)) `shiftL` 3
      where size = (1 `shiftL` shiftSize) - 1
{-# INLINE reindex #-}

-- | /O(n)/. Convert the map to a list of key\/value pairs. The list
-- returned will be sorted in lexicographically ascending order.
--
-- > toList (fromList [("b",3), ("a",5)]) == [("a",5),("b",3)]
-- > toList empty == []
toList :: CritBit k v -> [(k, v)]
toList (CritBit root) = go root []
  where
    go (Internal l r _ _) next = go l (go r next)
    go (Leaf k v) next         = (k,v) : next
    go Empty next              = next


-- | A set based on crit-bit trees.
newtype Set a = Set (CritBit a ())
    deriving (Eq, NFData)
