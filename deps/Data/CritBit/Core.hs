{-# LANGUAGE BangPatterns, RecordWildCards, ScopedTypeVariables #-}
-- |
-- Module      :  Data.CritBit.Core
-- Copyright   :  (c) Bryan O'Sullivan 2013
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- "Core" functions that implement the crit-bit tree algorithms.
--
-- I plopped these functions into their own source file to demonstrate
-- just how small the core of the crit-bit tree concept is.
--
-- I have also commented this module a bit more heavily than I usually
-- do, in the hope that the comments will make the code more
-- approachable to less experienced Haskellers.
module Data.CritBit.Core
    (
    -- * Public functions
      insertWithKey
    , insertLookupWithKey
    , insertLookupGen
    , lookupWith
    , updateLookupWithKey
    , leftmost
    , rightmost
    -- * Internal functions
    , Diff(..)
    , diffOrd
    , followPrefixes
    , followPrefixesFrom
    , followPrefixesByteFrom
    , findPosition
    -- ** Predicates
    , onLeft
    , above
    -- ** Smart constructors
    , setLeft
    , setRight
    , setLeft'
    , setRight'
    , internal
    ) where

import Prelude
import Data.Bits ((.|.), (.&.), complement, shiftR, xor)
import Data.CritBit.Types.Internal

-- | /O(k)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value cb@
-- will insert the pair (key, value) into cb if key does not exist in the map.
-- If the key does exist, the function will insert the pair
-- @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to insertWithKey.
--
-- > let f key new_value old_value = byteCount key + new_value + old_value
-- > insertWithKey f "a" 1 (fromList [("a",5), ("b",3)]) == fromList [("a",7), ("b",3)]
-- > insertWithKey f "c" 1 (fromList [("a",5), ("b",3)]) == fromList [("a",5), ("b",3), ("c",1)]
-- > insertWithKey f "a" 1 empty                         == singleton "a" 1
insertWithKey :: CritBitKey k => (k -> v -> v -> v) -> k -> v -> CritBit k v
              -> CritBit k v
insertWithKey f k v m = insertLookupGen (flip const) f k v m
{-# INLINABLE insertWithKey #-}

-- | /O(k)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = length key + old_value + new_value
-- > insertLookupWithKey f "a" 2 (fromList [("a",5), ("b",3)]) == (Just 5, fromList [("a",8), ("b",3)])
-- > insertLookupWithKey f "c" 2 (fromList [(5,"a"), (3,"b")]) == (Nothing, fromList [("a",5), ("b",3), ("c",2)])
-- > insertLookupWithKey f "a" 2 empty                         == (Nothing, singleton "a" 2)
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup "a" 1 (fromList [("a",5), ("b",3)]) == (Just 5, fromList [("a",1), ("b",3)])
-- > insertLookup "c" 1 (fromList [("a",5), ("b",3)]) == (Nothing,  fromList [("a",5), ("b",3), ("c",1)])
insertLookupWithKey :: CritBitKey k
                    => (k -> v -> v -> v)
                    -> k -> v -> CritBit k v
                    -> (Maybe v, CritBit k v)
insertLookupWithKey f k v m = insertLookupGen (,) f k v m
{-# INLINABLE insertLookupWithKey #-}

-- | General function used to implement all insert functions.
insertLookupGen :: CritBitKey k
                => (Maybe v -> CritBit k v -> a)
                -> (k -> v -> v -> v)
                -> k -> v -> CritBit k v -> a
insertLookupGen ret f !k v m = findPosition ret' finish setLeft setRight k m
  where
    finish _ Empty           = Leaf k v
    finish diff (Leaf _ v')
      | diffOrd diff == EQ   = Leaf k $ f k v v'
    finish diff node         = internal diff node (Leaf k v)

    ret' a b = ret a (CritBit b)
{-# INLINE insertLookupGen #-}

-- | Common part of key finding/insert functions
findPosition :: (CritBitKey k)
             => (Maybe v -> r -> t) -> (Diff -> Node k v -> r)
             -> (Node k v -> r -> r) -> (Node k v -> r -> r)
             -> k -> CritBit k v -> t
findPosition ret finish toLeft toRight k (CritBit root) = go root
  where
    go i@(Internal {..})
      | k `onLeft` i = go ileft
      | otherwise    = go iright
    go (Leaf lk lv)
      | diffOrd diff == EQ = ret (Just lv) $ rewalk root
      | otherwise          = ret Nothing   $ rewalk root
      where
        rewalk i@(Internal left right _ _)
          | diff `above` i = finish diff i
          | k `onLeft` i   = toLeft  i (rewalk left )
          | otherwise      = toRight i (rewalk right)
        rewalk i           = finish diff i

        diff               = followPrefixes k lk
    go Empty = ret Nothing $ finish undefined Empty
{-# INLINE findPosition #-}

data Diff = Diff {-# UNPACK #-} !Int
                 {-# UNPACK #-} !BitMask
                 {-# UNPACK #-} !BitMask

-- | Smart consturctor for Internal nodes
internal :: Diff -> Node k v -> Node k v -> Node k v
internal diff@(Diff byte bits _) child1 child2 = case diffOrd diff of
  LT -> Internal child1 child2 byte bits
  GT -> Internal child2 child1 byte bits
  EQ -> error "Data.CritBit.Cord.internal: Equal."
{-# INLINE internal #-}

setLeft :: Node k v -> Node k v -> Node k v
setLeft i@(Internal{}) node = i { ileft = node }
setLeft _ _ = error "Data.CritBit.Core.setLeft: Non-Internal node"
{-# INLINE setLeft #-}

setRight :: Node k v -> Node k v -> Node k v
setRight i@(Internal{}) node = i { iright = node }
setRight _ _ = error "Data.CritBit.Core.setRight: Non-Internal node"
{-# INLINE setRight #-}

setLeft' :: Node k v -> Node k v -> Node k v
setLeft' i@(Internal{}) Empty = iright i
setLeft' i@(Internal{}) child = i { ileft = child }
setLeft' _ _ = error "Data.CritBit.Core.setLeft': Non-internal node"
{-# INLINE setLeft' #-}

setRight' :: Node k v -> Node k v -> Node k v
setRight' i@(Internal{}) Empty = ileft i
setRight' i@(Internal{}) child = i { iright = child }
setRight' _ _ = error "Data.CritBit.Core.alter.setRight': Non-internal node"
{-# INLINE setRight' #-}

above :: Diff -> Node k v -> Bool
above (Diff dbyte dbits _) (Internal _ _ byte bits) =
    dbyte < byte || dbyte == byte && dbits < bits
above _ _ = error "Data.CritBit.Core.above: Non-Internal node"
{-# INLINE above #-}

lookupWith :: (CritBitKey k) =>
              a                 -- ^ Failure continuation
           -> (v -> a)          -- ^ Success continuation
           -> k
           -> CritBit k v -> a
-- We use continuations here to avoid reimplementing the lookup
-- algorithm with trivial variations.
lookupWith notFound found k (CritBit root) = go root
  where
    go i@(Internal {..})
       | k `onLeft` i = go ileft
       | otherwise    = go iright
    go (Leaf lk v)
       | k == lk      = found v
    go _              = notFound
{-# INLINE lookupWith #-}

-- | /O(k)/. Lookup and update; see also 'updateWithKey'.
-- This function returns the changed value if it is updated, or
-- the original value if the entry is deleted.
--
-- > let f k x = if x == 5 then Just (x + fromEnum (k < "d")) else Nothing
-- > updateLookupWithKey f "a" (fromList [("b",3), ("a",5)]) == (Just 6, fromList [("a", 6), ("b",3)])
-- > updateLookupWithKey f "c" (fromList [("a",5), ("b",3)]) == (Nothing, fromList [("a",5), ("b",3)])
-- > updateLookupWithKey f "b" (fromList [("a",5), ("b",3)]) == (Just 3, singleton "a" 5)
updateLookupWithKey :: (CritBitKey k) => (k -> v -> Maybe v) -> k
                       -> CritBit k v -> (Maybe v, CritBit k v)
-- Once again with the continuations! It's somewhat faster to do
-- things this way than to expicitly unwind our recursion once we've
-- found the leaf to delete. It's also a ton less code.
--
-- (If you want a good little exercise, rewrite this function without
-- using continuations, and benchmark the two versions.)
updateLookupWithKey f k t@(CritBit root) = go root (CritBit Empty) CritBit
  where
    go i@(Internal left right _ _) _ cont = dispatch i left right cont
    go (Leaf lk lv) other cont
      | k == lk   = case f lk lv of
                      Just lv' -> (Just lv', cont $! Leaf lk lv')
                      Nothing  -> (Just lv, other)
      | otherwise = (Nothing, t)
    go Empty _ _  = (Nothing, t)
    {-# INLINE go #-}

    dispatch i left right cont
      | k `onLeft` i = go left (cont right) $ (cont $!) . setLeft'  i
      | otherwise    = go right (cont left) $ (cont $!) . setRight' i
{-# INLINABLE updateLookupWithKey #-}

-- | Determine whether specified key is on the left subtree of the
-- 'Internal' node.
onLeft :: (CritBitKey k) => k -> Node k v -> Bool
onLeft k (Internal _ _ byte bits) =
  (1 + (bits .|. getByte k byte)) `shiftR` 9 == 0
onLeft _ _ = error "Data.CritBit.Core.onLeft: Non-Internal node"
{-# INLINE onLeft #-}

-- | Given a diff of two keys determines result of comparison of them.
diffOrd :: Diff -> Ordering
diffOrd (Diff _ bits c)
  | bits == 0x1ff                      = EQ
  | (1 + (bits .|. c)) `shiftR` 9 == 0 = LT
  | otherwise                          = GT
{-# INLINE diffOrd #-}

-- | Figure out the byte offset at which the key we are interested in
-- differs from the leaf we reached when we initially walked the tree.
--
-- We return some auxiliary stuff that we'll bang on to help us figure
-- out which direction to go in to insert a new node.
followPrefixes :: (CritBitKey k) =>
                  k             -- ^ The key from "outside" the tree.
               -> k             -- ^ Key from the leaf we reached.
               -> Diff
followPrefixes = followPrefixesFrom 0
{-# INLINE followPrefixes #-}

-- | Figure out the offset of the first different byte in two keys,
-- starting from specified position.
--
-- We return some auxiliary stuff that we'll bang on to help us figure
-- out which direction to go in to insert a new node.
followPrefixesFrom :: (CritBitKey k) =>
                      Int           -- ^ Positition to start from
                   -> k             -- ^ First key.
                   -> k             -- ^ Second key.
                   -> Diff
followPrefixesFrom !position !k !l = Diff n (maskLowerBits (b `xor` c)) c
  where
    n = followPrefixesByteFrom position k l
    b = getByte k n
    c = getByte l n

    maskLowerBits v = (n3 .&. complement (n3 `shiftR` 1)) `xor` 0x1FF
      where
        n3 = n2 .|. (n2 `shiftR` 8)
        n2 = n1 .|. (n1 `shiftR` 4)
        n1 = n0 .|. (n0 `shiftR` 2)
        n0 = v  .|. (v  `shiftR` 1)
{-# INLINE followPrefixesFrom #-}

-- | Figure out the offset of the first different byte in two keys,
-- starting from specified position.
followPrefixesByteFrom :: (CritBitKey k) =>
                          Int           -- ^ Positition to start from
                       -> k             -- ^ First key.
                       -> k             -- ^ Second key.
                       -> Int
followPrefixesByteFrom !position !k !l = go position
  where
    go !n | b /= c || b == 0 || c == 0 = n
          | otherwise                  = go (n + 1)
      where b = getByte k n
            c = getByte l n
{-# INLINE followPrefixesByteFrom #-}

leftmost, rightmost :: a -> (k -> v -> a) -> Node k v -> a
leftmost  = extremity ileft
{-# INLINE leftmost #-}
rightmost = extremity iright
{-# INLINE rightmost #-}

-- | Generic function so we can easily implement 'leftmost' and 'rightmost'.
extremity :: (Node k v -> Node k v) -- ^ Either 'ileft' or 'iright'.
          -> a                      -- ^ 'Empty' continuation.
          -> (k -> v -> a)          -- ^ 'Leaf' continuation.
          -> Node k v
          -> a
extremity direct onEmpty onLeaf node = go node
  where
    go i@(Internal{}) = go $ direct i
    go (Leaf k v)     = onLeaf k v
    go _              = onEmpty
    {-# INLINE go #-}
{-# INLINE extremity #-}
