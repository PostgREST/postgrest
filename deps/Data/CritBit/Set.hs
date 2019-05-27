{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Data.CritBit.Set
-- Copyright   :  (c) Bryan O'Sullivan and others 2013-2014
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- A set type that uses crit-bit trees internally.
--
-- For every /n/ key-value pairs stored, a crit-bit tree uses /n/-1
-- internal nodes, for a total of 2/n/-1 internal nodes and leaves.
module Data.CritBit.Set
    (
    -- * Set type
    Set

    -- * Operators
    , (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE
    , isSubsetOf
    , isProperSubsetOf

    -- * Construction
    , empty
    , singleton
    , insert
    , delete

    -- * Combine
    , union
    , unions
    , difference
    , intersection

    -- * Filter
    , filter
    , partition
    , split
    , splitMember

    -- * Map
    , map
    , mapMonotonic

    -- * Folds
    , foldr
    , foldl
    -- ** Strict folds
    , foldr'
    , foldl'

    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , maxView
    , minView

    -- * Conversion

    -- ** List
    , elems
    , toList
    , fromList

    -- ** Ordered list
    , toAscList
    , toDescList
    , fromAscList
    , fromDistinctAscList
    ) where

import Control.Arrow ((***))
import Data.CritBit.Types.Internal (CritBit(..), Set(..), CritBitKey, Node(..))
import Data.Foldable (Foldable, foldMap)
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..))
import Prelude hiding (null, filter, map, foldl, foldr)
import qualified Data.CritBit.Tree as T
import qualified Data.List as List

instance (Show a) => Show (Set a) where
    show s = "fromList " ++ show (toList s)

instance CritBitKey k => Monoid (Set k) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance Foldable Set where
    foldMap f (Set (CritBit n)) = foldSet f n

foldSet :: (Monoid m) => (a -> m) -> Node a () -> m
foldSet f (Internal l r _ _) = mappend (foldSet f l) (foldSet f r)
foldSet f (Leaf k _)         = f k
foldSet _ Empty              = mempty
{-# INLINABLE foldSet #-}

-- | Same as 'difference'.
(\\) :: CritBitKey a => Set a -> Set a -> Set a
s \\ p = difference s p
{-# INLINABLE (\\) #-}

-- | /O(1)/. Is the set empty?
--
-- > null (empty)         == True
-- > null (singleton "a") == False
null :: Set a -> Bool
null (Set a) = T.null a

-- | /O(1)/. The empty set.
--
-- > empty      == fromList []
-- > size empty == 0
empty :: Set a
empty = Set T.empty
{-# INLINABLE empty #-}

-- | /O(1)/. A set with a single element.
--
-- > singleton "a"        == fromList ["a"]
singleton :: a -> Set a
singleton a = Set $ T.singleton a ()
{-# INLINE singleton #-}

-- | /O(k)/. Build a set from a list of values.
--
-- > fromList [] == empty
-- > fromList ["a", "b", "a"] == fromList ["a", "b"]
fromList :: (CritBitKey a) => [a] -> Set a
fromList = liftFromList T.fromList
{-# INLINABLE fromList #-}

-- | /O(n)/. An alias of 'toList'.
--
-- Returns the elements of a set in ascending order.
elems :: Set a -> [a]
elems = toList

-- | /O(n)/. Convert the set to a list of values. The list returned
-- will be sorted in lexicographically ascending order.
--
-- > toList (fromList ["b", "a"]) == ["a", "b"]
-- > toList empty == []
toList :: Set a -> [a]
toList = wrapS id T.keys
{-# INLINABLE toList #-}

-- | /O(n)/. The number of elements in the set.
--
-- > size empty                      == 0
-- > size (singleton "a")            == 1
-- > size (fromList ["a", "c", "b"]) == 3
size :: Set a -> Int
size = wrapS id T.size
{-# INLINABLE size #-}

-- | /O(k)/. Is the element in the set?
--
-- > member "a" (fromList ["a", "b"]) == True
-- > member "c" (fromList ["a", "b"]) == False
--
-- See also 'notMember'.
member :: (CritBitKey a) => a -> Set a -> Bool
member a (Set s) = T.member a s
{-# INLINABLE member #-}

-- | /O(k)/. Is the element not in the set?
--
-- > notMember "a" (fromList ["a", "b"]) == False
-- > notMember "c" (fromList ["a", "b"]) == True
--
-- See also 'member'.
notMember :: (CritBitKey a) => a -> Set a -> Bool
notMember a (Set s) = T.notMember a s
{-# INLINABLE notMember #-}

-- | /O(k)/. Find largest element smaller than the given one.
--
-- > lookupLT "b"  (fromList ["a", "b"]) == Just "a"
-- > lookupLT "aa" (fromList ["a", "b"]) == Just "a"
-- > lookupLT "a"  (fromList ["a", "b"]) == Nothing
lookupLT :: (CritBitKey a) => a -> Set a -> Maybe a
lookupLT = wrapVS (fmap fst) T.lookupLT
{-# INLINABLE lookupLT #-}

-- | /O(k)/. Find smallest element greater than the given one.
--
-- > lookupGT "b"  (fromList ["a", "b"]) == Nothing
-- > lookupGT "aa" (fromList ["a", "b"]) == Just "b"
-- > lookupGT "a"  (fromList ["a", "b"]) == Just "b"
lookupGT :: (CritBitKey a) => a -> Set a -> Maybe a
lookupGT = wrapVS (fmap fst) T.lookupGT
{-# INLINABLE lookupGT #-}

-- | /O(k)/. Find largest element smaller than or equal to the given one.
--
-- > lookupGE "b"  (fromList ["a", "b"]) == Just "b"
-- > lookupGE "aa" (fromList ["a", "b"]) == Just "b"
-- > lookupGE "a"  (fromList ["a", "b"]) == Just "a"
-- > lookupGE ""   (fromList ["a", "b"]) == Nothing
lookupLE :: (CritBitKey a) => a -> Set a -> Maybe a
lookupLE = wrapVS (fmap fst) T.lookupLE
{-# INLINABLE lookupLE #-}

-- | /O(k)/. Find smallest element greater than or equal to the given one.
--
-- > lookupGE "aa" (fromList ["a", "b"]) == Just "b"
-- > lookupGE "b"  (fromList ["a", "b"]) == Just "b"
-- > lookupGE "bb" (fromList ["a", "b"]) == Nothing
lookupGE :: (CritBitKey a) => a -> Set a -> Maybe a
lookupGE = wrapVS (fmap fst) T.lookupGE
{-# INLINABLE lookupGE #-}

-- | /O(n+m)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: (CritBitKey a) => Set a -> Set a -> Bool
isSubsetOf = wrapSS id T.isSubmapOf
{-# INLINABLE isSubsetOf #-}

-- | /O(n+m)/. Is this a proper subset (ie. a subset but not equal)?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a proper subset of @s2@.
isProperSubsetOf :: (CritBitKey a) => Set a -> Set a -> Bool
isProperSubsetOf = wrapSS id T.isProperSubmapOf
{-# INLINABLE isProperSubsetOf #-}

-- | /O(k)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
insert :: (CritBitKey a) => a -> Set a -> Set a
insert = wrapVS Set (`T.insert` ())
{-# INLINABLE insert #-}

-- | /O(k)/. Delete an element from a set.
delete :: (CritBitKey a) => a -> Set a -> Set a
delete = wrapVS Set T.delete
{-# INLINABLE delete #-}

-- | /O(k)/. The union of two sets, preferring the first set when
-- equal elements are encountered.
union :: (CritBitKey a) => Set a -> Set a -> Set a
union = wrapSS Set T.union
{-# INLINABLE union #-}

-- | The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: (CritBitKey a) => [Set a] -> Set a
unions = List.foldl' union empty
{-# INLINABLE unions #-}

-- | /O(k)/. The difference of two sets.
difference :: (CritBitKey a) => Set a -> Set a -> Set a
difference = wrapSS Set T.difference
{-# INLINABLE difference #-}

-- | /O(k)/. The intersection of two sets. Elements of the
-- result come from the first set.
intersection :: (CritBitKey a) => Set a -> Set a -> Set a
intersection = wrapSS Set T.intersection
{-# INLINABLE intersection #-}

-- | /O(n)/. Filter all elements that satisfy the predicate.
--
-- > filter (> "a") (fromList ["a", "b"]) == fromList [("3","b")]
-- > filter (> "x") (fromList ["a", "b"]) == empty
-- > filter (< "a") (fromList ["a", "b"]) == empty
filter :: (a -> Bool) -> Set a -> Set a
filter = wrapVS Set (T.filterWithKey . (const .))
{-# INLINABLE filter #-}

-- | /O(n)/. Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: (CritBitKey a) => (a -> Bool) -> Set a -> (Set a, Set a)
partition = wrapVS (Set *** Set) (T.partitionWithKey . (const .))
{-# INLINABLE partition #-}

-- | /O(k)/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split "a" (fromList ["b", "d"]) == (empty, fromList ["b", "d")])
-- > split "b" (fromList ["b", "d"]) == (empty, singleton "d")
-- > split "c" (fromList ["b", "d"]) == (singleton "b", singleton "d")
-- > split "d" (fromList ["b", "d"]) == (singleton "b", empty)
-- > split "e" (fromList ["b", "d"]) == (fromList ["b", "d"], empty)
split :: (CritBitKey a) => a -> Set a -> (Set a, Set a)
split = wrapVS (Set *** Set) T.split
{-# INLINABLE split #-}

-- | /O(k)/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
--
-- > splitMember "a" (fromList ["b", "d"]) == (empty, False, fromList ["b", "d"])
-- > splitMember "b" (fromList ["b", "d"]) == (empty, True, singleton "d")
-- > splitMember "c" (fromList ["b", "d"]) == (singleton "b", False, singleton "d")
-- > splitMember "d" (fromList ["b", "d"]) == (singleton "b", True, empty)
-- > splitMember "e" (fromList ["b", "d"]) == (fromList ["b", "d"], False, empty)
splitMember :: (CritBitKey a) => a -> Set a -> (Set a, Bool, Set a)
splitMember = wrapVS pack T.splitLookup
  where pack (l, m, r) = (Set l, isJust m, Set r)
{-# INLINABLE splitMember #-}

-- | /O(k)/. @'map' f s@ is the set obtained by applying @f@ to each
-- element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: (CritBitKey a2) => (a1 -> a2) -> Set a1 -> Set a2
map = wrapVS Set T.mapKeys
{-# INLINABLE map #-}

-- | /O(n)/. The @'mapMonotonic' f s == 'map' f s@, but works only when
-- @f@ is monotonic.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s
mapMonotonic :: (CritBitKey a2) => (a1 -> a2) -> Set a1 -> Set a2
mapMonotonic = wrapVS Set T.mapKeysMonotonic
{-# INLINABLE mapMonotonic #-}

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
foldl :: (a -> b -> a) -> a -> Set b -> a
foldl f = wrapVS id (T.foldlWithKey ((const .) . f))
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> Set b -> a
foldl' f = wrapVS id (T.foldlWithKey' ((const .) . f))
{-# INLINE foldl' #-}

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
foldr :: (a -> b -> b) -> b -> Set a -> b
foldr f = wrapVS id (T.foldrWithKey (const . f))
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' f = wrapVS id (T.foldrWithKey' (const . f))
{-# INLINE foldr' #-}

-- | /O(k')/. The minimal element of a set.
findMin :: Set a -> a
findMin = wrapS fst T.findMin
{-# INLINE findMin #-}

-- | /O(k)/. The maximal element of a set.
findMax :: Set a -> a
findMax = wrapS fst T.findMax
{-# INLINE findMax #-}

-- | /O(k')/. Delete the minimal element. Returns an empty set if the
-- set is empty.
deleteMin :: Set a -> Set a
deleteMin = wrapS Set T.deleteMin
{-# INLINE deleteMin #-}

-- | /O(k)/. Delete the maximal element. Returns an empty set if the
-- set is empty.
deleteMax :: Set a -> Set a
deleteMax = wrapS Set T.deleteMax
{-# INLINE deleteMax #-}

-- | /O(k')/. Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: Set a -> (a, Set a)
deleteFindMin = wrapS (fst *** Set) T.deleteFindMin
{-# INLINE deleteFindMin #-}

-- | /O(k)/. Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: Set a -> (a, Set a)
deleteFindMax = wrapS (fst *** Set) T.deleteFindMax
{-# INLINE deleteFindMax #-}

-- | /O(k')/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: Set a -> Maybe (a, Set a)
minView = wrapS (fmap (fst *** Set)) T.minViewWithKey
{-# INLINE minView #-}

-- | /O(k)/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: Set a -> Maybe (a, Set a)
maxView = wrapS (fmap (fst *** Set)) T.maxViewWithKey
{-# INLINE maxView #-}

-- | /O(n)/. Convert the set to an ascending list of elements.
toAscList :: Set a -> [a]
toAscList = toList

-- | /O(n)/. Convert the set to a descending list of elements.
toDescList :: Set a -> [a]
toDescList = reverse . toAscList

-- | /O(n)/. Build a set from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: (CritBitKey a) => [a] -> Set a
fromAscList = liftFromList T.fromAscList

-- | /O(n)/. Build a set from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromDistinctAscList :: (CritBitKey a) => [a] -> Set a
fromDistinctAscList = liftFromList T.fromDistinctAscList

-- | Wraps tree operation to set operation
wrapS :: (r -> q) -> (CritBit a () -> r) -> Set a -> q
wrapS f g (Set s) = f $ g s
{-# INLINE wrapS #-}

-- | Wraps (value, tree) operation to (value, set) operation
wrapVS :: (r -> q) -> (t -> CritBit a () -> r) -> t -> Set a -> q
wrapVS f g a (Set s) = f $ g a s
{-# INLINE wrapVS #-}

-- | Wraps (tree, tree) operation to (set, set) operation
wrapSS :: (r -> q) -> (CritBit a () -> CritBit a () -> r) -> Set a -> Set a -> q
wrapSS f g (Set s1) (Set s2) = f $ g s1 s2
{-# INLINE wrapSS #-}

liftFromList :: ([(a, ())] -> CritBit a ()) -> [a] -> Set a
liftFromList f xs = Set . f . zip xs . repeat $ ()
{-# INLINE liftFromList #-}
