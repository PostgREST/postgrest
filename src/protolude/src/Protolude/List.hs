{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe #-}

module Protolude.List
  ( head,
    ordNub,
    sortOn,
    list,
    product,
    sum,
    groupBy,
  )
where

import Control.Applicative (pure)
import Data.Foldable (Foldable, foldl', foldr)
import Data.Function ((.))
import Data.Functor (fmap)
import Data.List (groupBy, sortBy)
import Data.Maybe (Maybe (Nothing))
import Data.Ord (Ord, comparing)
import qualified Data.Set as Set
import Prelude ((*), (+), Num)

head :: (Foldable f) => f a -> Maybe a
head = foldr (\x _ -> pure x) Nothing

sortOn :: (Ord o) => (a -> o) -> [a] -> [a]
sortOn = sortBy . comparing

-- O(n * log n)
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x : xs) =
      if x `Set.member` s
        then go s xs
        else x : go (Set.insert x s) xs

list :: [b] -> (a -> b) -> [a] -> [b]
list def f xs = case xs of
  [] -> def
  _ -> fmap f xs

{-# INLINE product #-}
product :: (Foldable f, Num a) => f a -> a
product = foldl' (*) 1

{-# INLINE sum #-}
sum :: (Foldable f, Num a) => f a -> a
sum = foldl' (+) 0
