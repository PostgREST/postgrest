{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Either (
  maybeToLeft
, maybeToRight
, leftToMaybe
, rightToMaybe
, maybeEmpty
, maybeToEither
, fromLeft
, fromRight
) where

import Data.Function (const)
import Data.Monoid (Monoid, mempty)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Either (Either(Left, Right), either)
#if MIN_VERSION_base(4,10,0)
import Data.Either (fromLeft, fromRight)
#else
-- | Return the contents of a 'Right'-value or a default value otherwise.
fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

-- | Return the contents of a 'Right'-value or a default value otherwise.
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b
#endif

leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeEmpty :: Monoid b => (a -> b) -> Maybe a -> b
maybeEmpty = maybe mempty

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a
