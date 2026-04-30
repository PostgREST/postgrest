{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Safe (
    headMay
  , headDef
  , initMay
  , initDef
  , initSafe
  , tailMay
  , tailDef
  , tailSafe
  , lastDef
  , lastMay
  , foldr1May
  , foldl1May
  , foldl1May'
  , maximumMay
  , minimumMay
  , maximumDef
  , minimumDef
  , atMay
  , atDef
) where


import Data.Ord (Ord, (<))
import Data.Int (Int)
import Data.Char (Char)
import Data.Bool (Bool, otherwise)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Either (Either(Left, Right))
import Data.Function ((.))
import Data.List (null, head, last, tail, init, maximum, minimum, foldr1, foldl1, foldl1', (++))

import Prelude ((-))
import GHC.Show (show)

liftMay :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
liftMay test f val = if test val then Nothing else Just (f val)

-------------------------------------------------------------------------------
-- Head
-------------------------------------------------------------------------------

headMay :: [a] -> Maybe a
headMay = liftMay null head

headDef :: a -> [a] -> a
headDef def = fromMaybe def . headMay

-------------------------------------------------------------------------------
-- Init
-------------------------------------------------------------------------------

initMay :: [a] -> Maybe [a]
initMay = liftMay null init

initDef :: [a] -> [a] -> [a]
initDef def = fromMaybe def . initMay

initSafe :: [a] -> [a]
initSafe = initDef []

-------------------------------------------------------------------------------
-- Tail
-------------------------------------------------------------------------------

tailMay :: [a] -> Maybe [a]
tailMay = liftMay null tail

tailDef :: [a] -> [a] -> [a]
tailDef def = fromMaybe def . tailMay

tailSafe :: [a] -> [a]
tailSafe = tailDef []

-------------------------------------------------------------------------------
-- Last
-------------------------------------------------------------------------------

lastMay :: [a] -> Maybe a
lastMay = liftMay null last

lastDef :: a -> [a] -> a
lastDef def = fromMaybe def . lastMay

-------------------------------------------------------------------------------
-- Maximum
-------------------------------------------------------------------------------

minimumMay, maximumMay :: Ord a => [a] -> Maybe a
minimumMay = liftMay null minimum
maximumMay = liftMay null maximum

minimumDef, maximumDef :: Ord a => a -> [a] -> a
minimumDef def = fromMaybe def . minimumMay
maximumDef def = fromMaybe def . maximumMay

-------------------------------------------------------------------------------
-- Foldr
-------------------------------------------------------------------------------

foldr1May, foldl1May, foldl1May' :: (a -> a -> a) -> [a] -> Maybe a
foldr1May = liftMay null . foldr1

-------------------------------------------------------------------------------
-- Foldl
-------------------------------------------------------------------------------

foldl1May = liftMay null . foldl1
foldl1May' = liftMay null . foldl1'

-------------------------------------------------------------------------------
-- At
-------------------------------------------------------------------------------

at_ :: [a] -> Int -> Either [Char] a
at_ ys o
  | o < 0 = Left ("index must not be negative, index=" ++ show o)
  | otherwise = f o ys
  where
    f 0 (x:_) = Right x
    f i (_:xs) = f (i-1) xs
    f i [] = Left ("index too large, index=" ++ show o ++ ", length=" ++ show (o-i))

atMay :: [a] -> Int -> Maybe a
atMay xs i = case xs `at_` i of
  Left _  -> Nothing
  Right val -> Just val

atDef :: a -> [a] -> Int -> a
atDef def xs i = case xs `at_` i of
  Left _  -> def
  Right val -> val
