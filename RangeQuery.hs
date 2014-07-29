{-# LANGUAGE OverloadedStrings #-}

module RangeQuery where

import Control.Applicative
import Network.HTTP.Types.Header

import Data.Ranged.Boundaries
import Data.Ranged.Ranges

import qualified Data.ByteString.Char8 as BS
import Text.Regex.TDFA ((=~))
import Text.Read (readMaybe)

import Data.Maybe (fromMaybe, listToMaybe)

rangeGeq :: Int -> Range Int
rangeGeq n =
  head $ rangeUnion (singletonRange n) $ Range (BoundaryAbove n) BoundaryAboveAll

rangeLeq :: Int -> Range Int
rangeLeq n =
  head $ rangeUnion (singletonRange n) $ Range BoundaryBelowAll (BoundaryBelow n)

parseRange :: String -> Maybe(Range Int)
parseRange range = do
  let rangeRegex = "^([0-9]+)-([0-9]*)$" :: String

  parsedRange <- listToMaybe (range =~ rangeRegex :: [[String]])

  let [_, from, to] = readMaybe <$> parsedRange
  let lower = fromMaybe emptyRange   (rangeGeq <$> from)
  let upper = fromMaybe (rangeGeq 0) (rangeLeq <$> to)

  return $ rangeIntersection lower upper

requestedRange :: RequestHeaders -> Maybe(Range Int)
requestedRange hdrs = parseRange =<< BS.unpack <$> lookup hRange hdrs
