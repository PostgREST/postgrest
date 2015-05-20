module PostgREST.RangeQuery (
  rangeParse
, rangeRequested
, rangeLimit
, rangeOffset
, NonnegRange
) where

import Control.Applicative
import Network.HTTP.Types.Header

import qualified Data.ByteString.Char8 as BS

import Data.Ranged.Boundaries
import Data.Ranged.Ranges

import Data.String.Conversions (cs)
import Text.Regex.TDFA ((=~))
import Text.Read (readMaybe)

import Data.Maybe (fromMaybe, listToMaybe)

import Prelude

type NonnegRange = Range Int

rangeParse :: BS.ByteString -> Maybe NonnegRange
rangeParse range = do
  let rangeRegex = "^([0-9]+)-([0-9]*)$" :: BS.ByteString

  parsedRange <- listToMaybe (range =~ rangeRegex :: [[BS.ByteString]])

  let [_, from, to] = readMaybe . cs <$> parsedRange
  let lower = fromMaybe emptyRange   (rangeGeq <$> from)
  let upper = fromMaybe (rangeGeq 0) (rangeLeq <$> to)

  return $ rangeIntersection lower upper

rangeRequested :: RequestHeaders -> Maybe NonnegRange
rangeRequested = (rangeParse =<<) . lookup hRange

rangeLimit :: NonnegRange -> Maybe Int
rangeLimit range =
  case [rangeLower range, rangeUpper range]
    of [BoundaryBelow from, BoundaryAbove to] -> Just (1 + to - from)
       _ -> Nothing

rangeOffset :: NonnegRange -> Int
rangeOffset range =
  case rangeLower range
    of BoundaryBelow from -> from
       _ -> error "range without lower bound" -- should never happen

rangeGeq :: Int -> NonnegRange
rangeGeq n =
  Range (BoundaryBelow n) BoundaryAboveAll

rangeLeq :: Int -> NonnegRange
rangeLeq n =
  Range BoundaryBelowAll (BoundaryAbove n)
