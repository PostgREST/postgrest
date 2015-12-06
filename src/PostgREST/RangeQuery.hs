module PostgREST.RangeQuery (
  rangeParse
, rangeRequested
, rangeLimit
, rangeOffset
, restrictRange
, NonnegRange
) where


import           Control.Applicative
import           Network.HTTP.Types.Header
import           PostgREST.Types           ()

import qualified Data.ByteString.Char8     as BS
import           Data.Ranged.Boundaries
import           Data.Ranged.Ranges

import           Data.String.Conversions   (cs)
import           Text.Read                 (readMaybe)
import           Text.Regex.TDFA           ((=~))

import           Data.Maybe                (fromMaybe, listToMaybe)

import           Prelude

type NonnegRange = Range Int

rangeParse :: BS.ByteString -> NonnegRange
rangeParse range = do
  let rangeRegex = "^([0-9]+)-([0-9]*)$" :: BS.ByteString

  case listToMaybe (range =~ rangeRegex :: [[BS.ByteString]]) of
    Just parsedRange ->
      let [_, from, to] = readMaybe . cs <$> parsedRange
          lower         = fromMaybe emptyRange   (rangeGeq <$> from)
          upper         = fromMaybe (rangeGeq 0) (rangeLeq <$> to) in
      rangeIntersection lower upper
    Nothing -> rangeGeq 0

rangeRequested :: RequestHeaders -> NonnegRange
rangeRequested = rangeParse . fromMaybe "" . lookup hRange

restrictRange :: Maybe Int -> NonnegRange -> NonnegRange
restrictRange Nothing r = r
restrictRange (Just limit) r =
  rangeIntersection r $
    Range BoundaryBelowAll (BoundaryAbove $ rangeOffset r + limit - 1)

rangeLimit :: NonnegRange -> Maybe Int
rangeLimit range =
  case [rangeLower range, rangeUpper range] of
    [BoundaryBelow from, BoundaryAbove to] -> Just (1 + to - from)
    _ -> Nothing

rangeOffset :: NonnegRange -> Int
rangeOffset range =
  case rangeLower range of
    BoundaryBelow from -> from
    _ -> error "range without lower bound" -- should never happen

rangeGeq :: Int -> NonnegRange
rangeGeq n =
  Range (BoundaryBelow n) BoundaryAboveAll

rangeLeq :: Int -> NonnegRange
rangeLeq n =
  Range BoundaryBelowAll (BoundaryAbove n)
