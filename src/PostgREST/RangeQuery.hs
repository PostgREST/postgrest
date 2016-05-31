module PostgREST.RangeQuery (
  rangeParse
, rangeRequested
, rangeLimit
, rangeOffset
, restrictRange
, rangeGeq
, allRange
, NonnegRange
) where


import           Control.Applicative
import           Network.HTTP.Types.Header

import qualified Data.ByteString.Char8     as BS
import           Data.Ranged.Boundaries
import           Data.Ranged.Ranges

import           Data.String.Conversions   (cs)
import           Text.Read                 (readMaybe)
import           Text.Regex.TDFA           ((=~))

import           Data.Maybe                (fromMaybe, listToMaybe)

import           Prelude

type NonnegRange = Range Integer

rangeParse :: BS.ByteString -> NonnegRange
rangeParse range = do
  let rangeRegex = "^([0-9]+)-([0-9]*)$" :: BS.ByteString

  case listToMaybe (range =~ rangeRegex :: [[BS.ByteString]]) of
    Just parsedRange ->
      let [_, from, to] = readMaybe . cs <$> parsedRange
          lower         = fromMaybe emptyRange   (rangeGeq <$> from)
          upper         = fromMaybe allRange (rangeLeq <$> to) in
      rangeIntersection lower upper
    Nothing -> allRange

rangeRequested :: RequestHeaders -> NonnegRange
rangeRequested headers = fromMaybe allRange $
  rangeParse <$> lookup hRange headers

restrictRange :: Maybe Integer -> NonnegRange -> NonnegRange
restrictRange Nothing r = r
restrictRange (Just limit) r =
   rangeIntersection r $
     Range BoundaryBelowAll (BoundaryAbove $ rangeOffset r + limit - 1)

rangeLimit :: NonnegRange -> Maybe Integer
rangeLimit range =
  case [rangeLower range, rangeUpper range] of
    [BoundaryBelow from, BoundaryAbove to] -> Just (1 + to - from)
    _ -> Nothing

rangeOffset :: NonnegRange -> Integer
rangeOffset range =
  case rangeLower range of
    BoundaryBelow from -> from
    _ -> error "range without lower bound" -- should never happen

rangeGeq :: Integer -> NonnegRange
rangeGeq n =
  Range (BoundaryBelow n) BoundaryAboveAll

allRange :: NonnegRange
allRange = rangeGeq 0

rangeLeq :: Integer -> NonnegRange
rangeLeq n =
  Range BoundaryBelowAll (BoundaryAbove n)
