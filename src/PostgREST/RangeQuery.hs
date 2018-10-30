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

import           Text.Regex.TDFA           ((=~))

import Data.List (lookup)

import           Protolude

type NonnegRange = Range Integer

rangeParse :: BS.ByteString -> NonnegRange
rangeParse range = do
  let rangeRegex = "^([0-9]+)-([0-9]*)$" :: BS.ByteString

  case listToMaybe (range =~ rangeRegex :: [[BS.ByteString]]) of
    Just parsedRange ->
      let [_, mLower, mUpper] = readMaybe . toS <$> parsedRange
          lower         = maybe emptyRange rangeGeq mLower
          upper         = maybe allRange rangeLeq mUpper in
      rangeIntersection lower upper
    Nothing -> allRange

rangeRequested :: RequestHeaders -> NonnegRange
rangeRequested headers = maybe allRange rangeParse $ lookup hRange headers

restrictRange :: Maybe Integer -> NonnegRange -> NonnegRange
restrictRange Nothing r = r
restrictRange (Just limit) r =
   rangeIntersection r $
     Range BoundaryBelowAll (BoundaryAbove $ rangeOffset r + limit - 1)

rangeLimit :: NonnegRange -> Maybe Integer
rangeLimit range =
  case [rangeLower range, rangeUpper range] of
    [BoundaryBelow lower, BoundaryAbove upper] -> Just (1 + upper - lower)
    _ -> Nothing

rangeOffset :: NonnegRange -> Integer
rangeOffset range =
  case rangeLower range of
    BoundaryBelow lower -> lower
    _ -> panic "range without lower bound" -- should never happen

rangeGeq :: Integer -> NonnegRange
rangeGeq n =
  Range (BoundaryBelow n) BoundaryAboveAll

allRange :: NonnegRange
allRange = rangeGeq 0

rangeLeq :: Integer -> NonnegRange
rangeLeq n =
  Range BoundaryBelowAll (BoundaryAbove n)
