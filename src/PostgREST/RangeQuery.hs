module PostgREST.RangeQuery (
  rangeParse
, rangeRequested
, rangeLimit
, rangeOffset
, restrictRange
, rangeGeq
, NonnegRange
, limitToRange
, toRange
) where


import           Control.Applicative
import           Network.HTTP.Types.Header
import           Data.Monoid               ((<>))

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
          upper         = fromMaybe (rangeGeq 0) (rangeLeq <$> to) in
      rangeIntersection lower upper
    Nothing -> rangeGeq 0

rangeRequested :: RequestHeaders -> Maybe NonnegRange
rangeRequested headers = rangeParse <$> lookup hRange headers

restrictRange :: Maybe Integer -> Maybe NonnegRange -> Maybe NonnegRange
restrictRange Nothing r = r
restrictRange (Just limit) Nothing = Just $ rangeIntersection (rangeGeq 0) (rangeLeq (limit - 1))
restrictRange (Just limit) (Just r) = Just $
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

rangeLeq :: Integer -> NonnegRange
rangeLeq n =
  Range BoundaryBelowAll (BoundaryAbove n)

limitToRange :: BS.ByteString -> NonnegRange
limitToRange l = rangeParse ("0-" <> cs (show (l' - 1)))
  where l' = fromMaybe 0 (readMaybe $ cs l)::Integer

toRange :: Maybe String -> Maybe String -> Maybe NonnegRange
toRange Nothing Nothing = Nothing
toRange Nothing (Just o) = Just $ rangeParse $ cs $ show o' <> "-"
  where o' = fromMaybe 0 (readMaybe $ cs o)::Integer
toRange (Just l) Nothing = Just $ limitToRange $ cs l
toRange (Just l) (Just o) = Just $ rangeParse $ cs $ show o' <> "-" <> show (o' + l' - 1)
  where
    l' = fromMaybe 0 (readMaybe $ cs l)::Integer
    o' = fromMaybe 0 (readMaybe $ cs o)::Integer
