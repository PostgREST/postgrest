module PostgREST.RangeQuery (
  rangeParse
, rangeRequested
, rangeLimit
, rangeOffset
, restrictRange
, NonnegRange
) where


import           Control.Monad             (join)
import           Control.Applicative
import           Network.HTTP.Types.Header
import           PostgREST.Types           ()

import qualified Data.ByteString.Char8     as BS
import           Data.Ranged.Boundaries
import           Data.Ranged.Ranges
import           Data.Maybe                (fromMaybe, listToMaybe, isJust, fromJust)
import           Data.String.Conversions   (cs)
import           Text.Read                 (readMaybe)
import           Text.Regex.TDFA           ((=~))

import           Prelude

type NonnegRange = Range Int
type ByteString = BS.ByteString
type RequestQuery = [(String, Maybe String)]

rangeRequested :: RequestHeaders -> RequestQuery -> NonnegRange
rangeRequested headers query = fromMaybe (rangeFromQuery Nothing query) (rangeFromHeaders headers)

rangeParse :: ByteString -> Maybe NonnegRange
rangeParse range = do
  let rangeRegex = "^([0-9]+)-([0-9]*)$" :: ByteString

  case listToMaybe (range =~ rangeRegex :: [[ByteString]]) of
    Just parsedRange ->
      let [_, from, to] = readMaybe . cs <$> parsedRange
          lower         = fromMaybe emptyRange   (rangeGeq <$> from)
          upper         = fromMaybe (rangeGeq 0) (rangeLeq <$> to) in
      Just $ rangeIntersection lower upper
    Nothing -> Nothing

rangeFromHeaders :: RequestHeaders -> Maybe NonnegRange
rangeFromHeaders headers = join $ rangeParse <$> lookup hRange headers

rangeFromQuery :: Maybe String -> RequestQuery -> NonnegRange
rangeFromQuery name query =
  if isJust limit
    then Range (BoundaryBelow offset) (BoundaryAbove (offset + fromJust limit - 1))
    else rangeGeq offset
  where
    intParam :: String -> Maybe Int
    intParam w = join $ readMaybe <$> join (lookup qw query)
      where qw = cs $ if isJust name then fromJust name ++ "." ++ w else w
    page = fromMaybe 0 $ intParam "page"
    limit = intParam "limit"
    offset = fromMaybe (maybe 0 (* page) limit) $ intParam "offset"

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
