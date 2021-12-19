{-|
Module      : PostgREST.RangeQuery
Description : Logic regarding the `Range`/`Content-Range` headers and `limit`/`offset` querystring arguments.
-}
module PostgREST.RangeQuery (
  rangeParse
, rangeRequested
, rangeLimit
, rangeOffset
, restrictRange
, rangeGeq
, allRange
, NonnegRange
, rangeStatusHeader
, contentRangeH
) where

import qualified Data.ByteString.Char8 as BS

import Data.List       (lookup)
import Text.Regex.TDFA ((=~))

import Control.Applicative
import Data.Ranged.Boundaries
import Data.Ranged.Ranges
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

import Protolude

type NonnegRange = Range Integer

rangeParse :: BS.ByteString -> NonnegRange
rangeParse range = do
  let rangeRegex = "^([0-9]+)-([0-9]*)$" :: BS.ByteString

  case listToMaybe (range =~ rangeRegex :: [[BS.ByteString]]) of
    Just parsedRange ->
      let [_, mLower, mUpper] = readMaybe . BS.unpack <$> parsedRange
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
    _                   -> panic "range without lower bound" -- should never happen

rangeGeq :: Integer -> NonnegRange
rangeGeq n =
  Range (BoundaryBelow n) BoundaryAboveAll

allRange :: NonnegRange
allRange = rangeGeq 0

rangeLeq :: Integer -> NonnegRange
rangeLeq n =
  Range BoundaryBelowAll (BoundaryAbove n)

rangeStatusHeader :: NonnegRange -> Int64 -> Maybe Int64 -> (Status, Header)
rangeStatusHeader topLevelRange queryTotal tableTotal =
  let lower = rangeOffset topLevelRange
      upper = lower + toInteger queryTotal - 1
      contentRange = contentRangeH lower upper (toInteger <$> tableTotal)
      status = rangeStatus lower upper (toInteger <$> tableTotal)
  in (status, contentRange)
  where
    rangeStatus :: Integer -> Integer -> Maybe Integer -> Status
    rangeStatus _ _ Nothing = status200
    rangeStatus lower upper (Just total)
      | lower > total               = status416 -- 416 Range Not Satisfiable
      | (1 + upper - lower) < total = status206 -- 206 Partial Content
      | otherwise                   = status200 -- 200 OK

contentRangeH :: (Integral a, Show a) => a -> a -> Maybe a -> Header
contentRangeH lower upper total =
    ("Content-Range", toUtf8 headerValue)
    where
      headerValue   = rangeString <> "/" <> totalString :: Text
      rangeString
        | totalNotZero && fromInRange = show lower <> "-" <> show upper
        | otherwise = "*"
      totalString   = maybe "*" show total
      totalNotZero  = Just 0 /= total
      fromInRange   = lower <= upper
