-- |
-- Module: PostgREST.ApiRequest.Preferences
-- Description: Track client preferences to be employed when processing requests
--
-- Track client prefences set in HTTP 'Prefer' headers according to RFC7240[1].
--
-- [1] https://datatracker.ietf.org/doc/html/rfc7240
--
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.ApiRequest.Preferences
  ( Preferences(..)
  , PreferCount(..)
  , PreferHandling(..)
  , PreferMissing(..)
  , PreferParameters(..)
  , PreferRepresentation(..)
  , PreferResolution(..)
  , PreferTransaction(..)
  , fromHeaders
  , shouldCount
  , prefAppliedHeader
  ) where

import qualified Data.ByteString.Char8     as BS
import qualified Data.Map                  as Map
import qualified Network.HTTP.Types.Header as HTTP

import PostgREST.Config (AppConfig (..))

import Protolude

-- $setup
-- Setup for doctests
-- >>> import Text.Pretty.Simple (pPrint)
-- >>> deriving instance Show PreferResolution
-- >>> deriving instance Show PreferRepresentation
-- >>> deriving instance Show PreferParameters
-- >>> deriving instance Show PreferCount
-- >>> deriving instance Show PreferTransaction
-- >>> deriving instance Show PreferMissing
-- >>> deriving instance Show PreferHandling
-- >>> deriving instance Show Preferences

-- | Preferences recognized by the application.
data Preferences
  = Preferences
    { preferResolution     :: Maybe PreferResolution
    , preferRepresentation :: Maybe PreferRepresentation
    , preferParameters     :: Maybe PreferParameters
    , preferCount          :: Maybe PreferCount
    , preferTransaction    :: Maybe PreferTransaction
    , preferMissing        :: Maybe PreferMissing
    , preferHandling       :: Maybe PreferHandling
    , preferTimezone       :: Maybe ByteString
    , invalidPrefs         :: [ByteString]
    }

-- |
-- Parse HTTP headers based on RFC7240[1] to identify preferences.
--
-- >>> :{
--  let conf = AppConfig{
--        configDbTxAllowOverride=True,
--        configTimezoneNames=["America/Los_Angeles"]
--      }
-- :}
--
-- One header with comma-separated values can be used to set multiple preferences:
-- >>> pPrint $ fromHeaders conf [("Prefer", "resolution=ignore-duplicates, count=exact, timezone=America/Los_Angeles")]
-- Preferences
--     { preferResolution = Just IgnoreDuplicates
--     , preferRepresentation = Nothing
--     , preferParameters = Nothing
--     , preferCount = Just ExactCount
--     , preferTransaction = Nothing
--     , preferMissing = Nothing
--     , preferHandling = Nothing
--     , preferTimezone = Just "America/Los_Angeles"
--     , invalidPrefs = []
--     }
--
-- Multiple headers can also be used:
--
-- >>> pPrint $ fromHeaders conf [("Prefer", "resolution=ignore-duplicates"), ("Prefer", "count=exact"), ("Prefer", "missing=null"), ("Prefer", "handling=lenient"), ("Prefer", "invalid")]
-- Preferences
--     { preferResolution = Just IgnoreDuplicates
--     , preferRepresentation = Nothing
--     , preferParameters = Nothing
--     , preferCount = Just ExactCount
--     , preferTransaction = Nothing
--     , preferMissing = Just ApplyNulls
--     , preferHandling = Just Lenient
--     , preferTimezone = Nothing
--     , invalidPrefs = [ "invalid" ]
--     }
--
-- If a preference is set more than once, only the first is used:
--
-- >>> preferTransaction $ fromHeaders conf [("Prefer", "tx=commit, tx=rollback")]
-- Just Commit
--
-- This is also the case across multiple headers:
--
-- >>> :{
--   preferResolution . fromHeaders conf $
--     [ ("Prefer", "resolution=ignore-duplicates")
--     , ("Prefer", "resolution=merge-duplicates")
--     ]
-- :}
-- Just IgnoreDuplicates
--
--
-- Preferences can be separated by arbitrary amounts of space, lower-case header is also recognized:
--
-- >>> pPrint $ fromHeaders conf [("prefer", "count=exact,    tx=commit   ,return=representation , missing=default, handling=strict, anything")]
-- Preferences
--     { preferResolution = Nothing
--     , preferRepresentation = Just Full
--     , preferParameters = Nothing
--     , preferCount = Just ExactCount
--     , preferTransaction = Just Commit
--     , preferMissing = Just ApplyDefaults
--     , preferHandling = Just Strict
--     , preferTimezone = Nothing
--     , invalidPrefs = [ "anything" ]
--     }
--
fromHeaders :: AppConfig -> [HTTP.Header] -> Preferences
fromHeaders AppConfig{..} headers =
  Preferences
    { preferResolution     = parsePrefs [MergeDuplicates, IgnoreDuplicates]
    , preferRepresentation = parsePrefs [Full, None, HeadersOnly]
    , preferParameters     = parsePrefs [SingleObject]
    , preferCount          = parsePrefs [ExactCount, PlannedCount, EstimatedCount]
    , preferTransaction    = if configDbTxAllowOverride then parsePrefs [Commit, Rollback] else Nothing
    , preferMissing        = parsePrefs [ApplyDefaults, ApplyNulls]
    , preferHandling       = parsePrefs [Strict, Lenient]
    , preferTimezone       = timezonePref getTimezoneFromPrefs
    , invalidPrefs         = filter (not . hasTimezone) $ filter (`notElem` acceptedPrefs) prefs
    }
  where
    mapToHeadVal :: ToHeaderValue a => [a] -> [ByteString]
    mapToHeadVal = map toHeaderValue
    acceptedPrefs = mapToHeadVal [MergeDuplicates, IgnoreDuplicates] ++
                    mapToHeadVal [Full, None, HeadersOnly] ++
                    mapToHeadVal [SingleObject] ++
                    mapToHeadVal [ExactCount, PlannedCount, EstimatedCount] ++
                    mapToHeadVal [Commit, Rollback] ++
                    mapToHeadVal [ApplyDefaults, ApplyNulls] ++
                    mapToHeadVal [Strict, Lenient]

    prefHeaders = filter ((==) HTTP.hPrefer . fst) headers
    prefs = fmap BS.strip . concatMap (BS.split ',' . snd) $ prefHeaders
    hasTimezone p = BS.take 9 p == "timezone="
    getTimezoneFromPrefs = fromMaybe mempty $ listToMaybe [ BS.drop 9 p | p <- prefs, hasTimezone p]
    timezonePref tz = encodeUtf8 <$> find (== decodeUtf8 tz) configTimezoneNames

    parsePrefs :: ToHeaderValue a => [a] -> Maybe a
    parsePrefs vals =
      head $ mapMaybe (flip Map.lookup $ prefMap vals) prefs

    prefMap :: ToHeaderValue a => [a] -> Map.Map ByteString a
    prefMap = Map.fromList . fmap (\pref -> (toHeaderValue pref, pref))

prefAppliedHeader :: Preferences -> Maybe HTTP.Header
prefAppliedHeader Preferences {preferResolution, preferRepresentation, preferParameters, preferCount, preferTransaction, preferMissing, preferHandling } =
  if null prefsVals
    then Nothing
    else Just (HTTP.hPreferenceApplied, combined)
  where
    combined = BS.intercalate ", " prefsVals
    prefsVals = catMaybes [
        toHeaderValue <$> preferResolution
      , toHeaderValue <$> preferMissing
      , toHeaderValue <$> preferRepresentation
      , toHeaderValue <$> preferParameters
      , toHeaderValue <$> preferCount
      , toHeaderValue <$> preferTransaction
      , toHeaderValue <$> preferHandling
      ]

-- |
-- Convert a preference into the value that we look for in the 'Prefer' headers.
--
-- >>> toHeaderValue MergeDuplicates
-- "resolution=merge-duplicates"
--
class ToHeaderValue a where
  toHeaderValue :: a -> ByteString

-- | How to handle duplicate values.
data PreferResolution
  = MergeDuplicates
  | IgnoreDuplicates
  deriving Eq

instance ToHeaderValue PreferResolution where
  toHeaderValue MergeDuplicates  = "resolution=merge-duplicates"
  toHeaderValue IgnoreDuplicates = "resolution=ignore-duplicates"

-- |
-- How to return the mutated data.
--
-- From https://tools.ietf.org/html/rfc7240#section-4.2
data PreferRepresentation
  = Full        -- ^ Return the body.
  | HeadersOnly -- ^ Return the Location header(in case of POST). This needs a SELECT privilege on the pk.
  | None        -- ^ Return nothing from the mutated data.
  deriving Eq

instance ToHeaderValue PreferRepresentation where
  toHeaderValue Full        = "return=representation"
  toHeaderValue None        = "return=minimal"
  toHeaderValue HeadersOnly = "return=headers-only"

-- | How to pass parameters to stored procedures.
data PreferParameters
  = SingleObject    -- ^ Pass all parameters as a single json object to a stored procedure.
  deriving Eq

instance ToHeaderValue PreferParameters where
  toHeaderValue SingleObject    = "params=single-object"

-- | How to determine the count of (expected) results
data PreferCount
  = ExactCount     -- ^ Exact count (slower).
  | PlannedCount   -- ^ PostgreSQL query planner rows count guess. Done by using EXPLAIN {query}.
  | EstimatedCount -- ^ Use the query planner rows if the count is superior to max-rows, otherwise get the exact count.
  deriving Eq

instance ToHeaderValue PreferCount where
  toHeaderValue ExactCount     = "count=exact"
  toHeaderValue PlannedCount   = "count=planned"
  toHeaderValue EstimatedCount = "count=estimated"

shouldCount :: Maybe PreferCount -> Bool
shouldCount prefCount =
  prefCount == Just ExactCount || prefCount == Just EstimatedCount

-- | Whether to commit or roll back transactions.
data PreferTransaction
  = Commit   -- ^ Commit transaction - the default.
  | Rollback -- ^ Rollback transaction after sending the response - does not persist changes, e.g. for running tests.
  deriving Eq

instance ToHeaderValue PreferTransaction where
  toHeaderValue Commit   = "tx=commit"
  toHeaderValue Rollback = "tx=rollback"

-- |
-- How to handle the insertion/update when the keys specified in ?columns are not present
-- in the json body.
data PreferMissing
  = ApplyDefaults  -- ^ Use the default column value for missing values.
  | ApplyNulls     -- ^ Use the null value for missing values.
  deriving Eq

instance ToHeaderValue PreferMissing where
  toHeaderValue ApplyDefaults = "missing=default"
  toHeaderValue ApplyNulls    = "missing=null"

-- |
-- Handling of unrecognised preferences
data PreferHandling
  = Strict  -- ^ Throw error on unrecognised preferences
  | Lenient -- ^ Ignore unrecognised preferences
  deriving Eq

instance ToHeaderValue PreferHandling where
  toHeaderValue Strict  = "handling=strict"
  toHeaderValue Lenient = "handling=lenient"
