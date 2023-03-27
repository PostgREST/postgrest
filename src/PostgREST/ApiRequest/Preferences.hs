-- |
-- Module: PostgREST.ApiRequest.Preferences
-- Description: Track client preferences to be employed when processing requests
--
-- Track client prefences set in HTTP 'Prefer' headers according to RFC7240[1].
--
-- [1] https://datatracker.ietf.org/doc/html/rfc7240
--
module PostgREST.ApiRequest.Preferences
  ( Preferences(..)
  , PreferCount(..)
  , PreferMissing(..)
  , PreferParameters(..)
  , PreferRepresentation(..)
  , PreferResolution(..)
  , PreferTransaction(..)
  , fromHeaders
  , ToAppliedHeader(..)
  , shouldCount
  ) where

import qualified Data.ByteString.Char8     as BS
import qualified Data.Map                  as Map
import qualified Network.HTTP.Types.Header as HTTP

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
-- >>> deriving instance Show Preferences

-- | Preferences recognized by the application.
data Preferences
  = Preferences
    { preferResolution     :: Maybe PreferResolution
    , preferRepresentation :: PreferRepresentation
    , preferParameters     :: Maybe PreferParameters
    , preferCount          :: Maybe PreferCount
    , preferTransaction    :: Maybe PreferTransaction
    , preferMissing        :: Maybe PreferMissing
    }

-- |
-- Parse HTTP headers based on RFC7240[1] to identify preferences.
--
-- One header with comma-separated values can be used to set multiple preferences:
--
-- >>> pPrint $ fromHeaders [("Prefer", "resolution=ignore-duplicates, count=exact")]
-- Preferences
--     { preferResolution = Just IgnoreDuplicates
--     , preferRepresentation = None
--     , preferParameters = Nothing
--     , preferCount = Just ExactCount
--     , preferTransaction = Nothing
--     , preferMissing = Nothing
--     }
--
-- Multiple headers can also be used:
--
-- >>> pPrint $ fromHeaders [("Prefer", "resolution=ignore-duplicates"), ("Prefer", "count=exact"), ("Prefer", "missing=null")]
-- Preferences
--     { preferResolution = Just IgnoreDuplicates
--     , preferRepresentation = None
--     , preferParameters = Nothing
--     , preferCount = Just ExactCount
--     , preferTransaction = Nothing
--     , preferMissing = Just ApplyNulls
--     }
--
-- If a preference is set more than once, only the first is used:
--
-- >>> preferTransaction $ fromHeaders [("Prefer", "tx=commit, tx=rollback")]
-- Just Commit
--
-- This is also the case across multiple headers:
--
-- >>> :{
--   preferResolution . fromHeaders $
--     [ ("Prefer", "resolution=ignore-duplicates")
--     , ("Prefer", "resolution=merge-duplicates")
--     ]
-- :}
-- Just IgnoreDuplicates
--
-- Preferences not recognized by the application are ignored:
--
-- >>> preferResolution $ fromHeaders [("Prefer", "resolution=foo")]
-- Nothing
--
-- Preferences can be separated by arbitrary amounts of space, lower-case header is also recognized:
--
-- >>> pPrint $ fromHeaders [("prefer", "count=exact,    tx=commit   ,return=representation , missing=default")]
-- Preferences
--     { preferResolution = Nothing
--     , preferRepresentation = Full
--     , preferParameters = Nothing
--     , preferCount = Just ExactCount
--     , preferTransaction = Just Commit
--     , preferMissing = Just ApplyDefaults
--     }
--
fromHeaders :: [HTTP.Header] -> Preferences
fromHeaders headers =
  Preferences
    { preferResolution = parsePrefs [MergeDuplicates, IgnoreDuplicates]
    , preferRepresentation = fromMaybe None $ parsePrefs [Full, None, HeadersOnly]
    , preferParameters = parsePrefs [SingleObject, MultipleObjects]
    , preferCount = parsePrefs [ExactCount, PlannedCount, EstimatedCount]
    , preferTransaction = parsePrefs [Commit, Rollback]
    , preferMissing = parsePrefs [ApplyDefaults, ApplyNulls]
    }
  where
    prefHeaders = filter ((==) HTTP.hPrefer . fst) headers
    prefs = fmap BS.strip . concatMap (BS.split ',' . snd) $ prefHeaders

    parsePrefs :: ToHeaderValue a => [a] -> Maybe a
    parsePrefs vals =
      head $ mapMaybe (flip Map.lookup $ prefMap vals) prefs

    prefMap :: ToHeaderValue a => [a] -> Map.Map ByteString a
    prefMap = Map.fromList . fmap (\pref -> (toHeaderValue pref, pref))

-- |
-- Convert a preference into the value that we look for in the 'Prefer' headers.
--
-- >>> toHeaderValue MergeDuplicates
-- "resolution=merge-duplicates"
--
class ToHeaderValue a where
  toHeaderValue :: a -> ByteString

-- |
-- Header to indicate that a preference has been applied.
--
-- >>> toAppliedHeader MergeDuplicates
-- ("Preference-Applied","resolution=merge-duplicates")
--
class ToHeaderValue a => ToAppliedHeader a where
  toAppliedHeader :: a -> HTTP.Header
  toAppliedHeader x = (HTTP.hPreferenceApplied, toHeaderValue x)

-- | How to handle duplicate values.
data PreferResolution
  = MergeDuplicates
  | IgnoreDuplicates

instance ToHeaderValue PreferResolution where
  toHeaderValue MergeDuplicates  = "resolution=merge-duplicates"
  toHeaderValue IgnoreDuplicates = "resolution=ignore-duplicates"

instance ToAppliedHeader PreferResolution

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
  | MultipleObjects -- ^ Pass an array of json objects as params to a stored procedure.
  deriving Eq

-- TODO: Deprecate params=multiple-objects in next major version
instance ToHeaderValue PreferParameters where
  toHeaderValue SingleObject    = "params=single-object"
  toHeaderValue MultipleObjects = "params=multiple-objects"

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

instance ToAppliedHeader PreferTransaction

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

instance ToAppliedHeader PreferMissing
