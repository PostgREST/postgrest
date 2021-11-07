module PostgREST.Request.Preferences
  ( Preferences(..)
  , PreferCount(..)
  , PreferParameters(..)
  , PreferRepresentation(..)
  , PreferResolution(..)
  , PreferTransaction(..)
  , fromHeaders
  , ToAppliedHeader(..)
  ) where

import qualified Data.ByteString           as BS
import qualified Data.Map                  as Map
import qualified Network.HTTP.Types.Header as HTTP

import Protolude


data Preferences
  = Preferences
    { preferResolution     :: Maybe PreferResolution
    , preferRepresentation :: PreferRepresentation
    , preferParameters     :: Maybe PreferParameters
    , preferCount          :: Maybe PreferCount
    , preferTransaction    :: Maybe PreferTransaction
    }

fromHeaders :: [HTTP.Header] -> Preferences
fromHeaders headers =
  Preferences
    { preferResolution = parsePrefs [MergeDuplicates, IgnoreDuplicates]
    , preferRepresentation = fromMaybe None $ parsePrefs [Full, None, HeadersOnly]
    , preferParameters = parsePrefs [SingleObject, MultipleObjects]
    , preferCount = parsePrefs [ExactCount, PlannedCount, EstimatedCount]
    , preferTransaction = parsePrefs [Commit, Rollback]
    }
  where
    prefHeaders = filter ((==) HTTP.hPrefer . fst) headers
    prefs = fmap strip . concatMap (BS.split comma . snd) $ prefHeaders
    comma = fromIntegral (ord ',')
    strip = BS.dropWhile (space ==) . BS.dropWhileEnd (space ==)
    space = fromIntegral (ord ' ')

    parsePrefs :: ToHeaderValue a => [a] -> Maybe a
    parsePrefs vals =
      head $ mapMaybe (flip Map.lookup $ prefMap vals) prefs

    prefMap :: ToHeaderValue a => [a] -> Map.Map ByteString a
    prefMap = Map.fromList . fmap (\pref -> (toHeaderValue pref, pref))

class ToHeaderValue a where
  toHeaderValue :: a -> ByteString

class ToHeaderValue a => ToAppliedHeader a where
  toAppliedHeader :: a -> HTTP.Header
  toAppliedHeader x = (HTTP.hPreferenceApplied, toHeaderValue x)

data PreferResolution
  = MergeDuplicates
  | IgnoreDuplicates

instance ToHeaderValue PreferResolution where
  toHeaderValue MergeDuplicates  = "resolution=merge-duplicates"
  toHeaderValue IgnoreDuplicates = "resolution=ignore-duplicates"

instance ToAppliedHeader PreferResolution

-- | How to return the mutated data. From https://tools.ietf.org/html/rfc7240#section-4.2
data PreferRepresentation
  = Full        -- ^ Return the body plus the Location header(in case of POST).
  | HeadersOnly -- ^ Return the Location header(in case of POST). This needs a SELECT privilege on the pk.
  | None        -- ^ Return nothing from the mutated data.
  deriving Eq

instance ToHeaderValue PreferRepresentation where
  toHeaderValue Full        = "return=representation"
  toHeaderValue None        = "return=minimal"
  toHeaderValue HeadersOnly = "return=headers-only"

data PreferParameters
  = SingleObject    -- ^ Pass all parameters as a single json object to a stored procedure
  | MultipleObjects -- ^ Pass an array of json objects as params to a stored procedure
  deriving Eq

instance ToHeaderValue PreferParameters where
  toHeaderValue SingleObject    = "params=single-object"
  toHeaderValue MultipleObjects = "params=multiple-objects"

data PreferCount
  = ExactCount     -- ^ exact count(slower)
  | PlannedCount   -- ^ PostgreSQL query planner rows count guess. Done by using EXPLAIN {query}.
  | EstimatedCount -- ^ use the query planner rows if the count is superior to max-rows, otherwise get the exact count.
 deriving Eq

instance ToHeaderValue PreferCount where
  toHeaderValue ExactCount     = "count=exact"
  toHeaderValue PlannedCount   = "count=planned"
  toHeaderValue EstimatedCount = "count=estimated"

data PreferTransaction
  = Commit   -- ^ Commit transaction - the default.
  | Rollback -- ^ Rollback transaction after sending the response - does not persist changes, e.g. for running tests.
  deriving Eq

instance ToHeaderValue PreferTransaction where
  toHeaderValue Commit   = "tx=commit"
  toHeaderValue Rollback = "tx=rollback"

instance ToAppliedHeader PreferTransaction
