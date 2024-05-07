{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PostgREST.MediaType
  ( MediaType(..)
  , MTVndPlanOption (..)
  , MTVndPlanFormat (..)
  , toContentType
  , toMime
  , decodeMediaType
  ) where

import qualified Data.Aeson      as JSON
import qualified Data.ByteString as BS

import Network.HTTP.Types.Header (Header, hContentType)

import           Data.Map           (fromList, (!?))
import qualified Data.Text          as T (break, drop, dropWhile,
                                          dropWhileEnd, null, splitOn,
                                          toLower)
import           Data.Text.Encoding (decodeLatin1)
import           Protolude

-- | Enumeration of currently supported media types
data MediaType
  = MTApplicationJSON
  | MTGeoJSON
  | MTTextCSV
  | MTTextPlain
  | MTTextXML
  | MTOpenAPI
  | MTUrlEncoded
  | MTOctetStream
  | MTAny
  | MTOther Text
  -- vendored media types
  | MTVndArrayJSONStrip
  | MTVndSingularJSON Bool
  -- TODO MTVndPlan should only have its options as [Text]. Its ResultAggregate should have the typed attributes.
  | MTVndPlan MediaType MTVndPlanFormat [MTVndPlanOption]
  deriving (Eq, Show, Generic, JSON.ToJSON)
instance Hashable MediaType

data MTVndPlanOption
  = PlanAnalyze | PlanVerbose | PlanSettings | PlanBuffers | PlanWAL
  deriving (Eq, Show, Generic, JSON.ToJSON)
instance Hashable MTVndPlanOption

data MTVndPlanFormat
  = PlanJSON | PlanText
  deriving (Eq, Show, Generic, JSON.ToJSON)
instance Hashable MTVndPlanFormat

-- | Convert MediaType to a Content-Type HTTP Header
toContentType :: MediaType -> Header
toContentType ct = (hContentType, toMime ct <> charset)
  where
    charset = case ct of
      MTOctetStream -> mempty
      MTOther _     -> mempty
      _             -> "; charset=utf-8"

-- | Convert from MediaType to a ByteString representing the mime type
toMime :: MediaType -> ByteString
toMime MTApplicationJSON      = "application/json"
toMime MTVndArrayJSONStrip    = "application/vnd.pgrst.array+json;nulls=stripped"
toMime MTGeoJSON              = "application/geo+json"
toMime MTTextCSV              = "text/csv"
toMime MTTextPlain            = "text/plain"
toMime MTTextXML              = "text/xml"
toMime MTOpenAPI              = "application/openapi+json"
toMime (MTVndSingularJSON True)  = "application/vnd.pgrst.object+json;nulls=stripped"
toMime (MTVndSingularJSON False) = "application/vnd.pgrst.object+json"
toMime MTUrlEncoded           = "application/x-www-form-urlencoded"
toMime MTOctetStream          = "application/octet-stream"
toMime MTAny                  = "*/*"
toMime (MTOther ct)           = encodeUtf8 ct
toMime (MTVndPlan mt fmt opts)   =
  "application/vnd.pgrst.plan+" <> toMimePlanFormat fmt <>
  ("; for=\"" <> toMime mt <> "\"") <>
  (if null opts then mempty else "; options=" <> BS.intercalate "|" (toMimePlanOption <$> opts))

toMimePlanOption :: MTVndPlanOption -> ByteString
toMimePlanOption PlanAnalyze  = "analyze"
toMimePlanOption PlanVerbose  = "verbose"
toMimePlanOption PlanSettings = "settings"
toMimePlanOption PlanBuffers  = "buffers"
toMimePlanOption PlanWAL      = "wal"

toMimePlanFormat :: MTVndPlanFormat -> ByteString
toMimePlanFormat PlanJSON = "json"
toMimePlanFormat PlanText = "text"

-- | Convert from ByteString to MediaType.
--
-- >>> decodeMediaType "application/json"
-- MTApplicationJSON
--
-- >>> decodeMediaType "application/vnd.pgrst.plan;"
-- MTVndPlan MTApplicationJSON PlanText []
--
-- >>> decodeMediaType "application/vnd.pgrst.plan;for=\"application/json\""
-- MTVndPlan MTApplicationJSON PlanText []
--
-- >>> decodeMediaType "application/vnd.pgrst.plan+json;for=\"text/csv\""
-- MTVndPlan MTTextCSV PlanJSON []
--
-- >>> decodeMediaType "application/vnd.pgrst.array+json;nulls=stripped"
-- MTVndArrayJSONStrip
--
-- >>> decodeMediaType "application/vnd.pgrst.array+json"
-- MTApplicationJSON
--
-- >>> decodeMediaType "application/vnd.pgrst.object+json;nulls=stripped"
-- MTVndSingularJSON True
--
-- >>> decodeMediaType "application/vnd.pgrst.object+json"
-- MTVndSingularJSON False
--
-- Test uppercase is parsed correctly (per issue #3478)
-- >>> decodeMediaType "ApplicatIon/vnd.PgRsT.object+json"
-- MTVndSingularJSON False
--
-- >>> decodeMediaType "application/vnd.twkb"
-- MTOther "application/vnd.twkb"

decodeMediaType :: ByteString -> MediaType
decodeMediaType mt = decodeMediaType' $ decodeLatin1 mt
  where
    decodeMediaType' :: Text -> MediaType
    decodeMediaType' mt' =
      case (T.toLower mainType, T.toLower subType, params) of
        ("application", "json", _)                  -> MTApplicationJSON
        ("application", "geo+json", _)              -> MTGeoJSON
        ("text", "csv", _)                          -> MTTextCSV
        ("text", "plain", _)                        -> MTTextPlain
        ("text", "xml", _)                          -> MTTextXML
        ("application", "openapi+json", _)          -> MTOpenAPI
        ("application", "x-www-form-urlencoded", _) -> MTUrlEncoded
        ("application", "octet-stream", _)          -> MTOctetStream
        ("application", "vnd.pgrst.plan", _)        -> getPlan PlanText
        ("application", "vnd.pgrst.plan+text", _)   -> getPlan PlanText
        ("application", "vnd.pgrst.plan+json", _)   -> getPlan PlanJSON
        ("application", "vnd.pgrst.object+json", _) -> MTVndSingularJSON strippedNulls
        ("application", "vnd.pgrst.object", _)      -> MTVndSingularJSON strippedNulls
        ("application", "vnd.pgrst.array+json", _)  -> checkArrayNullStrip
        ("application", "vnd.pgrst.array", _)       -> checkArrayNullStrip
        ("*","*",_)                                 -> MTAny
        _                                           -> MTOther mt'
      where
        (mainType, subType, params') = tokenizeMediaType mt'
        params = fromList $ map (first T.toLower) params' -- normalize parameter names to lowercase, per RFC 7321
        getPlan fmt = MTVndPlan mtFor fmt $
          [PlanAnalyze  | inOpts "analyze" ] ++
          [PlanVerbose  | inOpts "verbose" ] ++
          [PlanSettings | inOpts "settings"] ++
          [PlanBuffers  | inOpts "buffers" ] ++
          [PlanWAL      | inOpts "wal"     ]
          where
            mtFor = decodeMediaType' $ fromMaybe "application/json" (params !? "for")
            inOpts str = str `elem` opts
            opts = T.splitOn "|" $ fromMaybe mempty (params !? "options")
        strippedNulls = fromMaybe "false" (params !? "nulls") == "stripped"
        checkArrayNullStrip = if strippedNulls then MTVndArrayJSONStrip else MTApplicationJSON

-- | Split a Media Type string into components
-- >>> tokenizeMediaType "application/vnd.pgrst.plan+json;for=\"text/csv\""
-- ("application","vnd.pgrst.plan+json",[("for","text/csv")])
-- >>> tokenizeMediaType "*/*"
-- ("*","*",[])
-- >>> tokenizeMediaType "application/vnd.pgrst.plan;wat=\"application/json;text/csv\""
-- ("application","vnd.pgrst.plan",[("wat","application/json"),("text/csv\"","")])
tokenizeMediaType :: Text -> (Text, Text, [(Text, Text)])
tokenizeMediaType t = (mainType, subType, params)
  where
    (mainType, rest) = T.break (== '/') t
    (subType, restParams) = T.break (== ';') $ T.drop 1 rest
    params =
      let rp = T.drop 1 restParams
      in if T.null rp then [] else map param $ T.splitOn ";" rp -- FIXME: breaks if there's a ';' in a quoted value
    param p =
      let (k, v) = T.break (== '=') p
      in (k, dropAround (== '"') $ T.drop 1 v) -- FIXME: doesn't unescape quotes in values
    dropAround p = T.dropWhile p . T.dropWhileEnd p
