{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module PostgREST.MediaType
  ( MediaType (..)
  , MTVndPlanOption (..)
  , MTVndPlanFormat (..)
  , toContentType
  , toMime
  , decodeMediaType
  )
where

import Data.Map ((!?))
import Data.Text.Encoding (decodeLatin1)
import Network.HTTP.Types.Header (Header, hContentType)
import Protolude

import Data.Aeson qualified as JSON
import Data.ByteString.Char8 qualified as BS
import Data.CaseInsensitive qualified as CI
import Network.HTTP.Media qualified as HTTP

-- $setup
-- >>> import qualified Text.ParserCombinators.Parsec as P

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
  | -- vendored media types
    MTVndArrayJSONStrip
  | MTVndSingularJSON Bool
  | -- TODO MTVndPlan should only have its options as [Text]. Its ResultAggregate should have the typed attributes.
    MTVndPlan MediaType MTVndPlanFormat [MTVndPlanOption]
  deriving (Eq, Generic, JSON.ToJSON, Show)

instance Hashable MediaType

data MTVndPlanOption
  = PlanAnalyze
  | PlanVerbose
  | PlanSettings
  | PlanBuffers
  | PlanWAL
  deriving (Eq, Generic, JSON.ToJSON, Show)

instance Hashable MTVndPlanOption

data MTVndPlanFormat
  = PlanJSON
  | PlanText
  deriving (Eq, Generic, JSON.ToJSON, Show)

instance Hashable MTVndPlanFormat

-- | Convert MediaType to a Content-Type HTTP Header
toContentType :: MediaType -> Header
toContentType ct = (hContentType, toMime ct <> charset)
  where
    charset = case ct of
      MTOctetStream -> mempty
      MTOther _ -> mempty
      _ -> "; charset=utf-8"

-- | Convert from MediaType to a ByteString representing the mime type
toMime :: MediaType -> ByteString
toMime MTApplicationJSON = "application/json"
toMime MTVndArrayJSONStrip = "application/vnd.pgrst.array+json;nulls=stripped"
toMime MTGeoJSON = "application/geo+json"
toMime MTTextCSV = "text/csv"
toMime MTTextPlain = "text/plain"
toMime MTTextXML = "text/xml"
toMime MTOpenAPI = "application/openapi+json"
toMime (MTVndSingularJSON True) = "application/vnd.pgrst.object+json;nulls=stripped"
toMime (MTVndSingularJSON False) = "application/vnd.pgrst.object+json"
toMime MTUrlEncoded = "application/x-www-form-urlencoded"
toMime MTOctetStream = "application/octet-stream"
toMime MTAny = "*/*"
toMime (MTOther ct) = encodeUtf8 ct
toMime (MTVndPlan mt fmt opts) =
  "application/vnd.pgrst.plan+"
    <> toMimePlanFormat fmt
    <> ("; for=\"" <> toMime mt <> "\"")
    <> (if null opts then mempty else "; options=" <> BS.intercalate "|" (toMimePlanOption <$> opts))

toMimePlanOption :: MTVndPlanOption -> ByteString
toMimePlanOption PlanAnalyze = "analyze"
toMimePlanOption PlanVerbose = "verbose"
toMimePlanOption PlanSettings = "settings"
toMimePlanOption PlanBuffers = "buffers"
toMimePlanOption PlanWAL = "wal"

toMimePlanFormat :: MTVndPlanFormat -> ByteString
toMimePlanFormat PlanJSON = "json"
toMimePlanFormat PlanText = "text"

-- | Convert from ByteString to MediaType.
--
-- >>> decodeMediaType "application/json"
-- MTApplicationJSON
--
-- >>> decodeMediaType "application/vnd.pgrst.plan"
-- MTVndPlan MTApplicationJSON PlanText []
--
-- >>> decodeMediaType "application/vnd.pgrst.plan;for=application/json"
-- MTVndPlan MTApplicationJSON PlanText []
--
-- >>> decodeMediaType "application/vnd.pgrst.plan ; for=text/xml ; options=analyze"
-- MTVndPlan MTTextXML PlanText [PlanAnalyze]
--
-- >>> decodeMediaType "application/vnd.pgrst.plan+json;for=text/csv"
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
decodeMediaType = decodeMediaType'
  where
    decodeMediaType' :: ByteString -> MediaType
    decodeMediaType' mt' =
      case (mainType, subType) of
        ("application", "json") -> MTApplicationJSON
        ("application", "geo+json") -> MTGeoJSON
        ("text", "csv") -> MTTextCSV
        ("text", "plain") -> MTTextPlain
        ("text", "xml") -> MTTextXML
        ("application", "openapi+json") -> MTOpenAPI
        ("application", "x-www-form-urlencoded") -> MTUrlEncoded
        ("application", "octet-stream") -> MTOctetStream
        ("application", "vnd.pgrst.plan") -> getPlan PlanText
        ("application", "vnd.pgrst.plan+text") -> getPlan PlanText
        ("application", "vnd.pgrst.plan+json") -> getPlan PlanJSON
        ("application", "vnd.pgrst.object+json") -> MTVndSingularJSON strippedNulls
        ("application", "vnd.pgrst.object") -> MTVndSingularJSON strippedNulls
        ("application", "vnd.pgrst.array+json") -> checkArrayNullStrip
        ("application", "vnd.pgrst.array") -> checkArrayNullStrip
        ("*", "*") -> MTAny
        _ -> MTOther $ decodeLatin1 mt'
      where
        (mainType, subType, params) = case HTTP.parseAccept mt' :: Maybe HTTP.MediaType of
          Just mt'' -> (HTTP.mainType mt'', HTTP.subType mt'', HTTP.parameters mt'')
          Nothing -> (CI.mk mt', "", mempty)
        getPlan fmt =
          MTVndPlan mtFor fmt $
            [PlanAnalyze | inOpts "analyze"]
              ++ [PlanVerbose | inOpts "verbose"]
              ++ [PlanSettings | inOpts "settings"]
              ++ [PlanBuffers | inOpts "buffers"]
              ++ [PlanWAL | inOpts "wal"]
          where
            mtFor = decodeMediaType' $ CI.original $ fromMaybe "application/json" (params !? "for")
            inOpts str = str `elem` opts
            opts = BS.split '|' $ CI.original $ fromMaybe mempty (params !? "options")
        strippedNulls = fromMaybe "false" (params !? "nulls") == "stripped"
        checkArrayNullStrip = if strippedNulls then MTVndArrayJSONStrip else MTApplicationJSON
