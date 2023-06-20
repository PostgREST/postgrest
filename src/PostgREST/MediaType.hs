{-# LANGUAGE DuplicateRecordFields #-}

module PostgREST.MediaType
  ( MediaType(..)
  , MTPlanOption (..)
  , MTPlanFormat (..)
  , toContentType
  , toMime
  , decodeMediaType
  , getMediaType
  ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS (c2w)

import Network.HTTP.Types.Header (Header, hContentType)

import Protolude

--
-- $setup
-- Setup for doctests
-- >>> import Text.Pretty.Simple (pPrint)
-- >>> deriving instance Show MTPlanFormat
-- >>> deriving instance Show MTPlanOption
-- >>> deriving instance Show MediaType

-- | Enumeration of currently supported media types
data MediaType
  = MTApplicationJSON
  | MTSingularJSON
  | MTGeoJSON
  | MTTextCSV
  | MTTextPlain
  | MTTextXML
  | MTOpenAPI
  | MTUrlEncoded
  | MTOctetStream
  | MTAny
  | MTOther ByteString
  | MTPlan MediaType MTPlanFormat [MTPlanOption]
instance Eq MediaType where
  MTApplicationJSON == MTApplicationJSON = True
  MTSingularJSON    == MTSingularJSON    = True
  MTGeoJSON         == MTGeoJSON         = True
  MTTextCSV         == MTTextCSV         = True
  MTTextPlain       == MTTextPlain       = True
  MTTextXML         == MTTextXML         = True
  MTOpenAPI         == MTOpenAPI         = True
  MTUrlEncoded      == MTUrlEncoded      = True
  MTOctetStream     == MTOctetStream     = True
  MTAny             == MTAny             = True
  MTOther x         == MTOther y         = x == y
  MTPlan{}          == MTPlan{}          = True
  _                 == _                 = False

data MTPlanOption
  = PlanAnalyze | PlanVerbose | PlanSettings | PlanBuffers | PlanWAL

data MTPlanFormat
  = PlanJSON | PlanText

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
toMime MTApplicationJSON = "application/json"
toMime MTGeoJSON         = "application/geo+json"
toMime MTTextCSV         = "text/csv"
toMime MTTextPlain       = "text/plain"
toMime MTTextXML         = "text/xml"
toMime MTOpenAPI         = "application/openapi+json"
toMime MTSingularJSON    = "application/vnd.pgrst.object+json"
toMime MTUrlEncoded      = "application/x-www-form-urlencoded"
toMime MTOctetStream     = "application/octet-stream"
toMime MTAny             = "*/*"
toMime (MTOther ct)      = ct
toMime (MTPlan mt fmt opts) =
  "application/vnd.pgrst.plan+" <> toMimePlanFormat fmt <>
  ("; for=\"" <> toMime mt <> "\"") <>
  (if null opts then mempty else "; options=" <> BS.intercalate "|" (toMimePlanOption <$> opts))

toMimePlanOption :: MTPlanOption -> ByteString
toMimePlanOption PlanAnalyze  = "analyze"
toMimePlanOption PlanVerbose  = "verbose"
toMimePlanOption PlanSettings = "settings"
toMimePlanOption PlanBuffers  = "buffers"
toMimePlanOption PlanWAL      = "wal"

toMimePlanFormat :: MTPlanFormat -> ByteString
toMimePlanFormat PlanJSON = "json"
toMimePlanFormat PlanText = "text"

-- | Convert from ByteString to MediaType.
--
-- >>> decodeMediaType "application/json"
-- MTApplicationJSON
--
-- >>> decodeMediaType "application/vnd.pgrst.plan;"
-- MTPlan MTApplicationJSON PlanText []
--
-- >>> decodeMediaType "application/vnd.pgrst.plan;for=\"application/json\""
-- MTPlan MTApplicationJSON PlanText []
--
-- >>> decodeMediaType "application/vnd.pgrst.plan+json;for=\"text/csv\""
-- MTPlan MTTextCSV PlanJSON []
decodeMediaType :: BS.ByteString -> MediaType
decodeMediaType mt =
  case BS.split (BS.c2w ';') mt of
    "application/json":_                   -> MTApplicationJSON
    "application/geo+json":_               -> MTGeoJSON
    "text/csv":_                           -> MTTextCSV
    "text/plain":_                         -> MTTextPlain
    "text/xml":_                           -> MTTextXML
    "application/openapi+json":_           -> MTOpenAPI
    "application/vnd.pgrst.object+json":_  -> MTSingularJSON
    "application/vnd.pgrst.object":_       -> MTSingularJSON
    "application/x-www-form-urlencoded":_  -> MTUrlEncoded
    "application/octet-stream":_           -> MTOctetStream
    "application/vnd.pgrst.plan":rest      -> getPlan PlanText rest
    "application/vnd.pgrst.plan+text":rest -> getPlan PlanText rest
    "application/vnd.pgrst.plan+json":rest -> getPlan PlanJSON rest
    "*/*":_                                -> MTAny
    other:_                                -> MTOther other
    _                                      -> MTAny
  where
    getPlan fmt rest =
      let
        opts         = BS.split (BS.c2w '|') $ fromMaybe mempty (BS.stripPrefix "options=" =<< find (BS.isPrefixOf "options=") rest)
        inOpts str   = str `elem` opts
        dropAround p = BS.dropWhile p . BS.dropWhileEnd p
        mtFor        = fromMaybe MTApplicationJSON $ do
          foundFor    <- find (BS.isPrefixOf "for=") rest
          strippedFor <- BS.stripPrefix "for=" foundFor
          pure . decodeMediaType $ dropAround (== BS.c2w '"') strippedFor
      in
      MTPlan mtFor fmt $
        [PlanAnalyze  | inOpts "analyze" ] ++
        [PlanVerbose  | inOpts "verbose" ] ++
        [PlanSettings | inOpts "settings"] ++
        [PlanBuffers  | inOpts "buffers" ] ++
        [PlanWAL      | inOpts "wal"     ]

getMediaType :: MediaType -> MediaType
getMediaType mt = case mt of
  MTPlan mType _ _ -> mType
  other            -> other
