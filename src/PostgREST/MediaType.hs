{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}

module PostgREST.MediaType
  ( MediaType(..)
  , NormalMedia(..)
  , MTPlanOption (..)
  , MTPlanFormat (..)
  , MTPlanAttrs(..)
  , toContentType
  , toMime
  , decodeMediaType
  , getMediaType
  ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.List                as L
import           Data.Maybe               (fromJust)

import Network.HTTP.Types.Header (Header, hContentType)

import Protolude
--
-- $setup
-- Setup for doctests
-- >>> import Text.Pretty.Simple (pPrint)
-- >>> deriving instance Show NormalMedia
-- >>> deriving instance Show MTPlanFormat
-- >>> deriving instance Show MTPlanOption
-- >>> deriving instance Show MTPlanAttrs
-- >>> deriving instance Show MediaType

-- | Enumeration of currently supported media types
data MediaType
  = MTNormal NormalMedia
  | MTPlan   MTPlanAttrs
  deriving Eq

data NormalMedia
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
  deriving Eq


data MTPlanAttrs = MTPlanAttrs (Maybe NormalMedia) MTPlanFormat [MTPlanOption]
instance Eq MTPlanAttrs where
  MTPlanAttrs {} == MTPlanAttrs {} = True -- we don't care about the attributes when comparing two MTPlan media types

data MTPlanOption
  = PlanAnalyze | PlanVerbose | PlanSettings | PlanBuffers | PlanWAL
  deriving Eq

data MTPlanFormat
  = PlanJSON | PlanText
  deriving Eq

-- | Convert MediaType to a Content-Type HTTP Header
toContentType :: MediaType -> Header
toContentType ct = (hContentType, toMime ct <> charset)
  where
    charset = case ct of
      MTNormal MTOctetStream -> mempty
      MTNormal (MTOther _)   -> mempty
      _                      -> "; charset=utf-8"

-- | Convert from MediaType to a ByteString representing the mime type
toMime :: MediaType -> ByteString
toMime (MTNormal x) = toMimeNormal x
toMime (MTPlan (MTPlanAttrs mt fmt opts)) =
  "application/vnd.pgrst.plan+" <> toMimePlanFormat fmt <>
  (if isNothing mt then mempty else "; for=\"" <> toMimeNormal (fromJust mt) <> "\"") <>
  (if null opts then mempty else "; options=" <> BS.intercalate "|" (toMimePlanOption <$> opts))

toMimeNormal :: NormalMedia -> ByteString
toMimeNormal = \case
  MTApplicationJSON -> "application/json"
  MTGeoJSON         -> "application/geo+json"
  MTTextCSV         -> "text/csv"
  MTTextPlain       -> "text/plain"
  MTTextXML         -> "text/xml"
  MTOpenAPI         -> "application/openapi+json"
  MTSingularJSON    -> "application/vnd.pgrst.object+json"
  MTUrlEncoded      -> "application/x-www-form-urlencoded"
  MTOctetStream     -> "application/octet-stream"
  MTAny             -> "*/*"
  (MTOther ct)      -> ct

toMimePlanOption :: MTPlanOption -> ByteString
toMimePlanOption = \case
  PlanAnalyze  -> "analyze"
  PlanVerbose  -> "verbose"
  PlanSettings -> "settings"
  PlanBuffers  -> "buffers"
  PlanWAL      -> "wal"

toMimePlanFormat :: MTPlanFormat -> ByteString
toMimePlanFormat PlanJSON = "json"
toMimePlanFormat PlanText = "text"

-- | Convert from ByteString to MediaType.
--
-- >>> decodeMediaType "application/json"
-- MTNormal MTApplicationJSON
--
-- >>> decodeMediaType "application/vnd.pgrst.plan;"
-- MTPlan (MTPlanAttrs Nothing PlanText [])
--
-- >>> decodeMediaType "application/vnd.pgrst.plan;for=\"application/json\""
-- MTPlan (MTPlanAttrs (Just MTApplicationJSON) PlanText [])
--
-- >>> decodeMediaType "application/vnd.pgrst.plan;for=\"text/csv\""
-- MTPlan (MTPlanAttrs (Just MTTextCSV) PlanText [])
--
-- A plan media type inside "for" shouldn't recurse
--
-- >>> decodeMediaType "application/vnd.pgrst.plan;for=\"application/vnd.pgrst.plan\""
-- MTPlan (MTPlanAttrs (Just (MTOther "application/vnd.pgrst.plan")) PlanText [])
decodeMediaType :: BS.ByteString -> MediaType
decodeMediaType bs =
  case BS.split (BS.c2w ';') bs of
    "application/vnd.pgrst.plan":rest      -> getPlan PlanText rest
    "application/vnd.pgrst.plan+text":rest -> getPlan PlanText rest
    "application/vnd.pgrst.plan+json":rest -> getPlan PlanJSON rest
    mt                                     -> MTNormal $ decodeNormalMediaType mt
  where
    getPlan fmt rest =
     let
       opts         = BS.split (BS.c2w '|') $ fromMaybe mempty (BS.stripPrefix "options=" =<< find (BS.isPrefixOf "options=") rest)
       inOpts str   = str `elem` opts
       mtFor        = decodeNormalMediaType . L.singleton . dropAround (== BS.c2w '"') <$> (BS.stripPrefix "for=" =<< find (BS.isPrefixOf "for=") rest)
       dropAround p = BS.dropWhile p . BS.dropWhileEnd p in
     MTPlan $ MTPlanAttrs mtFor fmt $
      [PlanAnalyze  | inOpts "analyze" ] ++
      [PlanVerbose  | inOpts "verbose" ] ++
      [PlanSettings | inOpts "settings"] ++
      [PlanBuffers  | inOpts "buffers" ] ++
      [PlanWAL      | inOpts "wal"     ]

-- | Convert from ByteString to MediaType. Warning: discards MIME parameters
decodeNormalMediaType :: [BS.ByteString] -> NormalMedia
decodeNormalMediaType bs =
  case bs of
    "application/json":_                  -> MTApplicationJSON
    "application/geo+json":_              -> MTGeoJSON
    "text/csv":_                          -> MTTextCSV
    "text/plain":_                        -> MTTextPlain
    "text/xml":_                          -> MTTextXML
    "application/openapi+json":_          -> MTOpenAPI
    "application/vnd.pgrst.object+json":_ -> MTSingularJSON
    "application/vnd.pgrst.object":_      -> MTSingularJSON
    "application/x-www-form-urlencoded":_ -> MTUrlEncoded
    "application/octet-stream":_          -> MTOctetStream
    "*/*":_                               -> MTAny
    other:_                               -> MTOther other
    _                                     -> MTAny

getMediaType :: MediaType -> NormalMedia
getMediaType mt = case mt of
  MTPlan (MTPlanAttrs (Just mType) _ _) -> mType
  MTPlan (MTPlanAttrs Nothing _ _)      -> MTApplicationJSON
  MTNormal x                            -> x
