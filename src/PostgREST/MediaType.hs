{-# LANGUAGE DuplicateRecordFields #-}

module PostgREST.MediaType
  ( MediaType(..)
  , toContentType
  , toMime
  , decodeMediaType
  ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS (c2w)

import Network.HTTP.Types.Header (Header, hContentType)

import Protolude

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
  | MTPlan {
      planAnalyze :: Bool, planVerbose  :: Bool,
      planCosts   :: Bool, planSettings :: Bool,
      planBuffers :: Bool, planWAL      :: Bool,
      planTiming  :: Bool, planSummary  :: Bool }
instance Eq MediaType where
  MTApplicationJSON == MTApplicationJSON = True
  MTSingularJSON == MTSingularJSON       = True
  MTGeoJSON == MTGeoJSON                 = True
  MTTextCSV == MTTextCSV                 = True
  MTTextPlain == MTTextPlain             = True
  MTTextXML == MTTextXML                 = True
  MTOpenAPI == MTOpenAPI                 = True
  MTUrlEncoded == MTUrlEncoded           = True
  MTOctetStream == MTOctetStream         = True
  MTAny == MTAny                         = True
  MTPlan {} == MTPlan {}                 = True
  MTOther bs1 == MTOther bs2             = bs1 == bs2
  _ == _                                 = False

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
toMime MTPlan{}          = "application/vnd.pgrst.plan+json"
toMime MTAny             = "*/*"
toMime (MTOther ct)      = ct

-- | Convert from ByteString to MediaType. Warning: discards MIME parameters
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
    "application/vnd.pgrst.plan":rest      -> getPlan rest
    "application/vnd.pgrst.plan+json":rest -> getPlan rest
    "*/*":_                                -> MTAny
    other:_                                -> MTOther other
    []                                     -> MTAny
  where
    getPlan rest =
     let opts = BS.split (BS.c2w '|') $ fromMaybe mempty (BS.stripPrefix "options=" =<< head rest) in
     MTPlan {
      planAnalyze  = "analyze" `elem` opts, planVerbose  = "verbose" `elem` opts,
      planCosts    = "costs" `elem` opts,   planSettings = "settings" `elem` opts,
      planBuffers  = "buffers" `elem` opts, planWAL      = "wal" `elem` opts,
      planTiming   = "timing" `elem` opts,  planSummary  = "summary" `elem` opts
     }
