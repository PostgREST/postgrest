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
  deriving (Eq)

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

-- | Convert from ByteString to MediaType. Warning: discards MIME parameters
decodeMediaType :: BS.ByteString -> MediaType
decodeMediaType ct =
  case BS.takeWhile (/= BS.c2w ';') ct of
    "application/json"                  -> MTApplicationJSON
    "application/geo+json"              -> MTGeoJSON
    "text/csv"                          -> MTTextCSV
    "text/plain"                        -> MTTextPlain
    "text/xml"                          -> MTTextXML
    "application/openapi+json"          -> MTOpenAPI
    "application/vnd.pgrst.object+json" -> MTSingularJSON
    "application/vnd.pgrst.object"      -> MTSingularJSON
    "application/x-www-form-urlencoded" -> MTUrlEncoded
    "application/octet-stream"          -> MTOctetStream
    "*/*"                               -> MTAny
    ct'                                 -> MTOther ct'
