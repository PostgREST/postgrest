{-# LANGUAGE DuplicateRecordFields #-}

module PostgREST.ContentType
  ( ContentType(..)
  , toHeader
  , toMime
  , decodeContentType
  ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS (c2w)

import Network.HTTP.Types.Header (Header, hContentType)

import Protolude

-- | Enumeration of currently supported response content types
data ContentType
  = CTApplicationJSON
  | CTSingularJSON
  | CTTextCSV
  | CTTextPlain
  | CTOpenAPI
  | CTUrlEncoded
  | CTOctetStream
  | CTAny
  | CTOther ByteString
  deriving (Eq)

-- | Convert from ContentType to a full HTTP Header
toHeader :: ContentType -> Header
toHeader ct = (hContentType, toMime ct <> charset)
  where
    charset = case ct of
      CTOctetStream -> mempty
      CTOther _     -> mempty
      _             -> "; charset=utf-8"

-- | Convert from ContentType to a ByteString representing the mime type
toMime :: ContentType -> ByteString
toMime CTApplicationJSON = "application/json"
toMime CTTextCSV         = "text/csv"
toMime CTTextPlain       = "text/plain"
toMime CTOpenAPI         = "application/openapi+json"
toMime CTSingularJSON    = "application/vnd.pgrst.object+json"
toMime CTUrlEncoded      = "application/x-www-form-urlencoded"
toMime CTOctetStream     = "application/octet-stream"
toMime CTAny             = "*/*"
toMime (CTOther ct)      = ct

-- | Convert from ByteString to ContentType. Warning: discards MIME parameters
decodeContentType :: BS.ByteString -> ContentType
decodeContentType ct =
  case BS.takeWhile (/= BS.c2w ';') ct of
    "application/json"                  -> CTApplicationJSON
    "text/csv"                          -> CTTextCSV
    "text/plain"                        -> CTTextPlain
    "application/openapi+json"          -> CTOpenAPI
    "application/vnd.pgrst.object+json" -> CTSingularJSON
    "application/vnd.pgrst.object"      -> CTSingularJSON
    "application/x-www-form-urlencoded" -> CTUrlEncoded
    "application/octet-stream"          -> CTOctetStream
    "*/*"                               -> CTAny
    ct'                                 -> CTOther ct'
