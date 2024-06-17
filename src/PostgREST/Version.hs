{-# LANGUAGE CPP #-}
module PostgREST.Version
  ( docsVersion
  , prettyVersion
  ) where

import qualified Data.Text as T

import Protolude

version :: [Text]
version = T.splitOn "." VERSION_postgrest

-- | User friendly version number such as '1.1.1'.
-- Pre-release versions are tagged as such, e.g., '1.1 (pre-release)'.
prettyVersion :: ByteString
prettyVersion =
  VERSION_postgrest <> preRelease
  where
    preRelease = if isPreRelease then " (pre-release)" else mempty


-- | Version number used in docs.
-- Pre-release versions link to the latest docs
-- Uses only the first component of the version. Example: 'v1'
docsVersion :: Text
docsVersion
  | isPreRelease = "latest"
  | otherwise    =  "v" <> (T.intercalate "." . map show . take 1 $ version)


-- | Versions with two components (e.g., '1.1') are treated as pre-releases.
isPreRelease :: Bool
isPreRelease =
  length version == 2
