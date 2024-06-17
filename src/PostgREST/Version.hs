{-# LANGUAGE TemplateHaskell #-}
module PostgREST.Version
  ( docsVersion
  , prettyVersion
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Data.Version       (showVersion, versionBranch)
import Development.GitRev (gitHash)
import Paths_postgrest    (version)

import Protolude


-- | User friendly version number such as '1.1.1'.
-- Pre-release versions are tagged as such, e.g., '1.1 (pre-release)'.
-- If a git hash is available, it's added to the version, e.g., '1.1.1 (abcdef0)'.
prettyVersion :: ByteString
prettyVersion =
  toUtf8 (showVersion version) <> preRelease <> gitRev
  where
    gitRev =
      if $(gitHash) == ("UNKNOWN" :: Text) then
        mempty
      else
        " (" <> BS.take 7 $(gitHash) <> ")"
    preRelease = if isPreRelease then " (pre-release)" else mempty


-- | Version number used in docs.
-- Pre-release versions link to the latest docs
-- Uses only the first component of the version. Example: 'v1'
docsVersion :: Text
docsVersion
  | isPreRelease = "latest"
  | otherwise    =  "v" <> (T.intercalate "." . map show . take 1 $ versionBranch version)


-- | Versions with two components (e.g., '1.1') are treated as pre-releases.
isPreRelease :: Bool
isPreRelease =
  length (versionBranch version) == 2
