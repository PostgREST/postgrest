{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module PostgREST.Version
  ( docsVersion
  , prettyVersion
  ) where

import qualified Data.Text as T

import Data.Version       (versionBranch)
import Development.GitRev (gitHash)
import Paths_postgrest    (version)

import Protolude


-- | User friendly version number
prettyVersion :: Text
prettyVersion =
  T.intercalate "." (map show $ versionBranch version) <> gitRev
  where
    gitRev =
      if $(gitHash) == "UNKNOWN"
        then mempty
        else " (" <> T.take 7 $(gitHash) <> ")"

-- | Version number used in docs
docsVersion :: Text
docsVersion = "v" <> T.dropEnd 1 (T.dropWhileEnd (/= '.') prettyVersion)
