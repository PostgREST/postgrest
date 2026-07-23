module PostgREST.Process.Restart
  ( -- * Process runner
    HandoverMode
  , isStandalone
  , isReplacement
  , AppRun
  , Ready
  , runRestartable

    -- * Replacement process configuration
  , ReplacementConfig(..)
  , currentReplacementConfig

    -- * Errors
  , HandoverError
  ) where

import PostgREST.Process.Restart.Impl   (runRestartable)
import PostgREST.Process.Restart.Shared
