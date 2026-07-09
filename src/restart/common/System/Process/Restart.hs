module System.Process.Restart
  ( -- * Process runner
    StartupMode
  , isStandalone
  , isReplacement
  , AppRun
  , Ready
  , runRestartable
  , runRestartableWithSIGHUP

    -- * Process configuration
  , ProcessConfig(..)
  , currentProcessConfig

    -- * Errors
  , HandoverError
  ) where

import System.Process.Restart.Impl   (runRestartable, runRestartableWithSIGHUP)
import System.Process.Restart.Shared
