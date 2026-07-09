{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module System.Process.Restart
  ( -- * Process runner
    StartupMode
  , isStandalone
  , isReplacement
  , AppRun
  , Ready
  , runRestartable

    -- * Restart options
  , RestartOptions(..)
  , defaultRestartOptions

    -- * Signal handlers
  , RestartSignal
  , sigHUP
  , sigINT
  , sigTERM
  , sigUSR1
  , sigUSR2
  , SignalAction(..)
  , SignalHandler
  , SignalHandlers
  , handleSignal

    -- * Process configuration
  , ProcessConfig(..)
  , currentProcessConfig

    -- * Errors
  , HandoverError
  ) where

import qualified System.Process.Restart.Impl   as Impl
import           System.Process.Restart.Impl   (sigHUP, sigINT, sigTERM,
                                                sigUSR1, sigUSR2)
import           System.Process.Restart.Shared (HandoverError,
                                                ProcessConfig (..),
                                                RestartSignal,
                                                SignalAction (..),
                                                SignalHandler, SignalHandlers,
                                                StartupMode,
                                                currentProcessConfig,
                                                handleSignal, isReplacement,
                                                isStandalone)

import Protolude

-- | Application readiness callback.
type Ready =
  forall a.
  IO a ->
  -- ^ Action to stop the running application.
  IO ()

-- | Application entry point executed by the high-level restart runner.
type AppRun res =
  StartupMode ->
  -- ^ Startup mode detected by the runner.
  Ready ->
  -- ^ Action to mark the application as ready for serving.
  IO res

-- | High-level restart runner options.
data RestartOptions = RestartOptions
  { restartProcessConfig  :: Maybe ProcessConfig
  -- ^ Process configuration used to start replacement processes. When
  -- 'Nothing', the current executable, arguments, and environment are used.
  , restartSignalHandlers :: SignalHandlers
  -- ^ Static signal handling policy installed before the application starts and
  -- activated when the application marks itself ready.
  }

-- | Default restart options.
--
-- Uses the current executable, arguments, and environment for replacement
-- processes, and does not install any signal handlers.
defaultRestartOptions :: RestartOptions
defaultRestartOptions =
  RestartOptions
    { restartProcessConfig = Nothing
    , restartSignalHandlers = mempty
    }

-- | Run an application under the process restart framework.
runRestartable :: RestartOptions -> IO b -> AppRun a -> IO a
runRestartable RestartOptions{restartProcessConfig, restartSignalHandlers} initialStopAction runApp = do
  processCfg <- maybe currentProcessConfig pure restartProcessConfig
  Impl.runRestartable processCfg $ \mode ready -> do
    Impl.installSignalHandlers restartSignalHandlers initialStopAction initialStopAction
    runApp mode $ \stopAction ->
      void $ ready stopAction $
        Impl.installSignalHandlers restartSignalHandlers stopAction
