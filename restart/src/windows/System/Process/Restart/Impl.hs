{-# LANGUAGE RankNTypes #-}

module System.Process.Restart.Impl
  ( runRestartable
  , installSignalHandlers
  , sigHUP
  , sigINT
  , sigTERM
  , sigUSR1
  , sigUSR2
  ) where

import System.Process.Restart.Shared

import Protolude

sigHUP :: RestartSignal
sigHUP =
  RestartSignal 1

sigINT :: RestartSignal
sigINT =
  RestartSignal 2

sigTERM :: RestartSignal
sigTERM =
  RestartSignal 15

sigUSR1 :: RestartSignal
sigUSR1 =
  RestartSignal 10

sigUSR2 :: RestartSignal
sigUSR2 =
  RestartSignal 12

-- | Run an application without handover support on platforms where the
-- replacement protocol is unavailable.
runRestartable ::
  ProcessConfig ->
  AppRun a ->
  IO a
runRestartable _ runApp =
  runApp StartupStandalone (const $ const $ pure Nothing)

-- | Ignore signal handlers on platforms where the signal API is unavailable.
installSignalHandlers :: SignalHandlers -> IO a -> IO a -> IO ()
installSignalHandlers _ _ _ =
  pass
