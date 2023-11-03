module PostgREST.Unix
  ( installSignalHandlers
  ) where

import qualified System.Posix.Signals as Signals

import qualified PostgREST.AppState as AppState

import Protolude


-- | Set signal handlers, only for systems with signals
installSignalHandlers :: AppState.AppState -> IO ()
installSignalHandlers appState = do
  let interrupt = throwTo (AppState.getMainThreadId appState) UserInterrupt
  install Signals.sigINT interrupt
  install Signals.sigTERM interrupt

  -- The SIGUSR1 signal updates the internal 'SchemaCache' by running
  -- 'connectionWorker' exactly as before.
  install Signals.sigUSR1 $ AppState.connectionWorker appState

  -- Re-read the config on SIGUSR2
  install Signals.sigUSR2 $ AppState.reReadConfig False appState
  where
    install signal handler =
      void $ Signals.installHandler signal (Signals.Catch handler) Nothing
