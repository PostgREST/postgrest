module PostgREST.Unix
  ( runAppWithSocket
  , installSignalHandlers
  ) where

import qualified Network.Socket           as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Posix.Signals     as Signals

import Network.Wai        (Application)
import System.Directory   (removeFile)
import System.IO.Error    (isDoesNotExistError)
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)

import qualified PostgREST.AppState as AppState
import qualified PostgREST.Workers  as Workers

import Protolude


-- | Run the PostgREST application with user defined socket.
runAppWithSocket :: Warp.Settings -> Application -> FileMode -> FilePath -> IO ()
runAppWithSocket settings app socketFileMode socketFilePath =
  bracket createAndBindSocket Socket.close $ \socket -> do
    Socket.listen socket Socket.maxListenQueue
    Warp.runSettingsSocket settings socket app
  where
    createAndBindSocket = do
      deleteSocketFileIfExist socketFilePath
      sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      Socket.bind sock $ Socket.SockAddrUnix socketFilePath
      setFileMode socketFilePath socketFileMode
      return sock

    deleteSocketFileIfExist path =
      removeFile path `catch` handleDoesNotExist

    handleDoesNotExist e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

-- | Set signal handlers, only for systems with signals
installSignalHandlers :: AppState.AppState -> IO ()
installSignalHandlers appState = do
  let interrupt = throwTo (AppState.getMainThreadId appState) UserInterrupt
  install Signals.sigINT interrupt
  install Signals.sigTERM interrupt

  -- The SIGUSR1 signal updates the internal 'DbStructure' by running
  -- 'connectionWorker' exactly as before.
  install Signals.sigUSR1 $ Workers.connectionWorker appState

  -- Re-read the config on SIGUSR2
  install Signals.sigUSR2 $ Workers.reReadConfig False appState
  where
    install signal handler =
      void $ Signals.installHandler signal (Signals.Catch handler) Nothing
