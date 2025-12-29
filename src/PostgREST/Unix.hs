{-# LANGUAGE CPP #-}

module PostgREST.Unix
  ( installSignalHandlers
  , createAndBindDomainSocket
  ) where

#ifndef mingw32_HOST_OS
import qualified System.Posix.Signals as Signals
#endif
import System.Posix.Types       (FileMode)
import System.PosixCompat.Files (setFileMode)

import           Data.String      (String)
import qualified Network.Socket   as NS
import           Protolude
import           System.Directory (removeFile)
import           System.IO.Error  (isDoesNotExistError)

-- | Set signal handlers
--
-- SIGINT:  Immediately throws UserInterrupt to the main thread.
-- SIGTERM: Delays shutdown by 'shutdownWaitPeriod' seconds before throwing UserInterrupt.
--          This allows load balancers (e.g., in AWS ECS) time to deregister the service
--          before the process terminates, reducing the risk of rejected connections.
-- SIGUSR1: Reloads the schema cache.
-- SIGUSR2: Reloads the configuration from the database.
installSignalHandlers :: Int -> ThreadId -> IO () -> IO () -> IO ()
#ifndef mingw32_HOST_OS
installSignalHandlers shutdownWaitPeriod tid usr1 usr2 = do
  let interruptImmediately = throwTo tid UserInterrupt
      interruptWithDelay = do
        when (shutdownWaitPeriod > 0) $
          threadDelay (shutdownWaitPeriod * 1000000)
        throwTo tid UserInterrupt
  install Signals.sigINT interruptImmediately
  install Signals.sigTERM interruptWithDelay
  install Signals.sigUSR1 usr1
  install Signals.sigUSR2 usr2
  where
    install signal handler =
      void $ Signals.installHandler signal (Signals.Catch handler) Nothing
#else
installSignalHandlers _ _ _ _ = pass
#endif

-- | Create a unix domain socket and bind it to the given path.
-- | The socket file will be deleted if it already exists.
createAndBindDomainSocket :: String -> FileMode -> IO NS.Socket
createAndBindDomainSocket path mode = do
  unless NS.isUnixDomainSocketAvailable $
    panic "Cannot run with unix socket on non-unix platforms. Consider deleting the `server-unix-socket` config entry in order to continue."
  deleteSocketFileIfExist path
  sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
  NS.bind sock $ NS.SockAddrUnix path
  NS.listen sock (max 2048 NS.maxListenQueue)
  setFileMode path mode
  return sock
  where
    deleteSocketFileIfExist path' =
      removeFile path' `catch` handleDoesNotExist
    handleDoesNotExist e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e
