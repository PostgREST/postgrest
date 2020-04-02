module UnixSocket (
  runAppInSocket
)where

import Network.Socket           (Family (AF_UNIX),
                                 SockAddr (SockAddrUnix), Socket,
                                 SocketType (Stream), bind, close,
                                 defaultProtocol, listen,
                                 maxListenQueue, socket)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp
import System.Directory         (removeFile)
import System.IO.Error          (isDoesNotExistError)
import System.Posix.Files       (setFileMode)
import System.Posix.Types       (FileMode)

import Protolude

createAndBindSocket :: FilePath -> Maybe FileMode -> IO Socket
createAndBindSocket socketFilePath maybeSocketFileMode = do
  deleteSocketFileIfExist socketFilePath
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock $ SockAddrUnix socketFilePath
  mapM_ (setFileMode socketFilePath) maybeSocketFileMode
  return sock
  where
    deleteSocketFileIfExist path = removeFile path `catch` handleDoesNotExist
    handleDoesNotExist e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

-- run the postgrest application with user defined socket.
runAppInSocket :: Settings -> Application -> Either Text FileMode -> FilePath -> IO ()
runAppInSocket settings app socketFileMode sockPath = do
  sock <- createAndBindSocket sockPath (rightToMaybe socketFileMode)
  putStrLn $ ("Listening on unix socket " :: Text) <> show sockPath
  listen sock maxListenQueue
  runSettingsSocket settings sock app
  -- clean socket up when done
  close sock
