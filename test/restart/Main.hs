{-# LANGUAGE TypeApplications #-}

-- | Test helper executable used by the systemd restart tests.
--
-- It runs a tiny SO_REUSEPORT HTTP server through the process restart library so
-- the tests can verify SIGHUP handover, process replacement, and systemd
-- notifications.
module Main (main) where

import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Lazy     as LBS
import qualified Network.HTTP.Types       as HTTP
import qualified Network.Socket           as NS
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment       (lookupEnv)
import           System.Posix.Process     (getProcessID)
import           System.Posix.Types       (ProcessID)

import qualified System.Process.Restart as Restart

import Protolude hiding (Handler)

main :: IO ()
main = do
  port <- getPort
  processCfg <- Restart.currentProcessConfig
  Restart.runRestartableWithSIGHUP processCfg $ \mode ready -> do
    bracket (bindSocket port) closeSocketQuietly $ \socket -> do
      let settings =
            Warp.defaultSettings
              & Warp.setPort port
              & Warp.setBeforeMainLoop (ready $ closeSocketQuietly socket)

      Warp.runSettingsSocket settings socket $ app mode

getPort :: IO Int
getPort =
  fromMaybe 3000 . (readMaybe =<<) <$> lookupEnv "PORT"

bindSocket :: Int -> IO NS.Socket
bindSocket port =
  bracketOnError
    (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol)
    NS.close
    $ \socket -> do
      NS.setSocketOption socket NS.ReuseAddr 1
      NS.setSocketOption socket NS.ReusePort 1
      NS.bind socket $ NS.SockAddrInet (fromIntegral port) $ NS.tupleToHostAddress (127, 0, 0, 1)
      NS.listen socket $ max 2048 NS.maxListenQueue
      pure socket

app :: Restart.StartupMode -> Wai.Application
app mode req respond =
  case Wai.rawPathInfo req of
    "/pid"  -> ok =<< processId
    "/mode" -> ok modeText
    _       -> respond $ Wai.responseLBS HTTP.status404 [] "not found\n"
  where
    processId =
      renderPid <$> getProcessID
    modeText
      | Restart.isReplacement mode = "replacement\n"
      | otherwise                  = "standalone\n"
    ok =
      respond . Wai.responseLBS HTTP.status200 [("Content-Type", "text/plain")]

renderPid :: ProcessID -> LBS.ByteString
renderPid =
  BB.toLazyByteString . (<> BB.char8 '\n') . BB.intDec . fromIntegral

closeSocketQuietly :: NS.Socket -> IO ()
closeSocketQuietly =
  handle @IOException mempty . NS.close
