{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module System.Process.Restart.Impl
  ( runRestartable
  , installSignalHandlers
  , sigHUP
  , sigINT
  , sigTERM
  , sigUSR1
  , sigUSR2
  ) where

import qualified Data.ByteString.Char8     as BS
import qualified Data.Map.Strict           as Map
import           Data.String               (String)
import qualified Data.Text.Encoding        as T
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB
import           System.Environment        (lookupEnv)
import           System.IO                 (BufferMode (NoBuffering), hClose,
                                            hSetBuffering)
import           System.IO.Error           (isEOFError)
import           System.Posix.IO           (FdOption (CloseOnExec), closeFd,
                                            createPipe, fdToHandle, setFdOption)
import           System.Posix.Process      (ProcessStatus (..), executeFile,
                                            forkProcess, getProcessStatus)
import qualified System.Posix.Signals      as Signals
import           System.Posix.Types        (Fd (..), ProcessID)

import System.Process.Restart.Shared

import Protolude

-- | Bidirectional line-oriented channel backed by a read handle and a write
-- handle.
data DuplexChannel = DuplexChannel
  { duplexReadHandle  :: Handle
  , duplexWriteHandle :: Handle
  }

-- | Parent-side replacement process handle.
data Replacement = Replacement
  { replacementProcessID :: ProcessID
  , replacementChannel   :: DuplexChannel
  }

data ReplacementExit
  = ReplacementExited ExitCode
  | ReplacementTerminated Int
  | ReplacementStopped Int
  | ReplacementStillRunning

data SystemdNotification
  = NotifyReady
  | NotifyReloading
  | NotifyMainPid ProcessID
  deriving (Eq, Show)

type SystemdNotifier =
  NonEmpty SystemdNotification -> IO ()

sigHUP :: RestartSignal
sigHUP =
  RestartSignal Signals.sigHUP

sigINT :: RestartSignal
sigINT =
  RestartSignal Signals.sigINT

sigTERM :: RestartSignal
sigTERM =
  RestartSignal Signals.sigTERM

sigUSR1 :: RestartSignal
sigUSR1 =
  RestartSignal Signals.sigUSR1

sigUSR2 :: RestartSignal
sigUSR2 =
  RestartSignal Signals.sigUSR2

handoverReadFdEnv :: String
handoverReadFdEnv = "PGRST_HANDOVER_READ_FD"

handoverWriteFdEnv :: String
handoverWriteFdEnv = "PGRST_HANDOVER_WRITE_FD"

-- | Run an application under the process handover framework.
--
-- The runner detects whether the process is a normal invocation or a
-- replacement child, performs the private parent/child handover protocol, and
-- exposes restart requests only in the accepted serving phase.
runRestartable ::
  ProcessConfig ->
  AppRun a ->
  IO a
runRestartable processCfg runApp = do
  bracketOnError
    getChildControl
    (traverse_ closeDuplexChannel) $
    \childControl -> do
      handoverLock <- newMVar ()
      runApp
        (maybe StartupStandalone (const StartupReplacement) childControl)
        (ready processCfg childControl handoverLock)

-- | Install gated signal handlers for the configured static signal policy.
installSignalHandlers :: SignalHandlers -> IO a -> IO a -> IO ()
installSignalHandlers signalHandlers stopAction restartAction =
  traverse_ installSignalHandler $ Map.toList $ signalHandlersMap signalHandlers
  where
    installSignalHandler (RestartSignal signal, handler) =
      void $ Signals.installHandler signal (Signals.Catch $ handler >>= signalAction) Nothing

    signalAction = \case
      Stop    -> void stopAction
      Restart -> void restartAction
      None    -> pass

ready ::
  ProcessConfig ->
  Maybe DuplexChannel ->
  MVar () ->
  Ready
ready processCfg childControl handoverLock stopAction withRequestReplacement =
  markReadyForHandover *> (Just <$> withRequestReplacement requestReplacement)
  where
    markReadyForHandover =
      maybe parentReady (finally <$> childReady <*> closeDuplexChannel) childControl

    parentReady = withSystemdNotifier ($ pure NotifyReady)

    childReady = liftA2 (*>) (`writeDuplexChannelLine` "READY") receiveCommit

    requestReplacement =
      withMVar handoverLock $ const $ runReplacementHandover processCfg stopAction

    receiveCommit=
      readDuplexChannelLine >=> \case
        Just "COMMIT" -> pass
        Nothing       -> throwIO $ HandoverProtocolError "Parent closed the handover channel before committing this process."
        Just msg      -> throwIO $ HandoverProtocolError $ "Unexpected parent handover message: " <> msg

runReplacementHandover :: ProcessConfig -> IO a -> IO a
runReplacementHandover processCfg stopAction = do
  withSystemdNotifier $ \notifySystemd -> do
    notifySystemd $ pure NotifyReloading
    bracket
      (startReplacement processCfg)
      (closeDuplexChannel . replacementChannel) $
      \replacement@Replacement{..} ->
        (`onException` cleanupUncommittedReplacement replacement) $ do
          waitForReplacementReady replacement
          notifySystemd (NotifyMainPid replacementProcessID :| [NotifyReady])
          (`writeDuplexChannelLine` "COMMIT") replacementChannel
  stopAction
  where
    cleanupUncommittedReplacement replacement = do
      void . forkIO $ terminateReplacement replacement

-- | Detect whether the current process was started as a handover child.
getChildControl :: IO (Maybe DuplexChannel)
getChildControl = do
  readFd <- lookupFd handoverReadFdEnv
  writeFd <- lookupFd handoverWriteFdEnv
  case (readFd, writeFd) of
    (Nothing, Nothing) -> pure Nothing
    (Just readFd', Just writeFd') -> Just <$> duplexChannelFromFds readFd' writeFd'
    _ -> throwIO $ HandoverProtocolError "Incomplete handover environment."
  where
    lookupFd key =
      fmap (Fd . fromIntegral) . (readMaybe @Int =<<) <$> lookupEnv key

-- | Start a replacement process and attach a private handover control channel.
startReplacement :: ProcessConfig -> IO Replacement
startReplacement ProcessConfig{..} = do
  (fromChildRead, fromChildWrite) <- createPipe
  (toChildRead, toChildWrite) <- createPipe
  traverse_ (`setCloseOnExec` True) [fromChildRead, fromChildWrite, toChildRead, toChildWrite]

  replacementProcessID <- forkProcess $ do
    closeFds [fromChildRead, toChildWrite]
    traverse_ (`setCloseOnExec` False) [toChildRead, fromChildWrite]
    execChild toChildRead fromChildWrite

  closeFds [toChildRead, fromChildWrite]
  replacementChannel <- duplexChannelFromFds fromChildRead toChildWrite

  pure Replacement{..}
  where
    execChild readFd writeFd = do
      let handoverEnv =
            [ (handoverReadFdEnv, show readFd)
            , (handoverWriteFdEnv, show writeFd)
            ]
          childEnv = mergeEnv handoverEnv processEnv
      executeFile processExecutable False processArguments (Just childEnv)
    closeFds =
      traverse_ (handle @IOException mempty . closeFd)
    mergeEnv overrides env =
      overrides <> filter ((`notElem` (fst <$> overrides)) . fst) env

-- | Wait for the replacement to report READY or close before readiness.
waitForReplacementReady :: Replacement -> IO ()
waitForReplacementReady replacement@Replacement{..} =
  readDuplexChannelLine replacementChannel >>= \case
    Nothing ->
      waitReplacementExit replacement >>= throwHandoverProtocolError . closedBeforeReady
    Just "READY" -> pass
    Just msg ->
      throwHandoverProtocolError $ "Unexpected replacement process handover message: " <> msg
  where
    throwHandoverProtocolError = throwIO . HandoverProtocolError

    closedBeforeReady = \case
      ReplacementStillRunning -> "Replacement process closed the handover channel before reporting readiness."
      replacementExit         -> "Replacement process exited before reporting readiness: " <> replacementExitText replacementExit

    replacementExitText = \case
      ReplacementExited ExitSuccess      -> "exited successfully"
      ReplacementExited (ExitFailure code) -> "exited with status " <> showByteString code
      ReplacementTerminated signal       -> "terminated by signal " <> showByteString signal
      ReplacementStopped signal          -> "stopped by signal " <> showByteString signal
      ReplacementStillRunning            -> "still running"

waitReplacementExit :: Replacement -> IO ReplacementExit
waitReplacementExit Replacement{..} =
  getProcessStatus True False replacementProcessID <&> \case
    Nothing -> ReplacementStillRunning
    Just (Exited exitCode) -> ReplacementExited exitCode
    Just (Terminated signal _) -> ReplacementTerminated $ fromIntegral signal
    Just (Stopped signal) -> ReplacementStopped $ fromIntegral signal

-- | Send SIGTERM to the replacement process, then escalate to SIGKILL if it
-- does not exit within the grace period.
terminateReplacement :: Replacement -> IO ()
terminateReplacement replacement@Replacement{..} = do
  Signals.signalProcess Signals.sigTERM replacementProcessID
  threadDelay terminationGracePeriodMicroseconds
  waitReplacementExit replacement >>= \case
    ReplacementStillRunning -> Signals.signalProcess Signals.sigKILL replacementProcessID
    _                       -> pass
  where
    terminationGracePeriodMicroseconds = 3_000_000

withSystemdNotifier :: (SystemdNotifier -> IO a) -> IO a
withSystemdNotifier action =
  lookupEnv "NOTIFY_SOCKET" >>= maybe (action mempty) withNotifySocket
  where
    withNotifySocket notifySocket =
      bracket
        (openNotifySocket notifySocket)
        NS.close
        $ \sock ->
          action $ NSB.sendAll sock . renderSystemdNotifications

    openNotifySocket notifySocket =
      bracketOnError
        (NS.socket NS.AF_UNIX NS.Datagram NS.defaultProtocol)
        NS.close
        $ \sock ->
          NS.connect sock (NS.SockAddrUnix $ notifySocketAddress notifySocket) $> sock

    notifySocketAddress ('@':xs) = '\0' : xs
    notifySocketAddress xs       = xs

    renderSystemdNotifications =
      (<> "\n") . BS.intercalate "\n" . toList . fmap render
      where
        render = \case
          NotifyReady        -> "READY=1"
          NotifyReloading    -> "RELOADING=1"
          NotifyMainPid pid  -> "MAINPID=" <> showByteString (fromIntegral pid :: Int)

showByteString :: Show a => a -> ByteString
showByteString =
  T.encodeUtf8 . show

duplexChannelFromFds :: Fd -> Fd -> IO DuplexChannel
duplexChannelFromFds readFd writeFd =
  DuplexChannel
    <$> fdToNoBufferingHandle readFd
    <*> fdToNoBufferingHandle writeFd
  where
    fdToNoBufferingHandle fd = do
      h <- fdToHandle fd
      hSetBuffering h NoBuffering
      pure h

setCloseOnExec :: Fd -> Bool -> IO ()
setCloseOnExec fd =
  setFdOption fd CloseOnExec

writeDuplexChannelLine :: DuplexChannel -> ByteString -> IO ()
writeDuplexChannelLine DuplexChannel{..} =
  BS.hPutStrLn duplexWriteHandle

readDuplexChannelLine :: DuplexChannel -> IO (Maybe ByteString)
readDuplexChannelLine DuplexChannel{..} =
  either (const Nothing) Just <$> tryJust eof (BS.hGetLine duplexReadHandle)
  where
    eof ex
      | isEOFError ex = Just ()
      | otherwise     = Nothing

closeDuplexChannel :: DuplexChannel -> IO ()
closeDuplexChannel DuplexChannel{..} =
  traverse_ hCloseQuiet [duplexReadHandle, duplexWriteHandle]
  where
    hCloseQuiet =
      handle @IOException mempty . hClose
