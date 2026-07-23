{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module PostgREST.Process.Restart.Impl
  ( runRestartable
  ) where

import qualified Data.ByteString.Char8     as BS
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
import           System.Posix.Signals      (sigKILL, sigTERM, signalProcess)
import           System.Posix.Types        (Fd (..), ProcessID)

import PostgREST.Process.Restart.Shared

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
  ReplacementConfig ->
  AppRun a ->
  IO a
runRestartable replacementCfg runApp = do
  bracketOnError
    getChildControl
    (traverse_ closeDuplexChannel) $
    \childControl -> do
      handoverLock <- newMVar ()
      runApp
        (HandoverMode $ isJust childControl)
        (ready replacementCfg childControl handoverLock)

ready ::
  ReplacementConfig ->
  Maybe DuplexChannel ->
  MVar () ->
  Ready
ready replacementCfg childControl handoverLock stopAction withRequestReplacement =
  markReadyForHandover *> withRequestReplacement requestReplacement
  where
    markReadyForHandover =
      foldMap (finally <$> markChildReady <*> closeDuplexChannel) childControl
    markChildReady = liftA2 (*>) (`writeDuplexChannelLine` "READY") receiveCommit
    requestReplacement =
      withMVar handoverLock $ const $ runReplacementHandover replacementCfg stopAction
    receiveCommit childChannel =
      (readDuplexChannelLine childChannel >>= handleCommit) `finally` closeDuplexChannel childChannel
    handleCommit = \case
      Just "COMMIT" -> pass
      Nothing       -> throwIO $ HandoverProtocolError "Parent closed the handover channel before committing this process."
      Just msg      -> throwIO $ HandoverProtocolError $ "Unexpected parent handover message: " <> msg

runReplacementHandover :: ReplacementConfig -> IO a -> IO a
runReplacementHandover replacementCfg stopAction = do
  withSystemdNotifier $ \notifySystemd -> do
    notifySystemd $ pure NotifyReloading
    bracket
      (startReplacement replacementCfg)
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
startReplacement :: ReplacementConfig -> IO Replacement
startReplacement ReplacementConfig{..} = do
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
            [ (handoverReadFdEnv, showFd readFd)
            , (handoverWriteFdEnv, showFd writeFd)
            ]
          childEnv = mergeEnv handoverEnv replacementEnv
      executeFile replacementExecutable False replacementArguments (Just childEnv)
    closeFds =
      traverse_ (handle @IOException mempty . closeFd)
    mergeEnv overrides env =
      overrides <> filter ((`notElem` (fst <$> overrides)) . fst) env
    showFd :: Fd -> String
    showFd (Fd fd) = show (fromIntegral fd :: Int)

-- | Wait for the replacement to report READY or close before readiness.
waitForReplacementReady :: Replacement -> IO ()
waitForReplacementReady replacement@Replacement{..} =
  readDuplexChannelLine replacementChannel >>= \case
    Nothing ->
      waitReplacementExit replacement >>= throwHandoverFailed . closedBeforeReady
    Just "READY" -> pass
    Just msg ->
      throwHandoverFailed $ "Unexpected replacement process handover message: " <> msg
  where
    throwHandoverFailed = throwIO . HandoverFailed

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
  getProcessStatus True False replacementProcessID >>= \case
    Nothing -> pure ReplacementStillRunning
    Just (Exited exitCode) -> pure $ ReplacementExited exitCode
    Just (Terminated signal _) -> pure $ ReplacementTerminated $ fromIntegral signal
    Just (Stopped signal) -> pure $ ReplacementStopped $ fromIntegral signal

-- | Send SIGTERM to the replacement process, then escalate to SIGKILL if it
-- does not exit within the grace period.
terminateReplacement :: Replacement -> IO ()
terminateReplacement replacement@Replacement{..} = do
  signalProcess sigTERM replacementProcessID
  threadDelay terminationGracePeriodMicroseconds
  waitReplacementExit replacement >>= \case
    ReplacementStillRunning -> signalProcess sigKILL replacementProcessID
    _                       -> pass
  where
    terminationGracePeriodMicroseconds =
      3 * 1000 * 1000

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
      ensureTrailingNewline . BS.intercalate "\n" . toList . fmap render
      where
        render = \case
          NotifyReady        -> "READY=1"
          NotifyReloading    -> "RELOADING=1"
          NotifyMainPid pid  -> "MAINPID=" <> showByteString (fromIntegral pid :: Int)

    ensureTrailingNewline txt
      | "\n" `BS.isSuffixOf` txt = txt
      | otherwise                = txt <> "\n"

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
