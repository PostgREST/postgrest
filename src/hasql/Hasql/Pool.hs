module Hasql.Pool
  ( -- * Pool
    Pool
  , acquire
  , use
  , release

    -- * Errors
  , UsageError (..)
  )
where

import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.UUID.V4 qualified as Uuid

import Hasql.Connection (Connection)
import Hasql.Pool.Observation
import Hasql.Pool.Prelude hiding (timeout)

import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Connection.Setting
import Hasql.Pool.Config.Config qualified as Config
import Hasql.Pool.SessionErrorDestructors qualified as ErrorsDestruction
import Hasql.Session qualified as Session

-- | A connection tagged with metadata.
data Entry = Entry
  { entryConnection :: Connection,
    entryCreationTimeNSec :: Word64,
    entryUseTimeNSec :: Word64,
    entryId :: UUID
  }

entryIsAged :: Word64 -> Word64 -> Entry -> Bool
entryIsAged maxLifetime now Entry{..} =
  now > entryCreationTimeNSec + maxLifetime

entryIsIdle :: Word64 -> Word64 -> Entry -> Bool
entryIsIdle maxIdletime now Entry{..} =
  now > entryUseTimeNSec + maxIdletime

-- | Pool of connections to DB.
data Pool = Pool
  { -- | Pool size.
    poolSize :: Int,
    -- | Connection settings.
    poolConnectionSettings :: [Connection.Setting.Setting],
    -- | Acquisition timeout, in microseconds.
    poolAcquisitionTimeout :: Int,
    -- | Maximal connection lifetime, in nanoseconds.
    poolMaxLifetime :: Word64,
    -- | Maximal connection idle time, in nanoseconds.
    poolMaxIdletime :: Word64,
    -- | Avail connections.
    poolConnectionQueue :: TQueue Entry,
    -- | Remaining capacity.
    --     The pool size limits the sum of poolCapacity, the length
    --     of poolConnectionQueue and the number of in-flight
    --     connections.
    poolCapacity :: TVar Int,
    -- | Whether to return a connection to the pool.
    poolReuseVar :: TVar (TVar Bool),
    -- | To stop the manager thread via garbage collection.
    poolReaperRef :: IORef (),
    -- | Action for reporting the observations.
    poolObserver :: Observation -> IO ()
  }

-- | Create a connection-pool.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
--
-- If you want to ensure that the pool connects fine at the initialization phase, just run 'use' with an empty session (@pure ()@) and check for errors.
acquire :: Config.Config -> IO Pool
acquire config = do
  connectionQueue <- newTQueueIO
  capVar <- newTVarIO (Config.size config)
  reuseVar <- newTVarIO =<< newTVarIO True
  reaperRef <- newIORef ()

  managerTid <- forkIOWithUnmask $ \unmask -> unmask $ forever $ do
    threadDelay 1000000
    now <- getMonotonicTimeNSec
    join . atomically $ do
      entries <- flushTQueue connectionQueue
      let
        (agedEntries, unagedEntries) = partition (entryIsAged agingTimeoutNanos now) entries
        (idleEntries, liveEntries) = partition (entryIsIdle maxIdletimeNanos now) unagedEntries
        -- we should abort this loop when we receive an async exception
        catchSync act handler =
          act `catch` \e ->
            case fromException e of
              Just (_ :: SomeAsyncException) -> throwIO e
              Nothing -> handler e
        -- If the pool observer is buggy we have nowhere to send that error
        -- so in this scenario missing an observation is acceptable since we should not abort the loop.
        observeOrFailSilently entry status = catchSync (Config.observationHandler config (ConnectionObservation (entryId entry) status)) (const $ pure ())
        reap reason entry = do
          catchSync (Connection.release (entryConnection entry)) (observeOrFailSilently entry . TerminationErrorStatus reason)
          atomically $ modifyTVar' capVar succ
          observeOrFailSilently entry $ TerminatedConnectionStatus reason

      traverse_ (writeTQueue connectionQueue) liveEntries
      pure $ do
        forM_ agedEntries $ reap AgingConnectionTerminationReason
        forM_ idleEntries $ reap IdlenessConnectionTerminationReason

  void . mkWeakIORef reaperRef $ do
    -- When the pool goes out of scope, stop the manager.
    killThread managerTid

  pure
    $ Pool
      { poolSize = Config.size config,
        poolConnectionSettings = Config.connectionSettings config,
        poolAcquisitionTimeout = acqTimeoutMicros,
        poolMaxLifetime = agingTimeoutNanos,
        poolMaxIdletime = maxIdletimeNanos,
        poolConnectionQueue = connectionQueue,
        poolCapacity = capVar,
        poolReuseVar = reuseVar,
        poolReaperRef = reaperRef,
        poolObserver = Config.observationHandler config
      }
  where
    acqTimeoutMicros =
      div (fromIntegral (diffTimeToPicoseconds (Config.acquisitionTimeout config))) 1_000_000
    agingTimeoutNanos =
      div (fromIntegral (diffTimeToPicoseconds (Config.agingTimeout config))) 1_000
    maxIdletimeNanos =
      div (fromIntegral (diffTimeToPicoseconds (Config.idlenessTimeout config))) 1_000

-- | Release all the idle connections in the pool, and mark the in-use connections
-- to be released after use. Any connections acquired after the call will be
-- freshly established.
--
-- The pool remains usable after this action.
-- So you can use this function to reset the connections in the pool.
-- Naturally, you can also use it to release the resources.
release :: Pool -> IO ()
release Pool{..} =
  join . atomically $ do
    prevReuse <- readTVar poolReuseVar
    writeTVar prevReuse False
    newReuse <- newTVar True
    writeTVar poolReuseVar newReuse
    entries <- flushTQueue poolConnectionQueue
    pure $ forM_ entries $ \entry -> do
      Connection.release (entryConnection entry)
      atomically $ modifyTVar' poolCapacity succ
      poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus ReleaseConnectionTerminationReason))

data SlotRelease = ReturnConnection ConnectionReadyForUseReason | CloseConnection (Maybe ConnectionTerminationReason)

-- Used so we always return connection entries with
-- their pool 'generation' flag
-- keep slotReuse & slotRelease first so we can partially apply
data Slot = Slot
  { slotReuse :: TVar Bool,
    slotRelease :: TVar SlotRelease,
    slotEntry :: Entry
  }

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished.
--
-- Session failing with a 'Session.ClientError' gets interpreted as a loss of
-- connection. In such case the connection does not get returned to the pool
-- and a slot gets freed up for a new connection to be established the next
-- time one is needed. The error still gets returned from this function.
--
-- __Warning:__ Due to the mechanism mentioned above you should avoid intercepting this error type from within sessions.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use Pool{..} sess =
  bracket
    acquireSlot
    (traverse_ releaseSlot)
    $ \case
      Left ex -> pure $ Left ex
      Right slot -> runSessionOnSlot slot
  where
    tryAvailableConnection :: STM (IO (Either UsageError Entry))
    tryAvailableConnection =
      useAvailableConnection <$> readTQueue poolConnectionQueue

    tryNewConnection :: STM (IO (Either UsageError Entry))
    tryNewConnection = do
      capVal <- readTVar poolCapacity
      if capVal > 0 then do
        writeTVar poolCapacity $! pred capVal
        pure acquireNewConnection
      else
        retry

    tryAcquireSlot :: STM (IO (Either UsageError Slot))
    tryAcquireSlot = do
      reuse <- readTVar poolReuseVar
      act <- tryAvailableConnection <|> tryNewConnection
      releaseVar <- newTVar (CloseConnection Nothing)
      pure $ fmap (Slot reuse releaseVar) <$> act

    acquireSlot :: IO (Either UsageError Slot)
    acquireSlot = do
      maybeActWithoutContention <- atomically $ optional tryAcquireSlot

      case maybeActWithoutContention of
        Just act -> act
        Nothing -> do
          timeout <- do
            delay <- registerDelay poolAcquisitionTimeout
            pure $ readTVar delay

          startWaitingTime <- getMonotonicTimeNSec
          join . atomically $ do
            let
              observeConnectionWaitTime = do
                doneWaitingTime <- getMonotonicTimeNSec
                let delta = fromIntegral (doneWaitingTime - startWaitingTime) / 1000000
                poolObserver $ ConnectionWaitCompletedStatus delta
              waitForConnection = do
                useConnection <- tryAcquireSlot
                pure $ do
                  observeConnectionWaitTime
                  useConnection
              waitForTimeout = do
                timedOut <- timeout
                if timedOut then pure $ do
                  observeConnectionWaitTime
                  pure $ Left AcquisitionTimeoutUsageError
                else
                  retry
            waitForConnection <|> waitForTimeout

    acquireNewConnection = do
      now <- getMonotonicTimeNSec
      uuid <- Uuid.nextRandom
      poolObserver (ConnectionObservation uuid ConnectingConnectionStatus)
      Connection.acquire poolConnectionSettings >>= \case
        Left connErr -> do
          poolObserver (ConnectionObservation uuid (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason (fmap (Text.decodeUtf8With Text.lenientDecode) connErr))))
          atomically $ modifyTVar' poolCapacity succ
          pure $ Left $ ConnectionUsageError connErr
        Right connection -> do
          poolObserver (ConnectionObservation uuid (ReadyForUseConnectionStatus EstablishedConnectionReadyForUseReason))
          pure $ Right $ Entry connection now now uuid

    useAvailableConnection entry = do
      now <- getMonotonicTimeNSec
      if entryIsAged poolMaxLifetime now entry then do
        Connection.release (entryConnection entry)
        poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus AgingConnectionTerminationReason))
        acquireNewConnection
      else
        if entryIsIdle poolMaxIdletime now entry then do
          Connection.release (entryConnection entry)
          poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus IdlenessConnectionTerminationReason))
          acquireNewConnection
        else do
          pure $ Right $ entry{entryUseTimeNSec = now}

    runSessionOnSlot slot@Slot{slotEntry = entry} = do
      poolObserver (ConnectionObservation (entryId entry) InUseConnectionStatus)
      sessRes <- try @SomeException (Session.run sess (entryConnection entry))

      case sessRes of
        Left exc -> do
          -- Connection in unknown state mark to close it and free its capacity instead.
          markSlot slot $ CloseConnection $ Just $ SessionExceptionTerminationReason exc
          throwIO exc
        Right (Left err) ->
          ErrorsDestruction.reset
            ( \details -> do
                let reason = NetworkErrorConnectionTerminationReason (fmap (Text.decodeUtf8With Text.lenientDecode) details)
                markSlot slot $ CloseConnection $ Just reason
                pure $ Left $ SessionUsageError err
            )
            ( do
                markSlot slot $ ReturnConnection $ SessionFailedConnectionReadyForUseReason err
                pure $ Left $ SessionUsageError err
            )
            err
        Right (Right res) -> do
          markSlot slot $ ReturnConnection SessionSucceededConnectionReadyForUseReason
          pure $ Right res

    markSlot :: Slot -> SlotRelease -> IO ()
    markSlot Slot{slotRelease} =
      atomically . writeTVar slotRelease

    releaseSlot :: Slot -> IO ()
    releaseSlot Slot{slotReuse = reuseVar, slotRelease = releaseVar, slotEntry = entry} =
      join . atomically $ do
        releaseAction <- readTVar releaseVar
        reuse <- readTVar reuseVar
        case (releaseAction, reuse) of
          (ReturnConnection reason, True) -> do
            unGetTQueue poolConnectionQueue entry
            pure $ poolObserver (ConnectionObservation (entryId entry) (ReadyForUseConnectionStatus reason))
          (ReturnConnection _, False) -> pure $ do
            Connection.release (entryConnection entry)
            atomically $ modifyTVar' poolCapacity succ
            observeTermination ReleaseConnectionTerminationReason
          (CloseConnection reason, _) -> pure $ do
            Connection.release (entryConnection entry)
            atomically $ modifyTVar' poolCapacity succ
            observeTermination $ fromMaybe ReleaseConnectionTerminationReason reason
      where
        observeTermination = poolObserver . ConnectionObservation (entryId entry) . TerminatedConnectionStatus

-- | Union over all errors that 'use' can result in.
data UsageError
  = -- | Attempt to establish a connection failed.
    ConnectionUsageError Connection.ConnectionError
  | -- | Session execution failed.
    SessionUsageError Session.SessionError
  | -- | Timeout acquiring a connection.
    AcquisitionTimeoutUsageError
  deriving (Eq, Show)

instance Exception UsageError
