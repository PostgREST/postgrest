{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PostgREST.Listener (runListener) where

import qualified Data.ByteString.Char8 as BS

import qualified Hasql.Connection      as SQL
import qualified Hasql.Notifications   as SQL
import           PostgREST.AppState    (AppState, getConfig)
import           PostgREST.Config      (AppConfig (..))
import           PostgREST.Observation (Observation (..),
                                        isDbListenerBug)
import           PostgREST.Version     (prettyVersion)

import qualified PostgREST.AppState as AppState
import qualified PostgREST.Config   as Config

import           Control.Arrow              ((&&&))
import           Data.Bitraversable         (bisequence)
import           Data.Either.Combinators    (whenRight)
import           Data.IORef                 (IORef, newIORef,
                                             readIORef, writeIORef)
import           Data.Time                  (UTCTime, diffUTCTime,
                                             nominalDiffTimeToSeconds)
import qualified Database.PostgreSQL.LibPQ  as LibPQ
import qualified Hasql.Session              as SQL
import           PostgREST.Config.Database  (queryPgVersion)
import           PostgREST.Config.PgVersion (pgvFullName)
import           Protolude
import           System.IO.Error            (isResourceVanishedError)

-- | Starts the Listener in a thread
runListener :: AppState -> IO ()
runListener appState = do
  AppConfig{..} <- getConfig appState
  when configDbChannelEnabled $ do
    started <- newIORef Nothing
    void . forkIO . void $ retryingListen started (15 * minute) (30 * minute) False appState
  where
    minute = 60

-- | Starts a LISTEN connection and handles notifications. It recovers with exponential backoff with a cap of 32 seconds, if the LISTEN connection is lost.
-- | This function never returns (but can throw) and return type enforces that.
retryingListen :: IORef (Maybe UTCTime) -> Int -> Int -> Bool -> AppState -> IO Void
retryingListen lastActivity currentKeepalivesIdle maxKeepalivesIdle retryingOnIdleTimeout appState = do
  AppConfig{..} <- AppState.getConfig appState
  let
    dbChannel = toS configDbChannel
    onError err = do
      -- ResourceVanished should be reported when reading from socket fails
      -- as long as hasql-notifications does not wrap IOException in something else...
      let resourceVanished = maybe False isResourceVanishedError (fromException @IOException err)
      (newTcpIdle, newmaxKeepalivesIdle) <-
        if resourceVanished then do
          readIORef lastActivity >>= maybe (pure (currentKeepalivesIdle, maxKeepalivesIdle)) adjustTcpIdle
        else
          pure (currentKeepalivesIdle, maxKeepalivesIdle)
      writeIORef lastActivity Nothing
      AppState.putIsListenerOn appState False
      observer $ DBListenFail dbChannel (Right err)
      when (isDbListenerBug err) $
        observer DBListenBugHint
      unless configDbPoolAutomaticRecovery $
        killThread mainThreadId

      -- retry the listener
      delay <- AppState.getNextListenerDelay appState
      observer $ DBListenRetry delay
      threadDelay (delay * oneSecondInMicro)
      unless (delay == maxDelay) $
        AppState.putNextListenerDelay appState (delay * 2)
      -- loop running the listener
      retryingListen lastActivity newTcpIdle newmaxKeepalivesIdle resourceVanished appState

  -- Execute the listener with with error handling
  handle onError $ do
    -- Make sure we don't leak connections on errors
    bracket
      -- acquire connection
      (SQL.acquire $ toUtf8 (addKeepalivesOptions $ Config.addTargetSessionAttrs $ Config.addFallbackAppName prettyVersion configDbUri))
      -- release connection
      (`whenRight` releaseConnection) $
      -- use connection
      \case
        Right db -> do
          SQL.listen db $ SQL.toPgIdentifier dbChannel
          (pqHost, pqPort) <- SQL.withLibPQConnection db $ bisequence . (LibPQ.host &&& LibPQ.port)
          pgFullName <- SQL.run (queryPgVersion False) db >>= either throwIO (pure . pgvFullName)

          AppState.putIsListenerOn appState True

          delay <- AppState.getNextListenerDelay appState
          when (delay > 1) $ do -- if we did a retry
            -- assume we lost notifications, refresh the schema cache
            AppState.schemaCacheLoader appState
            -- reset the delay
            AppState.putNextListenerDelay appState 1

          observer $ DBListenStart pqHost pqPort pgFullName dbChannel
          AppState.getTime appState >>= writeIORef lastActivity . pure

          -- wait for notifications
          -- this will never return, in case of an error it will throw and be caught by onError
          forever $ SQL.waitForNotifications handleNotification db

        Left err -> do
          observer $ DBListenFail dbChannel (Left err)
          exitFailure
  where
    observer = AppState.getObserver appState
    mainThreadId = AppState.getMainThreadId appState
    oneSecondInMicro = 1000000
    maxDelay = 32

    handleNotification channel msg = do
      if | BS.null msg            -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload schema" -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload config" -> observer (DBListenerGotConfigMsg channel) >> AppState.readInDbConfig False appState
         | otherwise              -> pure () -- Do nothing if anything else than an empty message is sent
      AppState.getTime appState >>= writeIORef lastActivity . Just


    cacheReloader =
      AppState.schemaCacheLoader appState

    releaseConnection = void . forkIO . handle (observer . DBListenerConnectionCleanupFail) . SQL.release

    adjustTcpIdle lastActiveTime = do
        currentIdleSeconds <- AppState.getTime appState <&> round . nominalDiffTimeToSeconds . (`diffUTCTime` lastActiveTime)
        let currentIdleTimeout = currentKeepalivesIdle + keepalivesInterval * keepalivesCount
        -- if our idle time == current idle timeout setting it means
        -- we have to make it shorter
        if currentIdleSeconds `div` currentIdleTimeout <= 1 then
          -- only adjust if this is the second idle timeout failure
          -- this is to eliminate spurious adjustments (TODO rethink if it is really needed)
          if retryingOnIdleTimeout then
            -- try with 1/2 of current keepalive idle
            -- remember that it is maximum we can try later
            pure (currentKeepalivesIdle `div` 2, currentKeepalivesIdle)
          else
            pure (currentKeepalivesIdle, maxKeepalivesIdle)
        else
          pure (currentKeepalivesIdle + (maxKeepalivesIdle - currentKeepalivesIdle) `div` 2, maxKeepalivesIdle)

    keepalivesInterval = currentKeepalivesIdle `div` (5 * keepalivesCount)
    keepalivesCount = 5

    -- (Config.addConnStringOption opt val) is an endomorphism
    -- so it is a Monoid under function composition
    -- Haskell is awesome
    addKeepalivesOptions = appEndo $ foldMap (Endo . uncurry Config.addConnStringOption . fmap show) [
        ("keepalives_count", keepalivesCount)
      , ("keepalives_interval", keepalivesInterval)
      , ("keepalives_idle", currentKeepalivesIdle)
      ]
