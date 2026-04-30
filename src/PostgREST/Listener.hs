{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Listener (runListener) where

import qualified Data.ByteString.Char8 as BS

import qualified Hasql.Connection      as SQL
import qualified Hasql.Notifications   as SQL
import           PostgREST.AppState    (AppState, getConfig)
import           PostgREST.Config      (AppConfig (..))
import           PostgREST.Observation (Observation (..))

import qualified PostgREST.AppState as AppState
import qualified PostgREST.Config   as Config

import           Control.Arrow              ((&&&))
import           Data.Bitraversable         (bisequence)
import           Data.Either.Combinators    (whenRight)
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.LibPQ  as LibPQ
import qualified Hasql.Session              as SQL
import           PostgREST.Config.Database  (queryPgVersion)
import           PostgREST.Config.PgVersion (pgvFullName)
import           Protolude

-- | Starts the Listener in a thread
runListener :: AppState -> IO ()
runListener appState = do
  AppConfig{..} <- getConfig appState
  when configDbChannelEnabled $
    void . forkIO . void $ retryingListen appState False

-- | Starts a LISTEN connection and handles notifications. It recovers with exponential backoff with a cap of 32 seconds, if the LISTEN connection is lost.
-- | This function never returns (but can throw) and return type enforces that.
retryingListen :: AppState -> Bool -> IO Void
retryingListen appState hasDbListenerBug = do
  cfg@AppConfig{..} <- AppState.getConfig appState
  let
    dbChannel = toS configDbChannel
    onError err = do
      AppState.putIsListenerOn appState False
      observer $ DBListenFail dbChannel (Right err)
      when (isDbListenerBug err) $
        observer DBListenBugCallQueryFix
      unless configDbPoolAutomaticRecovery $
        killThread mainThreadId

      -- retry the listener
      delay <- AppState.getNextListenerDelay appState
      observer $ DBListenRetry delay
      threadDelay (delay * oneSecondInMicro)
      unless (delay == maxDelay) $
        AppState.putNextListenerDelay appState (delay * 2)
      -- loop running the listener
      retryingListen appState (isDbListenerBug err)

  -- Execute the listener with with error handling
  handle onError $ do
    -- Make sure we don't leak connections on errors
    bracket
      -- acquire connection
      (SQL.acquire $
        Config.toConnectionSettings Config.addTargetSessionAttrs cfg)
      -- release connection
      (`whenRight` releaseConnection) $
      -- use connection
      \case
        Right db -> do
          when hasDbListenerBug $ SQL.run callNotifQueryUsage db >>= either throwIO pure
          SQL.listen db $ SQL.toPgIdentifier dbChannel
          (pqHost, pqPort) <- SQL.withLibPQConnection db $ bisequence . (LibPQ.host &&& LibPQ.port)
          pgFullName <- SQL.run queryPgVersion db >>= either throwIO (pure . pgvFullName)

          AppState.putIsListenerOn appState True

          delay <- AppState.getNextListenerDelay appState
          when (delay > 1) $ do -- if we did a retry
            -- assume we lost notifications, refresh the schema cache
            AppState.schemaCacheLoader appState
            -- reset the delay
            AppState.putNextListenerDelay appState 1

          observer $ DBListenStart pqHost pqPort pgFullName dbChannel

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

    handleNotification channel msg =
      if | BS.null msg            -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload schema" -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload config" -> observer (DBListenerGotConfigMsg channel) >> AppState.readInDbConfig False appState
         | otherwise              -> pure () -- Do nothing if anything else than an empty message is sent

    cacheReloader =
      AppState.schemaCacheLoader appState

    releaseConnection = void . forkIO . handle (observer . DBListenerConnectionCleanupFail) . SQL.release

    isDbListenerBug e = "could not access status of transaction" `T.isInfixOf` show e

    -- Used to fix a Postgres bug in the listener, see: https://github.com/PostgREST/postgrest/issues/3147#issuecomment-3494591361
    -- This query advances the async notification query tail, which solves this issue.
    callNotifQueryUsage :: SQL.Session ()
    callNotifQueryUsage = SQL.sql "SELECT pg_notification_queue_usage();"
