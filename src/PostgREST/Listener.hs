{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Listener (runListener) where

import qualified Data.ByteString.Char8 as BS

import qualified Hasql.Connection      as SQL
import qualified Hasql.Notifications   as SQL
import           PostgREST.AppState    (AppState, getConfig)
import           PostgREST.Config      (AppConfig (..))
import           PostgREST.Observation (Observation (..))
import           PostgREST.Version     (prettyVersion)

import qualified PostgREST.AppState as AppState
import qualified PostgREST.Config   as Config

import Protolude

-- | Starts the Listener in a thread
runListener :: AppState -> IO ()
runListener appState = do
  AppConfig{..} <- getConfig appState
  when configDbChannelEnabled $
    void . forkIO $ retryingListen appState

-- | Starts a LISTEN connection and handles notifications. It recovers with exponential backoff with a cap of 32 seconds, if the LISTEN connection is lost.
retryingListen :: AppState -> IO ()
retryingListen appState = do
  AppConfig{..} <- AppState.getConfig appState
  let
    dbChannel = toS configDbChannel
    handleFinally err = do
      AppState.putIsListenerOn appState False
      observer $ DBListenFail dbChannel (Right err)
      unless configDbPoolAutomaticRecovery $
        killThread mainThreadId

      -- retry the listener
      delay <- AppState.getNextListenerDelay appState
      observer $ DBListenRetry delay
      threadDelay (delay * oneSecondInMicro)
      unless (delay == maxDelay) $
        AppState.putNextListenerDelay appState (delay * 2)
      retryingListen appState

  -- forkFinally allows to detect if the thread dies
  void . flip forkFinally handleFinally $ do
    dbOrError <- SQL.acquire $ toUtf8 (Config.addTargetSessionAttrs $ Config.addFallbackAppName prettyVersion configDbUri)
    case dbOrError of
      Right db -> do
        SQL.listen db $ SQL.toPgIdentifier dbChannel
        AppState.putIsListenerOn appState True

        delay <- AppState.getNextListenerDelay appState
        when (delay > 1) $ do -- if we did a retry
          -- assume we lost notifications, refresh the schema cache
          AppState.schemaCacheLoader appState
          -- reset the delay
          AppState.putNextListenerDelay appState 1

        observer $ DBListenStart dbChannel
        SQL.waitForNotifications handleNotification db

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
