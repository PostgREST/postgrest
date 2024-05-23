{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Listener (runListener) where

import qualified Data.ByteString.Char8 as BS

import Control.Exception       (throw)
import Data.Either.Combinators (whenLeft)

import qualified Hasql.Connection      as SQL
import qualified Hasql.Notifications   as SQL
import           PostgREST.AppState    (AppState, getConfig)
import           PostgREST.Config      (AppConfig (..))
import           PostgREST.Observation (Observation (..))
import           PostgREST.Version     (prettyVersion)

import           Control.Retry      (RetryPolicy, RetryStatus (..),
                                     capDelay, exponentialBackoff,
                                     recoverAll, rsPreviousDelay)
import qualified PostgREST.AppState as AppState
import qualified PostgREST.Config   as Config

import Protolude

-- | Starts the Listener in a thread
runListener :: AppState -> IO ()
runListener appState = do
  AppConfig{..} <- getConfig appState
  when configDbChannelEnabled $
    void . forkIO $ retryingListen appState

-- | Starts a LISTEN connection and handles notifications. It recovers with exponential backoff if the LISTEN connection is lost.
-- TODO Once the listen channel is recovered, the retry status is not reset. So if the last backoff was 4 seconds, the next time recovery kicks in the backoff will be 8 seconds.
-- This is because `Hasql.Notifications.waitForNotifications` uses a forever loop that only finishes when it throws an exception.
retryingListen :: AppState -> IO ()
retryingListen appState = do
  AppConfig{..} <- AppState.getConfig appState
  let
    dbChannel = toS configDbChannel
    -- Try, catch and rethrow the exception. This is done so we can observe the failure message and let Control.Retry.recoverAll do its work.
    -- There's a `Control.Retry.recovering` we could use to avoid this rethrowing, but it's more complex to use.
    -- The root cause of these workarounds is that `Hasql.Notifications.waitForNotifications` uses exceptions.
    tryRethrow :: IO () -> IO ()
    tryRethrow action =  do
      act <- try action
      whenLeft act (\ex -> do
        AppState.putIsListenerOn appState False
        observer $ DBListenFail dbChannel (Right $ Left ex)
        unless configDbPoolAutomaticRecovery $ do
          killThread mainThreadId
        throw ex)

  recoverAll retryPolicy (\RetryStatus{rsIterNumber, rsPreviousDelay} -> do

    when (rsIterNumber > 0) $
      let delay = fromMaybe 0 rsPreviousDelay `div` oneSecondInUs in
      observer $ DBListenRetry delay

    connection <- SQL.acquire $ toUtf8 (Config.addTargetSessionAttrs $ Config.addFallbackAppName prettyVersion configDbUri)
    case connection of
      Right conn -> do

        tryRethrow $ SQL.listen conn $ SQL.toPgIdentifier dbChannel

        AppState.putIsListenerOn appState True
        observer $ DBListenStart dbChannel

        when (rsIterNumber > 0) $ do
          -- once we can LISTEN again, we might have lost schema cache notificacions, so reload
          AppState.connectionWorker appState

        tryRethrow $ SQL.waitForNotifications handleNotification conn

      Left err -> do
        observer $ DBListenFail dbChannel (Left err)
        -- throw an exception so recoverAll works
        exitFailure
    )

  where
    handleNotification channel msg =
      if | BS.null msg            -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload schema" -> observer (DBListenerGotSCacheMsg channel) >> cacheReloader
         | msg == "reload config" -> observer (DBListenerGotConfigMsg channel) >> AppState.reReadConfig False appState
         | otherwise              -> pure () -- Do nothing if anything else than an empty message is sent

    cacheReloader =
      AppState.connectionWorker appState

    observer = AppState.getObserver appState
    mainThreadId = AppState.getMainThreadId appState

    retryPolicy :: RetryPolicy
    retryPolicy =
      let
        delayMicroseconds = 32000000 -- 32 seconds
      in
      capDelay delayMicroseconds $ exponentialBackoff oneSecondInUs
    oneSecondInUs = 1000000 -- | One second in microseconds
