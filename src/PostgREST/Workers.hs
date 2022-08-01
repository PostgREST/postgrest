{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Workers
  ( connectionWorker
  , reReadConfig
  , listener
  ) where

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text.Encoding         as T
import qualified Hasql.Notifications        as SQL
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Transaction.Sessions as SQL

import Control.Retry    (RetryStatus, capDelay, exponentialBackoff,
                         retrying, rsPreviousDelay)
import Hasql.Connection (acquire)

import PostgREST.AppState         (AppState)
import PostgREST.Config           (AppConfig (..), readAppConfig)
import PostgREST.Config.Database  (queryDbSettings, queryPgVersion)
import PostgREST.Config.PgVersion (PgVersion (..), minimumPgVersion)
import PostgREST.DbStructure      (queryDbStructure)
import PostgREST.Error            (PgError (PgError), checkIsFatal,
                                   errorPayload)

import qualified PostgREST.AppState as AppState

import Protolude


-- | Current database connection status data ConnectionStatus
data ConnectionStatus
  = NotConnected
  | Connected PgVersion
  | FatalConnectionError Text
  deriving (Eq)

-- | Schema cache status
data SCacheStatus
  = SCLoaded
  | SCOnRetry
  | SCFatalFail

-- | The purpose of this worker is to obtain a healthy connection to pg and an
-- up-to-date schema cache(DbStructure).  This method is meant to be called
-- multiple times by the same thread, but does nothing if the previous
-- invocation has not terminated. In all cases this method does not halt the
-- calling thread, the work is preformed in a separate thread.
--
-- Background thread that does the following :
--  1. Tries to connect to pg server and will keep trying until success.
--  2. Checks if the pg version is supported and if it's not it kills the main
--     program.
--  3. Obtains the dbStructure. If this fails, it goes back to 1.
connectionWorker :: AppState -> IO ()
connectionWorker appState = do
  isWorkerOn <- AppState.getIsWorkerOn appState
  -- Prevents multiple workers to be running at the same time. Could happen on
  -- too many SIGUSR1s.
  unless isWorkerOn $ do
    AppState.putIsWorkerOn appState True
    void $ forkIO work
  where
    work = do
      AppConfig{..} <- AppState.getConfig appState
      AppState.logWithZTime appState "Attempting to connect to the database..."
      connected <- connectionStatus appState
      case connected of
        FatalConnectionError reason ->
          -- Fatal error when connecting
          AppState.logWithZTime appState reason >> killThread (AppState.getMainThreadId appState)
        NotConnected ->
          -- Unreachable because connectionStatus will keep trying to connect
          return ()
        Connected actualPgVersion -> do
          -- Procede with initialization
          AppState.putPgVersion appState actualPgVersion
          when configDbChannelEnabled $
            AppState.signalListener appState
          AppState.logWithZTime appState "Connection successful"
          -- this could be fail because the connection drops, but the
          -- loadSchemaCache will pick the error and retry again
          when configDbConfig $ reReadConfig False appState
          scStatus <- loadSchemaCache appState
          case scStatus of
            SCLoaded ->
              -- do nothing and proceed if the load was successful
              return ()
            SCOnRetry ->
              -- retry reloading the schema cache
              work
            SCFatalFail ->
              -- die if our schema cache query has an error
              killThread $ AppState.getMainThreadId appState
          AppState.putIsWorkerOn appState False

-- | Check if a connection from the pool allows access to the PostgreSQL
-- database.  If not, the pool connections are released and a new connection is
-- tried.  Releasing the pool is key for rapid recovery. Otherwise, the pool
-- timeout would have to be reached for new healthy connections to be acquired.
-- Which might not happen if the server is busy with requests. No idle
-- connection, no pool timeout.
--
-- The connection tries are capped, but if the connection times out no error is
-- thrown, just 'False' is returned.
connectionStatus :: AppState -> IO ConnectionStatus
connectionStatus appState =
  retrying retrySettings shouldRetry $
    const $ AppState.releasePool appState >> getConnectionStatus
  where
    pool = AppState.getPool appState
    retrySettings = capDelay delayMicroseconds $ exponentialBackoff backoffMicroseconds
    delayMicroseconds = 32000000 -- 32 seconds
    backoffMicroseconds = 1000000 -- 1 second

    getConnectionStatus :: IO ConnectionStatus
    getConnectionStatus = do
      pgVersion <- SQL.use pool queryPgVersion
      case pgVersion of
        Left e -> do
          let err = PgError False e
          AppState.logWithZTime appState . T.decodeUtf8 . LBS.toStrict $ errorPayload err
          case checkIsFatal err of
            Just reason ->
              return $ FatalConnectionError reason
            Nothing ->
              return NotConnected
        Right version ->
          if version < minimumPgVersion then
            return . FatalConnectionError $
              "Cannot run in this PostgreSQL version, PostgREST needs at least "
              <> pgvName minimumPgVersion
          else
            return . Connected  $ version

    shouldRetry :: RetryStatus -> ConnectionStatus -> IO Bool
    shouldRetry rs isConnSucc = do
      let
        delay = fromMaybe 0 (rsPreviousDelay rs) `div` backoffMicroseconds
        itShould = NotConnected == isConnSucc
      when itShould . AppState.logWithZTime appState $
        "Attempting to reconnect to the database in "
        <> (show delay::Text)
        <> " seconds..."
      when itShould $ AppState.putRetryNextIn appState delay
      return itShould

-- | Load the DbStructure by using a connection from the pool.
loadSchemaCache :: AppState -> IO SCacheStatus
loadSchemaCache appState = do
  AppConfig{..} <- AppState.getConfig appState
  result <-
    let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
    SQL.use (AppState.getPool appState) . transaction SQL.ReadCommitted SQL.Read $
      queryDbStructure (toList configDbSchemas) configDbExtraSearchPath configDbPreparedStatements
  case result of
    Left e -> do
      let
        err = PgError False e
        putErr = AppState.logWithZTime appState . T.decodeUtf8 . LBS.toStrict $ errorPayload err
      case checkIsFatal err of
        Just hint -> do
          AppState.logWithZTime appState "A fatal error ocurred when loading the schema cache"
          putErr
          AppState.logWithZTime appState hint
          return SCFatalFail
        Nothing -> do
          AppState.putDbStructure appState Nothing
          AppState.logWithZTime appState "An error ocurred when loading the schema cache"
          putErr
          return SCOnRetry

    Right dbStructure -> do
      AppState.putDbStructure appState (Just dbStructure)
      when (isJust configDbRootSpec) .
        AppState.putJsonDbS appState . LBS.toStrict $ JSON.encode dbStructure
      AppState.logWithZTime appState "Schema cache loaded"
      return SCLoaded

-- | Starts a dedicated pg connection to LISTEN for notifications.  When a
-- NOTIFY <db-channel> - with an empty payload - is done, it refills the schema
-- cache.  It uses the connectionWorker in case the LISTEN connection dies.
listener :: AppState -> IO ()
listener appState = do
  AppConfig{..} <- AppState.getConfig appState
  let dbChannel = toS configDbChannel

  -- The listener has to wait for a signal from the connectionWorker.
  -- This is because when the connection to the db is lost, the listener also
  -- tries to recover the connection, but not with the same pace as the connectionWorker.
  -- Not waiting makes stderr quickly fill with connection retries messages from the listener.
  AppState.waitListener appState

  -- forkFinally allows to detect if the thread dies
  void . flip forkFinally (handleFinally dbChannel) $ do
    dbOrError <- acquire $ toUtf8 configDbUri
    case dbOrError of
      Right db -> do
        AppState.logWithZTime appState $ "Listening for notifications on the " <> dbChannel <> " channel"
        AppState.putIsListenerOn appState True
        SQL.listen db $ SQL.toPgIdentifier dbChannel
        SQL.waitForNotifications handleNotification db
      _ ->
        die $ "Could not listen for notifications on the " <> dbChannel <> " channel"
  where
    handleFinally dbChannel _ = do
      -- if the thread dies, we try to recover
      AppState.logWithZTime appState $ "Retrying listening for notifications on the " <> dbChannel <> " channel.."
      AppState.putIsListenerOn appState False
      -- assume the pool connection was also lost, call the connection worker
      connectionWorker appState
      -- retry the listener
      listener appState

    handleNotification _ msg
      | BS.null msg            = scLoader -- reload the schema cache
      | msg == "reload schema" = scLoader -- reload the schema cache
      | msg == "reload config" = reReadConfig False appState -- reload the config
      | otherwise              = pure () -- Do nothing if anything else than an empty message is sent

    scLoader =
      -- It's not necessary to check the loadSchemaCache success
      -- here. If the connection drops, the thread will die and
      -- proceed to recover.
      void $ loadSchemaCache appState

-- | Re-reads the config plus config options from the db
reReadConfig :: Bool -> AppState -> IO ()
reReadConfig startingUp appState = do
  AppConfig{..} <- AppState.getConfig appState
  dbSettings <-
    if configDbConfig then do
      qDbSettings <- SQL.use (AppState.getPool appState) $ queryDbSettings configDbPreparedStatements
      case qDbSettings of
        Left e -> do
          let
            err = PgError False e
            putErr = AppState.logWithZTime appState . T.decodeUtf8 . LBS.toStrict $ errorPayload err
          AppState.logWithZTime appState
            "An error ocurred when trying to query database settings for the config parameters"
          case checkIsFatal err of
            Just hint -> do
              putErr
              AppState.logWithZTime appState hint
              killThread (AppState.getMainThreadId appState)
            Nothing -> do
              putErr
          pure []
        Right x -> pure x
    else
      pure mempty
  readAppConfig dbSettings configFilePath (Just configDbUri) >>= \case
    Left err   ->
      if startingUp then
        panic err -- die on invalid config if the program is starting up
      else
        AppState.logWithZTime appState $ "Failed reloading config: " <> err
    Right newConf -> do
      AppState.putConfig appState newConf
      if startingUp then
        pass
      else
        AppState.logWithZTime appState "Config reloaded"
