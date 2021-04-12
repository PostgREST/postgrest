{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Workers
  ( connectionWorker
  , reReadConfig
  , listener
  ) where

import qualified Data.ByteString            as BS
import qualified Hasql.Connection           as C
import qualified Hasql.Notifications        as N
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction.Sessions as HT

import Control.Retry (RetryStatus, capDelay, exponentialBackoff,
                      retrying, rsPreviousDelay)
import Data.Text.IO  (hPutStrLn)

import PostgREST.AppState              (AppState)
import PostgREST.Config                (AppConfig (..), readAppConfig)
import PostgREST.Config.Database       (loadDbSettings)
import PostgREST.DbStructure           (getDbStructure, getPgVersion)
import PostgREST.DbStructure.PgVersion (PgVersion (..),
                                        minimumPgVersion)
import PostgREST.Error                 (PgError (PgError),
                                        checkIsFatal, errorPayload)

import qualified PostgREST.AppState as AppState

import Protolude      hiding (hPutStrLn, head, toS)
import Protolude.Conv (toS)


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
      putStrLn ("Attempting to connect to the database..." :: Text)
      connected <- connectionStatus $ AppState.getPool appState
      case connected of
        FatalConnectionError reason ->
          -- Fatal error when connecting
          hPutStrLn stderr reason >> killThread (AppState.getMainThreadId appState)
        NotConnected ->
          -- Unreachable because connectionStatus will keep trying to connect
          return ()
        Connected actualPgVersion -> do
          when configDbChannelEnabled $
            -- tryPutMVar doesn't lock the thread. It should always succeed since
            -- the worker is the only mvar producer.
            AppState.putPgVersion appState actualPgVersion
          -- Procede with initialization
          putStrLn ("Connection successful" :: Text)
          -- this could be fail because the connection drops, but the
          -- loadSchemaCache will pick the error and retry again
          when configDbConfig $ reReadConfig False appState
          scStatus <- loadSchemaCache appState actualPgVersion
          case scStatus of
            SCLoaded ->
              -- do nothing and proceed if the load was successful
              return ()
            SCOnRetry ->
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
connectionStatus :: P.Pool -> IO ConnectionStatus
connectionStatus pool =
  retrying retrySettings shouldRetry $
    const $ P.release pool >> getConnectionStatus
  where
    retrySettings = capDelay delayMicroseconds $ exponentialBackoff backoffMicroseconds
    delayMicroseconds = 32000000 -- 32 seconds
    backoffMicroseconds = 1000000 -- 1 second

    getConnectionStatus :: IO ConnectionStatus
    getConnectionStatus = do
      pgVersion <- P.use pool getPgVersion
      case pgVersion of
        Left e -> do
          let err = PgError False e
          hPutStrLn stderr . toS $ errorPayload err
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
      when itShould . putStrLn $
        "Attempting to reconnect to the database in "
        <> (show delay::Text)
        <> " seconds..."
      return itShould

-- | Load the DbStructure by using a connection from the pool.
loadSchemaCache :: AppState -> PgVersion -> IO SCacheStatus
loadSchemaCache appState actualPgVersion = do
  AppConfig{..} <- AppState.getConfig appState
  result <-
    P.use (AppState.getPool appState) . HT.transaction HT.ReadCommitted HT.Read $
      getDbStructure (toList configDbSchemas) configDbExtraSearchPath actualPgVersion configDbPreparedStatements
  case result of
    Left e -> do
      let
        err = PgError False e
        putErr = hPutStrLn stderr . toS . errorPayload $ err
      case checkIsFatal err of
        Just _  -> do
          hPutStrLn stderr "A fatal error ocurred when loading the schema cache"
          putErr
          hPutStrLn stderr $
            "This is probably a bug in PostgREST, please report it at "
            <> "https://github.com/PostgREST/postgrest/issues"
          return SCFatalFail
        Nothing -> do
          hPutStrLn stderr "An error ocurred when loading the schema cache"
          putErr
          return SCOnRetry

    Right dbStructure -> do
      AppState.putDbStructure appState dbStructure
      putStrLn ("Schema cache loaded" :: Text)
      return SCLoaded

-- | Starts a dedicated pg connection to LISTEN for notifications.  When a
-- NOTIFY <db-channel> - with an empty payload - is done, it refills the schema
-- cache.  It uses the connectionWorker in case the LISTEN connection dies.
listener :: AppState -> IO ()
listener appState = do
  AppConfig{..} <- AppState.getConfig appState
  let dbChannel = toS configDbChannel

  -- AppState.getPgVersion makes the thread wait until the pgVersion has been
  -- set by the connectionWorker
  actualPgVersion <- AppState.getPgVersion appState
  -- forkFinally allows to detect if the thread dies
  void . flip forkFinally (handleFinally dbChannel) $ do
    dbOrError <- C.acquire $ toS configDbUri
    case dbOrError of
      Right db -> do
        putStrLn $ "Listening for notifications on the " <> dbChannel <> " channel"
        N.listen db $ N.toPgIdentifier dbChannel
        N.waitForNotifications (handleNotification actualPgVersion) db
      _ ->
        die $ "Could not listen for notifications on the " <> dbChannel <> " channel"
  where
    handleFinally dbChannel _ = do
      -- if the thread dies, we try to recover
      putStrLn $ "Retrying listening for notifications on the " <> dbChannel <> " channel.."
      -- assume the pool connection was also lost, call the connection worker
      connectionWorker appState

      -- retry the listener
      listener appState

    handleNotification actualPgVersion _ msg
      | BS.null msg            = scLoader actualPgVersion -- reload the schema cache
      | msg == "reload schema" = scLoader actualPgVersion -- reload the schema cache
      | msg == "reload config" = reReadConfig False appState -- reload the config
      | otherwise              = pure () -- Do nothing if anything else than an empty message is sent

    scLoader actualPgVersion =
      -- It's not necessary to check the loadSchemaCache success
      -- here. If the connection drops, the thread will die and
      -- proceed to recover below.
      void $ loadSchemaCache appState actualPgVersion

-- | Re-reads the config plus config options from the db
reReadConfig :: Bool -> AppState -> IO ()
reReadConfig startingUp appState = do
  AppConfig{..} <- AppState.getConfig appState
  dbSettings <-
    if configDbConfig then
      loadDbSettings (AppState.getPool appState)
    else
      pure mempty
  readAppConfig dbSettings configFilePath (Just configDbUri) >>= \case
    Left err   ->
      if startingUp then
        panic err -- die on invalid config if the program is starting up
      else
        hPutStrLn stderr $ "Failed loading in-database config. " <> err
    Right newConf -> do
      AppState.putConfig appState newConf
      if startingUp then
        pass
      else
        putStrLn ("In-database config loaded" :: Text)
