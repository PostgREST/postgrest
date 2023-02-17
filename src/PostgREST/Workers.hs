{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Workers
  ( connectionWorker
  , reReadConfig
  , runListener
  , runAdmin
  ) where

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Hasql.Notifications        as SQL
import qualified Hasql.Session              as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.HTTP.Types.Status  as HTTP
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp

import Control.Retry    (RetryStatus, capDelay, exponentialBackoff,
                         retrying, rsPreviousDelay)
import Hasql.Connection (acquire)

import Network.Socket
import Network.Socket.ByteString

import PostgREST.AppState         (AppState)
import PostgREST.Config           (AppConfig (..), readAppConfig)
import PostgREST.Config.Database  (queryDbSettings, queryPgVersion)
import PostgREST.Config.PgVersion (PgVersion (..), minimumPgVersion)
import PostgREST.Error            (checkIsFatal)
import PostgREST.SchemaCache      (querySchemaCache)

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
-- up-to-date schema cache(SchemaCache).  This method is meant to be called
-- multiple times by the same thread, but does nothing if the previous
-- invocation has not terminated. In all cases this method does not halt the
-- calling thread, the work is performed in a separate thread.
--
-- Background thread that does the following :
--  1. Tries to connect to pg server and will keep trying until success.
--  2. Checks if the pg version is supported and if it's not it kills the main
--     program.
--  3. Obtains the sCache. If this fails, it goes back to 1.
connectionWorker :: AppState -> IO ()
connectionWorker appState = do
  runExclusively (AppState.getWorkerSem appState) work
  -- Prevents multiple workers to be running at the same time. Could happen on
  -- too many SIGUSR1s.
  where
    runExclusively mvar action = mask_ $ do
      success <- tryPutMVar mvar ()
      when success $ do
        void $ forkIO $ action `finally` takeMVar mvar
    work = do
      AppConfig{..} <- AppState.getConfig appState
      AppState.logWithZTime appState "Attempting to connect to the database..."
      connected <- establishConnection appState
      case connected of
        FatalConnectionError reason ->
          -- Fatal error when connecting
          AppState.logWithZTime appState reason >> killThread (AppState.getMainThreadId appState)
        NotConnected ->
          -- Unreachable because establishConnection will keep trying to connect
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

-- | Repeatedly flush the pool, and check if a connection from the
-- pool allows access to the PostgreSQL database.
--
-- Releasing the pool is key for rapid recovery. Otherwise, the pool
-- timeout would have to be reached for new healthy connections to be acquired.
-- Which might not happen if the server is busy with requests. No idle
-- connection, no pool timeout.
--
-- The connection tries are capped, but if the connection times out no error is
-- thrown, just 'False' is returned.
establishConnection :: AppState -> IO ConnectionStatus
establishConnection appState =
  retrying retrySettings shouldRetry $
    const $ AppState.flushPool appState >> getConnectionStatus
  where
    retrySettings = capDelay delayMicroseconds $ exponentialBackoff backoffMicroseconds
    delayMicroseconds = 32000000 -- 32 seconds
    backoffMicroseconds = 1000000 -- 1 second

    getConnectionStatus :: IO ConnectionStatus
    getConnectionStatus = do
      pgVersion <- AppState.usePool appState queryPgVersion
      case pgVersion of
        Left e -> do
          AppState.logPgrstError appState e
          case checkIsFatal e of
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

-- | Load the SchemaCache by using a connection from the pool.
loadSchemaCache :: AppState -> IO SCacheStatus
loadSchemaCache appState = do
  AppConfig{..} <- AppState.getConfig appState
  result <-
    let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
    AppState.usePool appState . transaction SQL.ReadCommitted SQL.Read $
      querySchemaCache (toList configDbSchemas) configDbExtraSearchPath configDbPreparedStatements
  case result of
    Left e -> do
      case checkIsFatal e of
        Just hint -> do
          AppState.logWithZTime appState "A fatal error ocurred when loading the schema cache"
          AppState.logPgrstError appState e
          AppState.logWithZTime appState hint
          return SCFatalFail
        Nothing -> do
          AppState.putSchemaCache appState Nothing
          AppState.logWithZTime appState "An error ocurred when loading the schema cache"
          AppState.logPgrstError appState e
          return SCOnRetry

    Right sCache -> do
      AppState.putSchemaCache appState (Just sCache)
      when (isJust configDbRootSpec) .
        AppState.putJsonDbS appState . LBS.toStrict $ JSON.encode sCache
      AppState.logWithZTime appState "Schema cache loaded"
      return SCLoaded

runListener :: AppConfig -> AppState -> IO ()
runListener AppConfig{configDbChannelEnabled} appState =
  when configDbChannelEnabled $ listener appState

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
      | BS.null msg            = cacheReloader
      | msg == "reload schema" = cacheReloader
      | msg == "reload config" = reReadConfig False appState
      | otherwise              = pure () -- Do nothing if anything else than an empty message is sent

    cacheReloader =
      -- reloads the schema cache + restarts pool connections
      -- it's necessary to restart the pg connections because they cache the pg catalog(see #2620)
      connectionWorker appState

-- | Re-reads the config plus config options from the db
reReadConfig :: Bool -> AppState -> IO ()
reReadConfig startingUp appState = do
  AppConfig{..} <- AppState.getConfig appState
  dbSettings <-
    if configDbConfig then do
      qDbSettings <- AppState.usePool appState $ queryDbSettings configDbPreparedStatements
      case qDbSettings of
        Left e -> do
          AppState.logWithZTime appState
            "An error ocurred when trying to query database settings for the config parameters"
          case checkIsFatal e of
            Just hint -> do
              AppState.logPgrstError appState e
              AppState.logWithZTime appState hint
              killThread (AppState.getMainThreadId appState)
            Nothing -> do
              AppState.logPgrstError appState e
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

runAdmin :: AppConfig -> AppState -> Warp.Settings -> IO ()
runAdmin conf@AppConfig{configAdminServerPort} appState settings =
  whenJust configAdminServerPort $ \adminPort -> do
    AppState.logWithZTime appState $ "Admin server listening on port " <> show adminPort
    void . forkIO $ Warp.runSettings (settings & Warp.setPort adminPort) adminApp
  where
    whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
    whenJust mg f = maybe (pure ()) f mg
    adminApp = admin appState conf

-- | PostgREST admin application
admin :: AppState.AppState -> AppConfig -> Wai.Application
admin appState appConfig req respond  = do
  isMainAppReachable  <- any isRight <$> reachMainApp appConfig
  isSchemaCacheLoaded <- isJust <$> AppState.getSchemaCache appState
  isConnectionUp      <-
    if configDbChannelEnabled appConfig
      then AppState.getIsListenerOn appState
      else isRight <$> AppState.usePool appState (SQL.sql "SELECT 1")

  case Wai.pathInfo req of
    ["ready"] ->
      respond $ Wai.responseLBS (if isMainAppReachable && isConnectionUp && isSchemaCacheLoaded then HTTP.status200 else HTTP.status503) [] mempty
    ["live"] ->
      respond $ Wai.responseLBS (if isMainAppReachable then HTTP.status200 else HTTP.status503) [] mempty
    _ ->
      respond $ Wai.responseLBS HTTP.status404 [] mempty

-- Try to connect to the main app socket
-- Note that it doesn't even send a valid HTTP request, we just want to check that the main app is accepting connections
-- The code for resolving the "*4", "!4", "*6", "!6", "*" special values is taken from
-- https://hackage.haskell.org/package/streaming-commons-0.2.2.4/docs/src/Data.Streaming.Network.html#bindPortGenEx
reachMainApp :: AppConfig -> IO [Either IOException ()]
reachMainApp AppConfig{..} =
  case configServerUnixSocket of
    Just path ->  do
      sock <- socket AF_UNIX Stream 0
      (:[]) <$> try (do
        connect sock $ SockAddrUnix path
        withSocketsDo $ bracket (pure sock) close sendEmpty)
    Nothing -> do
      let
        host | configServerHost `elem` ["*4", "!4", "*6", "!6", "*"] = Nothing
             | otherwise                                             = Just configServerHost
        filterAddrs xs =
          case configServerHost of
              "*4" -> ipv4Addrs xs ++ ipv6Addrs xs
              "!4" -> ipv4Addrs xs
              "*6" -> ipv6Addrs xs ++ ipv4Addrs xs
              "!6" -> ipv6Addrs xs
              _    -> xs
        ipv4Addrs = filter ((/=) AF_INET6 . addrFamily)
        ipv6Addrs = filter ((==) AF_INET6 . addrFamily)

      addrs <- getAddrInfo (Just $ defaultHints { addrSocketType = Stream }) (T.unpack <$> host) (Just . show $ configServerPort)
      tryAddr `traverse` filterAddrs addrs
  where
    sendEmpty sock = void $ send sock mempty
    tryAddr :: AddrInfo -> IO (Either IOException ())
    tryAddr addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      try $ do
        connect sock $ addrAddress addr
        withSocketsDo $ bracket (pure sock) close sendEmpty
