{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.AppState
  ( AppState
  , SAML2State(..)
  , AuthResult(..)
  , stateSAML2
  , destroy
  , getConfig
  , getSchemaCache
  , getIsListenerOn
  , getMainThreadId
  , getPgVersion
  , getRetryNextIn
  , getTime
  , getJwtCache
  , getSocketREST
  , getSocketAdmin
  , init
  , initSockets
  , initWithPool
  , logWithZTime
  , putSchemaCache
  , putPgVersion
  , usePool
  , loadSchemaCache
  , reReadConfig
  , connectionWorker
  , runListener
  ) where

import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.KeyMap          as KM
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Cache                 as C
import           Data.Either.Combinators    (whenLeft)
import qualified Data.Text                  as T (unpack)
import qualified Data.Text.Encoding         as T
import           Hasql.Connection           (acquire)
import qualified Hasql.Notifications        as SQL
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Session              as SQL
import qualified Hasql.Transaction.Sessions as SQL
import qualified Network.HTTP.Types.Status  as HTTP
import qualified Network.Socket             as NS
import qualified PostgREST.Error            as Error
import           PostgREST.Version          (prettyVersion)

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)

import Crypto.PubKey.RSA.Types (PublicKey (..))
import Control.Debounce
import Control.Retry      (RetryStatus, capDelay, exponentialBackoff,
                           retrying, rsPreviousDelay)
import Data.IORef         (IORef, atomicWriteIORef, newIORef,
                           readIORef)
import Data.Time          (ZonedTime, defaultTimeLocale, formatTime,
                           getZonedTime)
import Data.Time.Clock    (UTCTime, getCurrentTime)

import Network.Wai.SAML2  (SAML2Config (..), saml2ConfigNoEncryption)

import PostgREST.Config                  (AppConfig (..),
                                          LogLevel (..),
                                          addFallbackAppName,
                                          readAppConfig)
import PostgREST.Config.Database         (queryDbSettings,
                                          queryPgVersion,
                                          queryRoleSettings)
import PostgREST.Config.PgVersion        (PgVersion (..),
                                          minimumPgVersion)
import PostgREST.SchemaCache             (SchemaCache,
                                          querySchemaCache)
import PostgREST.SchemaCache.Identifiers (dumpQi)
import PostgREST.Unix                    (createAndBindDomainSocket)

import Data.Streaming.Network (bindPortTCP, bindRandomPortTCP)
import Data.String            (IsString (..))
import Protolude

data AuthResult = AuthResult
  { authClaims :: KM.KeyMap JSON.Value
  , authRole   :: BS.ByteString
  }

data AppState = AppState
  -- | Database connection pool
  { statePool                     :: SQL.Pool
  -- | Database server version, will be updated by the connectionWorker
  , statePgVersion                :: IORef PgVersion
  -- | No schema cache at the start. Will be filled in by the connectionWorker
  , stateSchemaCache              :: IORef (Maybe SchemaCache)
  -- | starts the connection worker with a debounce
  , debouncedConnectionWorker     :: IO ()
  -- | Binary semaphore used to sync the listener(NOTIFY reload) with the connectionWorker.
  , stateListener                 :: MVar ()
  -- | State of the LISTEN channel, used for the admin server checks
  , stateIsListenerOn             :: IORef Bool
  -- | Config that can change at runtime
  , stateConf                     :: IORef AppConfig
  -- | Time used for verifying JWT expiration
  , stateGetTime                  :: IO UTCTime
  -- | Time with time zone used for worker logs
  , stateGetZTime                 :: IO ZonedTime
  -- | Used for killing the main thread in case a subthread fails
  , stateMainThreadId             :: ThreadId
  -- | Keeps track of when the next retry for connecting to database is scheduled
  , stateRetryNextIn              :: IORef Int
  -- | Logs a pool error with a debounce
  , debounceLogAcquisitionTimeout :: IO ()
  -- | JWT Cache
  , jwtCache                      :: C.Cache ByteString AuthResult
  -- | Network socket for REST API
  , stateSocketREST               :: NS.Socket
  -- | Network socket for the admin UI
  , stateSocketAdmin              :: Maybe NS.Socket
  -- | Options for the SAML authentication middleware
  , stateSAML2                    :: Maybe SAML2State
  }

type AppSockets = (NS.Socket, Maybe NS.Socket)

data SAML2State = SAML2State
  { saml2StateAppConfig :: SAML2Config
  -- | Known assertion IDs, so we can avoid 'replay' attacks.
  , saml2KnownIds       :: C.Cache Text ()
  -- | PostgREST endpoint that points to a Postgres function that generates JWT tokens.
  , saml2JwtEndpoint    :: Text
  }

-- | The default SAML2 parameters.
standardSAML2State :: IO SAML2State
standardSAML2State = do
  knownIds <- C.newCache Nothing :: IO (C.Cache Text ())
  pure $ SAML2State
    { saml2StateAppConfig = (saml2ConfigNoEncryption pubKey)
      { saml2DisableTimeValidation = True }
    , saml2KnownIds    = knownIds
    , saml2JwtEndpoint = "/rpc/login"
    }
  where
    pubKey = PublicKey
      { public_size = 256
      , public_n = 22422774418294161467689281605893099697695348238043024221462308433077554631939140778208843872196057459666733781706329732318701515820436112779102460560259099305916255329626929877997549586841451270776743862120602357029558305888132098945391819549132185888975931559389043395881630665509783899290824553207915569312623093600583991101104654520228295494031084631981134094819019941584685926482312779995332766353128642954180559563564860075114471257488173038109753857397617663192161051780809557088509925550614979729987724491937739323735648092627388826541432994389317410204416927290400855649211463115203541531238127283730363993681
      , public_e = 65537
      }

init :: AppConfig -> IO AppState
init conf = do
  pool <- initPool conf
  (sock, adminSock) <- initSockets conf
  state' <- initWithPool (sock, adminSock) pool conf
  pure state' { stateSocketREST = sock, stateSocketAdmin = adminSock }

initWithPool :: AppSockets -> SQL.Pool -> AppConfig -> IO AppState
initWithPool (sock, adminSock) pool conf = do
  appState <- AppState pool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> pure (pure ())
    <*> newEmptyMVar
    <*> newIORef False
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
    <*> myThreadId
    <*> newIORef 0
    <*> pure (pure ())
    <*> C.newCache Nothing
    <*> pure sock
    <*> pure adminSock
    <*> (Just <$> standardSAML2State)


  debLogTimeout <-
    let oneSecond = 1000000 in
    mkDebounce defaultDebounceSettings
       { debounceAction = logPgrstError appState SQL.AcquisitionTimeoutUsageError
       , debounceFreq = 5*oneSecond
       , debounceEdge = leadingEdge -- logs at the start and the end
       }

  debWorker <-
    let decisecond = 100000 in
    mkDebounce defaultDebounceSettings
       { debounceAction = internalConnectionWorker appState
       , debounceFreq = decisecond
       , debounceEdge = leadingEdge -- runs the worker at the start and the end
       }

  return appState { debounceLogAcquisitionTimeout = debLogTimeout, debouncedConnectionWorker = debWorker }

destroy :: AppState -> IO ()
destroy = destroyPool

initSockets :: AppConfig -> IO AppSockets
initSockets AppConfig{..} = do
  let
    cfg'usp = configServerUnixSocket
    cfg'uspm = configServerUnixSocketMode
    cfg'host = configServerHost
    cfg'port = configServerPort
    cfg'adminport = configAdminServerPort

  sock <- case cfg'usp of
    -- I'm not using `streaming-commons`' bindPath function here because it's not defined for Windows,
    -- but we need to have runtime error if we try to use it in Windows, not compile time error
    Just path -> createAndBindDomainSocket path cfg'uspm
    Nothing -> do
      (_, sock) <-
        if cfg'port /= 0
          then do
            sock <- bindPortTCP cfg'port (fromString $ T.unpack cfg'host)
            pure (cfg'port, sock)
          else do
            -- explicitly bind to a random port, returning bound port number
            (num, sock) <- bindRandomPortTCP (fromString $ T.unpack cfg'host)
            pure (num, sock)
      pure sock

  adminSock <- case cfg'adminport of
    Just adminPort -> do
      adminSock <- bindPortTCP adminPort (fromString $ T.unpack cfg'host)
      pure $ Just adminSock
    Nothing -> pure Nothing

  pure (sock, adminSock)

initPool :: AppConfig -> IO SQL.Pool
initPool AppConfig{..} =
  SQL.acquire
    configDbPoolSize
    (fromIntegral configDbPoolAcquisitionTimeout)
    (fromIntegral configDbPoolMaxLifetime)
    (fromIntegral configDbPoolMaxIdletime)
    (toUtf8 $ addFallbackAppName prettyVersion configDbUri)

-- | Run an action with a database connection.
usePool :: AppState -> AppConfig -> SQL.Session a -> IO (Either SQL.UsageError a)
usePool appState@AppState{..} AppConfig{configLogLevel} x = do
  res <- SQL.use statePool x

  when (configLogLevel > LogCrit) $ do
    whenLeft res (\case
      SQL.AcquisitionTimeoutUsageError -> debounceLogAcquisitionTimeout -- this can happen rapidly for many requests, so we debounce
      error
        -- TODO We're using the 500 HTTP status for getting all internal db errors but there's no response here. We need a new intermediate type to not rely on the HTTP status.
        | Error.status (Error.PgError False error) >= HTTP.status500 -> logPgrstError appState error
        | otherwise -> pure ())

  return res

-- | Flush the connection pool so that any future use of the pool will
-- use connections freshly established after this call.
flushPool :: AppState -> IO ()
flushPool AppState{..} = SQL.release statePool

-- | Destroy the pool on shutdown.
destroyPool :: AppState -> IO ()
destroyPool AppState{..} = SQL.release statePool

getPgVersion :: AppState -> IO PgVersion
getPgVersion = readIORef . statePgVersion

putPgVersion :: AppState -> PgVersion -> IO ()
putPgVersion = atomicWriteIORef . statePgVersion

getSchemaCache :: AppState -> IO (Maybe SchemaCache)
getSchemaCache = readIORef . stateSchemaCache

putSchemaCache :: AppState -> Maybe SchemaCache -> IO ()
putSchemaCache appState = atomicWriteIORef (stateSchemaCache appState)

connectionWorker :: AppState -> IO ()
connectionWorker = debouncedConnectionWorker

getRetryNextIn :: AppState -> IO Int
getRetryNextIn = readIORef . stateRetryNextIn

putRetryNextIn :: AppState -> Int -> IO ()
putRetryNextIn = atomicWriteIORef . stateRetryNextIn

getConfig :: AppState -> IO AppConfig
getConfig = readIORef . stateConf

putConfig :: AppState -> AppConfig -> IO ()
putConfig = atomicWriteIORef . stateConf

getTime :: AppState -> IO UTCTime
getTime = stateGetTime

getJwtCache :: AppState -> C.Cache ByteString AuthResult
getJwtCache = jwtCache

getSocketREST :: AppState -> NS.Socket
getSocketREST = stateSocketREST

getSocketAdmin :: AppState -> Maybe NS.Socket
getSocketAdmin = stateSocketAdmin

-- | Log to stderr with local time
logWithZTime :: AppState -> Text -> IO ()
logWithZTime appState txt = do
  zTime <- stateGetZTime appState
  hPutStrLn stderr $ toS (formatTime defaultTimeLocale "%d/%b/%Y:%T %z: " zTime) <> txt

logPgrstError :: AppState -> SQL.UsageError -> IO ()
logPgrstError appState e = logWithZTime appState . T.decodeUtf8 . LBS.toStrict $ Error.errorPayload $ Error.PgError False e

getMainThreadId :: AppState -> ThreadId
getMainThreadId = stateMainThreadId

-- | As this IO action uses `takeMVar` internally, it will only return once
-- `stateListener` has been set using `signalListener`. This is currently used
-- to syncronize workers.
waitListener :: AppState -> IO ()
waitListener = takeMVar . stateListener

-- tryPutMVar doesn't lock the thread. It should always succeed since
-- the connectionWorker is the only mvar producer.
signalListener :: AppState -> IO ()
signalListener appState = void $ tryPutMVar (stateListener appState) ()

getIsListenerOn :: AppState -> IO Bool
getIsListenerOn = readIORef . stateIsListenerOn

putIsListenerOn :: AppState -> Bool -> IO ()
putIsListenerOn = atomicWriteIORef . stateIsListenerOn

-- | Schema cache status
data SCacheStatus
  = SCLoaded
  | SCOnRetry
  | SCFatalFail

-- | Load the SchemaCache by using a connection from the pool.
loadSchemaCache :: AppState -> IO SCacheStatus
loadSchemaCache appState = do
  conf@AppConfig{..} <- getConfig appState
  result <-
    let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
    usePool appState conf . transaction SQL.ReadCommitted SQL.Read $
      querySchemaCache conf
  case result of
    Left e -> do
      case checkIsFatal e of
        Just hint -> do
          logWithZTime appState "A fatal error ocurred when loading the schema cache"
          logPgrstError appState e
          logWithZTime appState hint
          return SCFatalFail
        Nothing -> do
          putSchemaCache appState Nothing
          logWithZTime appState "An error ocurred when loading the schema cache"
          logPgrstError appState e
          return SCOnRetry

    Right sCache -> do
      putSchemaCache appState (Just sCache)
      logWithZTime appState "Schema cache loaded"
      return SCLoaded

-- | Current database connection status data ConnectionStatus
data ConnectionStatus
  = NotConnected
  | Connected PgVersion
  | FatalConnectionError Text
  deriving (Eq)

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
internalConnectionWorker :: AppState -> IO ()
internalConnectionWorker appState = work
  where
    work = do
      config@AppConfig{..} <- getConfig appState
      logWithZTime appState $ "Starting PostgREST " <> T.decodeUtf8 prettyVersion <> "..."
      logWithZTime appState "Attempting to connect to the database..."
      connected <- establishConnection appState config
      case connected of
        FatalConnectionError reason ->
          -- Fatal error when connecting
          logWithZTime appState reason >> killThread (getMainThreadId appState)
        NotConnected ->
          -- Unreachable because establishConnection will keep trying to connect, unless disable-recovery is turned on
          unless configDbPoolAutomaticRecovery
              $ logWithZTime appState "Automatic recovery disabled, exiting." >> killThread (getMainThreadId appState)
        Connected actualPgVersion -> do
          -- Procede with initialization
          putPgVersion appState actualPgVersion
          when configDbChannelEnabled $
            signalListener appState
          logWithZTime appState "Connection successful"
          -- this could be fail because the connection drops, but the loadSchemaCache will pick the error and retry again
          -- We cannot retry after it fails immediately, because db-pre-config could have user errors. We just log the error and continue.
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
              killThread $ getMainThreadId appState

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
establishConnection :: AppState -> AppConfig -> IO ConnectionStatus
establishConnection appState config =
  retrying retrySettings shouldRetry $
    const $ flushPool appState >> getConnectionStatus
  where
    retrySettings = capDelay delayMicroseconds $ exponentialBackoff backoffMicroseconds
    delayMicroseconds = 32000000 -- 32 seconds
    backoffMicroseconds = 1000000 -- 1 second

    getConnectionStatus :: IO ConnectionStatus
    getConnectionStatus = do
      pgVersion <- usePool appState config $ queryPgVersion False -- No need to prepare the query here, as the connection might not be established
      case pgVersion of
        Left e -> do
          logPgrstError appState e
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
      AppConfig{..} <- getConfig appState
      let
        delay = fromMaybe 0 (rsPreviousDelay rs) `div` backoffMicroseconds
        itShould = NotConnected == isConnSucc && configDbPoolAutomaticRecovery
      when itShould . logWithZTime appState $
        "Attempting to reconnect to the database in "
        <> (show delay::Text)
        <> " seconds..."
      when itShould $ putRetryNextIn appState delay
      return itShould

-- | Re-reads the config plus config options from the db
reReadConfig :: Bool -> AppState -> IO ()
reReadConfig startingUp appState = do
  config@AppConfig{..} <- getConfig appState
  pgVer <- getPgVersion appState
  dbSettings <-
    if configDbConfig then do
      qDbSettings <- usePool appState config $ queryDbSettings (dumpQi <$> configDbPreConfig) configDbPreparedStatements
      case qDbSettings of
        Left e -> do
          logWithZTime appState
            "An error ocurred when trying to query database settings for the config parameters"
          case checkIsFatal e of
            Just hint -> do
              logPgrstError appState e
              logWithZTime appState hint
              killThread (getMainThreadId appState)
            Nothing -> do
              logPgrstError appState e
          pure mempty
        Right x -> pure x
    else
      pure mempty
  (roleSettings, roleIsolationLvl) <-
    if configDbConfig then do
      rSettings <- usePool appState config $ queryRoleSettings pgVer configDbPreparedStatements
      case rSettings of
        Left e -> do
          logWithZTime appState "An error ocurred when trying to query the role settings"
          logPgrstError appState e
          pure (mempty, mempty)
        Right x -> pure x
    else
      pure mempty
  readAppConfig dbSettings configFilePath (Just configDbUri) roleSettings roleIsolationLvl >>= \case
    Left err   ->
      if startingUp then
        panic err -- die on invalid config if the program is starting up
      else
        logWithZTime appState $ "Failed reloading config: " <> err
    Right newConf -> do
      putConfig appState newConf
      if startingUp then
        pass
      else
        logWithZTime appState "Config reloaded"


runListener :: AppConfig -> AppState -> IO ()
runListener AppConfig{configDbChannelEnabled} appState =
  when configDbChannelEnabled $ listener appState

-- | Starts a dedicated pg connection to LISTEN for notifications.  When a
-- NOTIFY <db-channel> - with an empty payload - is done, it refills the schema
-- cache.  It uses the connectionWorker in case the LISTEN connection dies.
listener :: AppState -> IO ()
listener appState = do
  AppConfig{..} <- getConfig appState
  let dbChannel = toS configDbChannel

  -- The listener has to wait for a signal from the connectionWorker.
  -- This is because when the connection to the db is lost, the listener also
  -- tries to recover the connection, but not with the same pace as the connectionWorker.
  -- Not waiting makes stderr quickly fill with connection retries messages from the listener.
  waitListener appState

  -- forkFinally allows to detect if the thread dies
  void . flip forkFinally (handleFinally dbChannel configDbPoolAutomaticRecovery) $ do
    dbOrError <- acquire $ toUtf8 (addFallbackAppName prettyVersion configDbUri)
    case dbOrError of
      Right db -> do
        logWithZTime appState $ "Listening for notifications on the " <> dbChannel <> " channel"
        putIsListenerOn appState True
        SQL.listen db $ SQL.toPgIdentifier dbChannel
        SQL.waitForNotifications handleNotification db
      _ ->
        die $ "Could not listen for notifications on the " <> dbChannel <> " channel"
  where
    handleFinally _ False _ =
      logWithZTime appState "Automatic recovery disabled, exiting." >> killThread (getMainThreadId appState)
    handleFinally dbChannel True _ = do
      -- if the thread dies, we try to recover
      logWithZTime appState $ "Retrying listening for notifications on the " <> dbChannel <> " channel.."
      putIsListenerOn appState False
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

checkIsFatal :: SQL.UsageError -> Maybe Text
checkIsFatal (SQL.ConnectionUsageError e)
  | isAuthFailureMessage = Just $ toS failureMessage
  | otherwise = Nothing
  where isAuthFailureMessage =
          ("FATAL:  password authentication failed" `isInfixOf` failureMessage) ||
          ("no password supplied" `isInfixOf` failureMessage)
        failureMessage = BS.unpack $ fromMaybe mempty e
checkIsFatal(SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError serverError)))
  = case serverError of
      -- Check for a syntax error (42601 is the pg code). This would mean the error is on our part somehow, so we treat it as fatal.
      SQL.ServerError "42601" _ _ _ _
        -> Just "Hint: This is probably a bug in PostgREST, please report it at https://github.com/PostgREST/postgrest/issues"
      -- Check for a "prepared statement <name> already exists" error (Code 42P05: duplicate_prepared_statement).
      -- This would mean that a connection pooler in transaction mode is being used
      -- while prepared statements are enabled in the PostgREST configuration,
      -- both of which are incompatible with each other.
      SQL.ServerError "42P05" _ _ _ _
        -> Just "Hint: If you are using connection poolers in transaction mode, try setting db-prepared-statements to false."
      -- Check for a "transaction blocks not allowed in statement pooling mode" error (Code 08P01: protocol_violation).
      -- This would mean that a connection pooler in statement mode is being used which is not supported in PostgREST.
      SQL.ServerError "08P01" "transaction blocks not allowed in statement pooling mode" _ _ _
        -> Just "Hint: Connection poolers in statement mode are not supported."
      _ -> Nothing
checkIsFatal _ = Nothing
