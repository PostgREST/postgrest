{-# LANGUAGE CPP            #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Hasql.Connection           as C
import qualified Hasql.Notifications        as N
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction          as HT
import qualified Hasql.Transaction.Sessions as HT

import Control.AutoUpdate       (defaultUpdateSettings, mkAutoUpdate,
                                 updateAction)
import Control.Retry            (RetryStatus, capDelay,
                                 exponentialBackoff, retrying,
                                 rsPreviousDelay)
import Data.IORef               (IORef, atomicWriteIORef, newIORef,
                                 readIORef)
import Data.String              (IsString (..))
import Data.Text.IO             (hPutStrLn)
import Data.Time.Clock          (getCurrentTime)
import Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                 setHost, setPort, setServerName)
import System.CPUTime           (getCPUTime)
import System.IO                (BufferMode (..), hSetBuffering)
import Text.Printf              (hPrintf)

import PostgREST.App         (postgrest)
import PostgREST.Config
import PostgREST.DbStructure (DbStructure, getDbStructure,
                              getPgVersion)
import PostgREST.Error       (PgError (PgError), checkIsFatal,
                              errorPayload)
import PostgREST.PgVersions  (PgVersion (..), minimumPgVersion)
import PostgREST.Statements  (dbSettingsStatement)
import Protolude             hiding (hPutStrLn, head, toS)
import Protolude.Conv        (toS)


#ifndef mingw32_HOST_OS
import System.Posix.Signals
import UnixSocket
#endif


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

-- | This is where everything starts.
main :: IO ()
main = do
  --
  -- LineBuffering: the entire output buffer is flushed whenever a newline is
  -- output, the buffer overflows, a hFlush is issued or the handle is closed
  --
  -- NoBuffering: output is written immediately and never stored in the buffer
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  hSetBuffering stderr NoBuffering

  -- read PGRST_ env variables
  env <- readEnvironment

  -- read command/path from commad line
  CLI{cliCommand, cliPath} <- readCLIShowHelp env

  -- build the 'AppConfig' from the config file path and env vars
  pathEnvConf <- either panic identity <$> readAppConfig mempty env cliPath Nothing Nothing

  -- read external files
  dbUriFile  <- readDbUriFile $ configDbUri pathEnvConf
  secretFile <- readSecretFile $ configJwtSecret pathEnvConf

  -- add the external files to AppConfig
  conf <- either panic identity <$> readAppConfig mempty env cliPath dbUriFile secretFile

  -- These are config values that can't be reloaded at runtime. Reloading some of them would imply restarting the web server.
  let
    host = configServerHost conf
    port = configServerPort conf
    maybeSocketAddr = configServerUnixSocket conf
#ifndef mingw32_HOST_OS
    socketFileMode = configServerUnixSocketMode conf
#endif
    dbUri = toS (configDbUri conf)
    (dbChannelEnabled, dbChannel) = (configDbChannelEnabled conf, toS $ configDbChannel conf)
    serverSettings =
        setHost ((fromString . toS) host) -- Warp settings
      . setPort port
      . setServerName (toS $ "postgrest/" <> prettyVersion) $
      defaultSettings
    poolSize = configDbPoolSize conf
    poolTimeout = configDbPoolTimeout' conf
    logLevel = configLogLevel conf
    dbConfigEnabled = configDbConfig conf

  -- create connection pool with the provided settings, returns either a 'Connection' or a 'ConnectionError'. Does not throw.
  pool <- P.acquire (poolSize, poolTimeout, dbUri)

  -- Used to sync the listener(NOTIFY reload) with the connectionWorker. No connection for the listener at first. Only used if dbChannelEnabled=true.
  mvarConnectionStatus <- newEmptyMVar

  -- No schema cache at the start. Will be filled in by the connectionWorker
  refDbStructure <- newIORef Nothing

  -- Helper ref to make sure just one connectionWorker can run at a time
  refIsWorkerOn <- newIORef False

  -- Config that can change at runtime
  refConf <- newIORef conf

  let
    -- re-reads config file + db config
    dbConfigReReader startingUp = when dbConfigEnabled $
      reReadConfig startingUp pool dbConfigEnabled env cliPath refConf dbUriFile secretFile
    -- re-reads jwt-secret external file + config file + db config
    fullConfigReReader =
      reReadConfig False pool dbConfigEnabled env cliPath refConf
        dbUriFile =<< -- db-uri external file could be re-read, but it doesn't make sense as db-uri is not reloadable
        readSecretFile (configJwtSecret pathEnvConf)

  -- Override the config with config options from the db
  -- TODO: the same operation is repeated on connectionWorker, ideally this would be done only once, but dump CmdDumpConfig needs it for tests.
  dbConfigReReader True

  case cliCommand of
    CmdDumpConfig ->
      do
        dumpedConfig <- dumpAppConfig <$> readIORef refConf
        putStr dumpedConfig
        exitSuccess
    CmdDumpSchema ->
      do
        dumpedSchema <- dumpSchema pool =<< readIORef refConf
        putStrLn dumpedSchema
        exitSuccess
    CmdRun ->
      pass

  -- This is passed to the connectionWorker method so it can kill the main thread if the PostgreSQL's version is not supported.
  mainTid <- myThreadId

  let connWorker = connectionWorker mainTid pool refConf refDbStructure refIsWorkerOn (dbChannelEnabled, mvarConnectionStatus) $
                     dbConfigReReader False

  -- Sets the initial refDbStructure
  connWorker

#ifndef mingw32_HOST_OS
  -- Only for systems with signals:
  --
  -- releases the connection pool whenever the program is terminated,
  -- see https://github.com/PostgREST/postgrest/issues/268
  forM_ [sigINT, sigTERM] $ \sig ->
    void $ installHandler sig (Catch $ do
        P.release pool
        throwTo mainTid UserInterrupt
      ) Nothing

  -- The SIGUSR1 signal updates the internal 'DbStructure' by running 'connectionWorker' exactly as before.
  void $ installHandler sigUSR1 (
    Catch connWorker
    ) Nothing

  -- Re-read the config on SIGUSR2
  void $ installHandler sigUSR2 (
    Catch fullConfigReReader
    ) Nothing
#endif

  -- reload schema cache + config on NOTIFY
  when dbChannelEnabled $
    listener dbUri dbChannel pool refConf refDbStructure mvarConnectionStatus connWorker fullConfigReReader

  -- ask for the OS time at most once per second
  getTime <- mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}

  let postgrestApplication =
        postgrest
          logLevel
          refConf
          refDbStructure
          pool
          getTime
          connWorker

#ifndef mingw32_HOST_OS
  -- run the postgrest application with user defined socket. Only for UNIX systems.
  whenJust maybeSocketAddr $
    runAppInSocket serverSettings postgrestApplication socketFileMode
#endif

  -- run the postgrest application
  whenNothing maybeSocketAddr $ do
    putStrLn $ ("Listening on port " :: Text) <> show port
    runSettings serverSettings postgrestApplication


-- Time constants
_32s :: Int
_32s = 32000000 :: Int -- 32 seconds

_1s :: Int
_1s  = 1000000  :: Int -- 1 second

{-|
  The purpose of this worker is to obtain a healthy connection to pg and an up-to-date schema cache(DbStructure).
  This method is meant to be called by multiple times by the same thread, but does nothing if
  the previous invocation has not terminated. In all cases this method does not
  halt the calling thread, the work is preformed in a separate thread.

  Note: 'atomicWriteIORef' is essentially a lazy semaphore that prevents two
  threads from running 'connectionWorker' at the same time.

  Background thread that does the following :
  1. Tries to connect to pg server and will keep trying until success.
  2. Checks if the pg version is supported and if it's not it kills the main
     program.
  3. Obtains the dbStructure. If this fails, it goes back to 1.
-}
connectionWorker
  :: ThreadId                      -- ^ Main thread id. Killed if pg version is unsupported
  -> P.Pool                        -- ^ The pg connection pool
  -> IORef AppConfig               -- ^ mutable reference to AppConfig
  -> IORef (Maybe DbStructure)     -- ^ mutable reference to 'DbStructure'
  -> IORef Bool                    -- ^ Used as a binary Semaphore
  -> (Bool, MVar ConnectionStatus) -- ^ For interacting with the LISTEN channel
  -> IO ()
  -> IO ()
connectionWorker mainTid pool refConf refDbStructure refIsWorkerOn (dbChannelEnabled, mvarConnectionStatus) dbCfReader = do
  isWorkerOn <- readIORef refIsWorkerOn
  unless isWorkerOn $ do -- Prevents multiple workers to be running at the same time. Could happen on too many SIGUSR1s.
    atomicWriteIORef refIsWorkerOn True
    void $ forkIO work
  where
    work = do
      putStrLn ("Attempting to connect to the database..." :: Text)
      connected <- connectionStatus pool
      when dbChannelEnabled $
        void $ tryPutMVar mvarConnectionStatus connected -- tryPutMVar doesn't lock the thread. It should always succeed since the worker is the only mvar producer.
      case connected of
        FatalConnectionError reason -> hPutStrLn stderr reason >> killThread mainTid -- Fatal error when connecting
        NotConnected                -> return ()                                     -- Unreachable because connectionStatus will keep trying to connect
        Connected actualPgVersion   -> do                                            -- Procede with initialization
          putStrLn ("Connection successful" :: Text)
          dbCfReader -- this could be fail because the connection drops, but the loadSchemaCache will pick the error and retry again
          scStatus <- loadSchemaCache pool actualPgVersion refConf refDbStructure
          case scStatus of
            SCLoaded    -> pure ()            -- do nothing and proceed if the load was successful
            SCOnRetry   -> work               -- retry
            SCFatalFail -> killThread mainTid -- die if our schema cache query has an error
          liftIO $ atomicWriteIORef refIsWorkerOn False

{-|
  Check if a connection from the pool allows access to the PostgreSQL database.
  If not, the pool connections are released and a new connection is tried.
  Releasing the pool is key for rapid recovery. Otherwise, the pool timeout would have to be reached for new healthy connections to be acquired.
  Which might not happen if the server is busy with requests. No idle connection, no pool timeout.

  The connection tries are capped, but if the connection times out no error is thrown, just 'False' is returned.
-}
connectionStatus :: P.Pool -> IO ConnectionStatus
connectionStatus pool =
  retrying (capDelay _32s $ exponentialBackoff _1s)
           shouldRetry
           (const $ P.release pool >> getConnectionStatus)
  where
    getConnectionStatus :: IO ConnectionStatus
    getConnectionStatus = do
      pgVersion <- P.use pool getPgVersion
      case pgVersion of
        Left e -> do
          let err = PgError False e
          hPutStrLn stderr . toS $ errorPayload err
          case checkIsFatal err of
            Just reason -> return $ FatalConnectionError reason
            Nothing     -> return NotConnected

        Right version ->
          if version < minimumPgVersion
             then return . FatalConnectionError $ "Cannot run in this PostgreSQL version, PostgREST needs at least " <> pgvName minimumPgVersion
             else return . Connected  $ version

    shouldRetry :: RetryStatus -> ConnectionStatus -> IO Bool
    shouldRetry rs isConnSucc = do
      let delay    = fromMaybe 0 (rsPreviousDelay rs) `div` _1s
          itShould = NotConnected == isConnSucc
      when itShould $
        putStrLn $ "Attempting to reconnect to the database in " <> (show delay::Text) <> " seconds..."
      return itShould

-- | Load the DbStructure by using a connection from the pool.
loadSchemaCache :: P.Pool -> PgVersion -> IORef AppConfig -> IORef (Maybe DbStructure) -> IO SCacheStatus
loadSchemaCache pool actualPgVersion refConf refDbStructure = do
  conf <- readIORef refConf
  result <- P.use pool $ HT.transaction HT.ReadCommitted HT.Read $ getDbStructure (toList $ configDbSchemas conf) (configDbExtraSearchPath conf) actualPgVersion (configDbPreparedStatements conf)
  case result of
    Left e -> do
      let err = PgError False e
          putErr = hPutStrLn stderr . toS . errorPayload $ err
      case checkIsFatal err of
        Just _  -> do
          hPutStrLn stderr ("A fatal error ocurred when loading the schema cache" :: Text)
          putErr
          hPutStrLn stderr ("This is probably a bug in PostgREST, please report it at https://github.com/PostgREST/postgrest/issues" :: Text)
          return SCFatalFail
        Nothing -> do
          hPutStrLn stderr ("An error ocurred when loading the schema cache" :: Text) >> putErr
          return SCOnRetry

    Right dbStructure -> do
      atomicWriteIORef refDbStructure $ Just dbStructure
      putStrLn ("Schema cache loaded" :: Text)
      return SCLoaded

{-|
  Starts a dedicated pg connection to LISTEN for notifications.
  When a NOTIFY <db-channel> - with an empty payload - is done, it refills the schema cache.
  It uses the connectionWorker in case the LISTEN connection dies.
-}
listener :: ByteString -> Text -> P.Pool -> IORef AppConfig -> IORef (Maybe DbStructure) -> MVar ConnectionStatus -> IO () -> IO () -> IO ()
listener dbUri dbChannel pool refConf refDbStructure mvarConnectionStatus connWorker configLoader = start
  where
    start = do
      connStatus <- takeMVar mvarConnectionStatus -- takeMVar makes the thread wait if the MVar is empty(until there's a connection).
      case connStatus of
        Connected actualPgVersion -> void $ forkFinally (do -- forkFinally allows to detect if the thread dies
          dbOrError <- C.acquire dbUri
          case dbOrError of
            Right db -> do
              putStrLn $ "Listening for notifications on the " <> dbChannel <> " channel"
              let channelToListen = N.toPgIdentifier dbChannel
                  scLoader = void $ loadSchemaCache pool actualPgVersion refConf refDbStructure -- It's not necessary to check the loadSchemaCache success here. If the connection drops, the thread will die and proceed to recover below.
              N.listen db channelToListen
              N.waitForNotifications (\_ msg ->
                if | BS.null msg            -> scLoader      -- reload the schema cache
                   | msg == "reload schema" -> scLoader      -- reload the schema cache
                   | msg == "reload config" -> configLoader  -- reload the config
                   | otherwise              -> pure ()       -- Do nothing if anything else than an empty message is sent
                ) db
            _ -> die errorMessage)
          (\_ -> do -- if the thread dies, we try to recover
            putStrLn retryMessage
            connWorker -- assume the pool connection was also lost, call the connection worker
            start)     -- retry the listener
        _ ->
          putStrLn errorMessage -- Should be unreachable. connectionStatus will retry until there's a connection.
    errorMessage = "Could not listen for notifications on the " <> dbChannel <> " channel" :: Text
    retryMessage = "Retrying listening for notifications on the " <> dbChannel <> " channel.." :: Text

-- | Re-reads the config plus config options from the db
reReadConfig :: Bool -> P.Pool -> Bool -> Environment -> Maybe FilePath -> IORef AppConfig -> Maybe Text -> Maybe BS.ByteString -> IO ()
reReadConfig startingUp pool dbConfigEnabled env path refConf dbUriFile secretFile = do
  dbSettings <- if dbConfigEnabled then loadDbSettings else pure []
  readAppConfig dbSettings env path dbUriFile secretFile >>= \case
    Left err   ->
      if startingUp
        then panic err -- die on invalid config if the program is starting up
        else hPutStrLn stderr $ "Failed loading in-database config. " <> err
    Right conf -> do
      atomicWriteIORef refConf conf
      if startingUp
        then pass
        else putStrLn ("In-database config loaded" :: Text)
  where
    loadDbSettings :: IO [(Text, Text)]
    loadDbSettings = do
      result <- P.use pool $ HT.transaction HT.ReadCommitted HT.Read $ HT.statement mempty dbSettingsStatement
      case result of
        Left  e -> do
          hPutStrLn stderr ("An error ocurred when trying to query database settings for the config parameters:\n" <> show e :: Text)
          pure []
        Right x -> pure x

-- | Dump DbStructure schema to JSON
dumpSchema :: P.Pool -> AppConfig -> IO LBS.ByteString
dumpSchema pool conf = do
  result <-
    timeToStderr "Loaded schema in %.3f seconds" $
      P.use pool $ do
        pgVersion <- getPgVersion
        HT.transaction HT.ReadCommitted HT.Read $
          getDbStructure
            (toList $ configDbSchemas conf)
            (configDbExtraSearchPath conf)
            pgVersion
            (configDbPreparedStatements conf)
  P.release pool
  case result of
    Left e -> do
      hPutStrLn stderr $ "An error ocurred when loading the schema cache:\n" <> show e
      exitFailure
    Right dbStructure -> return $ Aeson.encode dbStructure

-- | Print the time taken to run an IO action to stderr with the given printf string
timeToStderr :: [Char] -> IO (Either a b) -> IO (Either a b)
timeToStderr fmtString a =
  do
    start <- getCPUTime
    result <- a
    end <- getCPUTime
    let
      duration :: Double
      duration = fromIntegral (end - start) / picoseconds
    when (isRight result) $
      hPrintf stderr (fmtString ++ "\n") duration
    return result


-- | 10^12 picoseconds per second
picoseconds :: Double
picoseconds = 1000000000000


-- Utility functions.
#ifndef mingw32_HOST_OS
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pass
#endif

whenNothing :: Applicative f => Maybe a -> f () -> f ()
whenNothing Nothing f = f
whenNothing _       _ = pass
