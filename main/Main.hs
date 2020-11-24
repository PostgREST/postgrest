{-# LANGUAGE CPP #-}

module Main where

import qualified Data.ByteString            as BS
import qualified Hasql.Connection           as C
import qualified Hasql.Notifications        as N
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction.Sessions as HT

import Control.AutoUpdate       (defaultUpdateSettings, mkAutoUpdate,
                                 updateAction)
import Control.Debounce         (debounceAction, debounceEdge,
                                 debounceFreq,
                                 defaultDebounceSettings, mkDebounce,
                                 trailingEdge)
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
import System.IO                (BufferMode (..), hSetBuffering)

import PostgREST.App         (postgrest)
import PostgREST.Config      (AppConfig (..), CLI (..), Command (..),
                              configPoolTimeout', dumpAppConfig,
                              prettyVersion, readCLIShowHelp,
                              readValidateConfig)
import PostgREST.DbStructure (getDbStructure, getPgVersion)
import PostgREST.Error       (PgError (PgError), checkIsFatal,
                              errorPayload)
import PostgREST.Types       (ConnectionStatus (..), DbStructure,
                              PgVersion (..), minimumPgVersion)
import Protolude             hiding (hPutStrLn, head, toS)
import Protolude.Conv        (toS)


#ifndef mingw32_HOST_OS
import System.Posix.Signals
import UnixSocket
#endif

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

  -- read path from commad line
  opts <- readCLIShowHelp

  -- build the 'AppConfig' from the config file path
  conf <- readValidateConfig $ cliPath opts

  -- dump config and exit if option is set
  when (cliCommand opts == CmdDumpConfig) $ dumpAppConfig conf

  -- These are config values that can't be reloaded at runtime. Reloading some of them would imply restarting the web server.
  let
    host = configHost conf
    port = configPort conf
    maybeSocketAddr = configSocket conf
#ifndef mingw32_HOST_OS
    socketFileMode = configSocketMode conf
#endif
    dbUri = toS (configDbUri conf)
    (dbChannelEnabled, dbChannel) = (configDbChannelEnabled conf, toS $ configDbChannel conf)
    serverSettings =
        setHost ((fromString . toS) host) -- Warp settings
      . setPort port
      . setServerName (toS $ "postgrest/" <> prettyVersion) $
      defaultSettings
    poolSize = configPoolSize conf
    poolTimeout = configPoolTimeout' conf
    logLevel = configLogLevel conf

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

  -- This is passed to the connectionWorker method so it can kill the main thread if the PostgreSQL's version is not supported.
  mainTid <- myThreadId

  let connWorker = connectionWorker mainTid pool refConf refDbStructure refIsWorkerOn (dbChannelEnabled, mvarConnectionStatus)

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
    Catch $ reReadConfig (cliPath opts) refConf
    ) Nothing
#endif

  -- reload schema cache on NOTIFY
  when dbChannelEnabled $
    listener dbUri dbChannel pool refConf refDbStructure mvarConnectionStatus connWorker

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
  3. Obtains the dbStructure.
-}
connectionWorker
  :: ThreadId                      -- ^ Main thread id. Killed if pg version is unsupported
  -> P.Pool                        -- ^ The pg connection pool
  -> IORef AppConfig               -- ^ mutable reference to AppConfig
  -> IORef (Maybe DbStructure)     -- ^ mutable reference to 'DbStructure'
  -> IORef Bool                    -- ^ Used as a binary Semaphore
  -> (Bool, MVar ConnectionStatus) -- ^ For interacting with the LISTEN channel
  -> IO ()
connectionWorker mainTid pool refConf refDbStructure refIsWorkerOn (dbChannelEnabled, mvarConnectionStatus) = do
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
          fillSchemaCache pool actualPgVersion refConf refDbStructure
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

-- | Fill the DbStructure by using a connection from the pool
fillSchemaCache :: P.Pool -> PgVersion -> IORef AppConfig -> IORef (Maybe DbStructure) -> IO ()
fillSchemaCache pool actualPgVersion refConf refDbStructure = do
  conf <- readIORef refConf
  result <- P.use pool $ HT.transaction HT.ReadCommitted HT.Read $ getDbStructure (toList $ configSchemas conf) (configExtraSearchPath conf) actualPgVersion (configDbPrepared conf)
  case result of
    Left e -> do
      -- If this error happens it would mean the connection is down again. Improbable because connectionStatus ensured the connection.
      -- It's not a problem though, because App.postgrest would retry the connectionWorker or the user can do a SIGSUR1 again.
      hPutStrLn stderr . toS . errorPayload $ PgError False e
      putStrLn ("Failed to load the schema cache" :: Text)

    Right dbStructure -> do
      atomicWriteIORef refDbStructure $ Just dbStructure
      putStrLn ("Schema cache loaded" :: Text)

{-|
  Starts a dedicated pg connection to LISTEN for notifications.
  When a NOTIFY <db-channel> - with an empty payload - is done, it refills the schema cache.
  It uses the connectionWorker in case the LISTEN connection dies.
-}
listener :: ByteString -> Text -> P.Pool -> IORef AppConfig -> IORef (Maybe DbStructure) -> MVar ConnectionStatus -> IO () -> IO ()
listener dbUri dbChannel pool refConf refDbStructure mvarConnectionStatus connWorker = start
  where
    start = do
      connStatus <- takeMVar mvarConnectionStatus -- takeMVar makes the thread wait if the MVar is empty(until there's a connection).
      case connStatus of
        Connected actualPgVersion -> void $ forkFinally (do -- forkFinally allows to detect if the thread dies
          dbOrError <- C.acquire dbUri
          -- Debounce in case too many NOTIFYs arrive. Could happen on a migration(assuming a pg EVENT TRIGGER is set up).
          scFiller <- mkDebounce (defaultDebounceSettings {
                        debounceAction = fillSchemaCache pool actualPgVersion refConf refDbStructure,
                        debounceEdge = trailingEdge, -- wait until the function hasn’t been called in _1s
                        debounceFreq = _1s })
          case dbOrError of
            Right db -> do
              putStrLn $ "Listening for notifications on the " <> dbChannel <> " channel"
              let channelToListen = N.toPgIdentifier dbChannel
              N.listen db channelToListen
              N.waitForNotifications (\_ msg ->
                if BS.null msg
                  then scFiller    -- reload the schema cache
                  else pure ()) db -- Do nothing if anything else than an empty message is sent
            _ -> die errorMessage)
          (\_ -> do -- if the thread dies, we try to recover
            putStrLn retryMessage
            connWorker -- assume the pool connection was also lost, call the connection worker
            start)     -- retry the listener
        _ ->
          putStrLn errorMessage -- Should be unreachable. connectionStatus will retry until there's a connection.
    errorMessage = "Could not listen for notifications on the " <> dbChannel <> " channel" :: Text
    retryMessage = "Retrying listening for notifications on the " <> dbChannel <> " channel.." :: Text

-- | Re-reads the config at runtime. Invoked on SIGUSR2.
-- | If it panics(config path was changed, invalid setting), it'll show an error but won't kill the main thread.
reReadConfig :: FilePath -> IORef AppConfig -> IO ()
reReadConfig path refConf = do
  conf <- readValidateConfig path
  atomicWriteIORef refConf conf
  putStrLn ("Config file reloaded" :: Text)

-- Utilitarian functions.
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pass

whenNothing :: Applicative f => Maybe a -> f () -> f ()
whenNothing Nothing f = f
whenNothing _       _ = pass
