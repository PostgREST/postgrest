{-# LANGUAGE CPP #-}

module Main where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as B64
import qualified Hasql.Connection           as C
import qualified Hasql.Notifications        as N
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction.Sessions as HT

import Control.AutoUpdate       (defaultUpdateSettings, mkAutoUpdate,
                                 updateAction)
import Control.Retry            (RetryStatus, capDelay,
                                 exponentialBackoff, retrying,
                                 rsPreviousDelay)
import Data.Either.Combinators  (whenLeft)
import Data.IORef               (IORef, atomicWriteIORef, newIORef,
                                 readIORef)
import Data.String              (IsString (..))
import Data.Text                (pack, replace, strip, stripPrefix)
import Data.Text.IO             (hPutStrLn)
import Data.Time.Clock          (getCurrentTime)
import Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                 setHost, setPort, setServerName)
import System.IO                (BufferMode (..), hSetBuffering)

import PostgREST.App         (postgrest)
import PostgREST.Config      (AppConfig (..), configPoolTimeout',
                              prettyVersion, readOptions)
import PostgREST.DbStructure (getDbStructure, getPgVersion)
import PostgREST.Error       (PgError (PgError), checkIsFatal,
                              errorPayload)
import PostgREST.OpenAPI     (isMalformedProxyUri)
import PostgREST.Types       (ConnectionStatus (..), DbStructure,
                              PgVersion (..), Schema,
                              minimumPgVersion)
import Protolude             hiding (hPutStrLn, head, replace, toS)
import Protolude.Conv        (toS)


#ifndef mingw32_HOST_OS
import System.Posix.Signals
import UnixSocket
#endif


{-|
  The purpose of this worker is to fill the refDbStructure created in 'main'
  with the 'DbStructure' returned from calling 'getDbStructure'. This method
  is meant to be called by multiple times by the same thread, but does nothing if
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
  :: ThreadId -- ^ Main thread id. Killed if pg version is unsupported
  -> P.Pool   -- ^ The PostgreSQL connection pool
  -> [Schema] -- ^ Schemas PostgREST is serving up
  -> IORef (Maybe DbStructure) -- ^ mutable reference to 'DbStructure'
  -> IORef Bool                -- ^ Used as a binary Semaphore
  -> (Bool, MVar ConnectionStatus) -- ^ For interacting with the LISTEN channel
  -> IO ()
connectionWorker mainTid pool schemas refDbStructure refIsWorkerOn (dbChannelEnabled, mvarConnectionStatus) = do
  isWorkerOn <- readIORef refIsWorkerOn
  unless isWorkerOn $ do
    atomicWriteIORef refIsWorkerOn True
    void $ forkIO work
  where
    work = do
      atomicWriteIORef refDbStructure Nothing
      putStrLn ("Attempting to connect to the database..." :: Text)
      connected <- connectionStatus pool
      when dbChannelEnabled $ -- If this is not done, the putMVar will lock the thread since the MVar will never be consumed with a takeMVar
        putMVar mvarConnectionStatus connected
      case connected of
        FatalConnectionError reason -> hPutStrLn stderr reason >> killThread mainTid -- Fatal error when connecting
        NotConnected                -> return ()                                     -- Unreachable because connectionStatus will keep trying to connect
        Connected actualPgVersion   -> do                                            -- Procede with initialization
          putStrLn ("Connection successful" :: Text)
          fillSchemaCache pool actualPgVersion schemas refDbStructure
          liftIO $ atomicWriteIORef refIsWorkerOn False

fillSchemaCache :: P.Pool -> PgVersion -> [Schema] -> IORef (Maybe DbStructure) -> IO ()
fillSchemaCache pool actualPgVersion schemas refDbStructure = do
  void $ P.use pool $ do -- here we assume P.use pool won't fail after connectionStatus succeeded.
    dbStructure <- HT.transaction HT.ReadCommitted HT.Read $ getDbStructure schemas actualPgVersion
    liftIO $ atomicWriteIORef refDbStructure $ Just dbStructure
  putStrLn ("Schema cache loaded" :: Text)

{-|
  Used by 'connectionWorker' to check if the provided db-uri lets
  the application access the PostgreSQL database. This method is used
  the first time the connection is tested, but only to test before
  calling 'getDbStructure' inside the 'connectionWorker' method.

  The connection tries are capped, but if the connection times out no error is
  thrown, just 'False' is returned.
-}
connectionStatus :: P.Pool -> IO ConnectionStatus
connectionStatus pool =
  retrying (capDelay _32s $ exponentialBackoff _1s)
           shouldRetry
           (const $ P.release pool >> getConnectionStatus)
  where
    _32s = 32000000 -- 32 seconds
    _1s  = 1000000  -- 1 second
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

{-|
  Starts a dedicated pg connection to LISTEN for notifications.
  When a NOTIFY channel(with an empty payload) is done, it refills the schema cache.
  It uses the connectionWorker in case the LISTEN connection dies.
-}
listener :: ByteString -> Text -> P.Pool -> [Schema] -> IORef (Maybe DbStructure) -> MVar ConnectionStatus -> IO () -> IO ()
listener dbUri dbChannel pool schemas refDbStructure mvarConnectionStatus connWorker = start
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
              N.listen db channelToListen
              N.waitForNotifications (\_ msg ->
                if BS.null msg
                  then fillSchemaCache pool actualPgVersion schemas refDbStructure -- reload the schema cache
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
  --
  -- readOptions builds the 'AppConfig' from the config file specified on the
  -- command line
  conf <- loadDbUriFile =<< loadSecretFile =<< readOptions
  let schemas = toList $ configSchemas conf
      host = configHost conf
      port = configPort conf
      proxy = configOpenAPIProxyUri conf
      maybeSocketAddr = configSocket conf
      socketFileMode = configSocketMode conf
      dbUri = toS (configDbUri conf)
      (dbChannelEnabled, dbChannel) = (configDbChannelEnabled conf, toS $ configDbChannel conf)
      roleClaimKey = configRoleClaimKey conf
      appSettings =
        setHost ((fromString . toS) host) -- Warp settings
        . setPort port
        . setServerName (toS $ "postgrest/" <> prettyVersion) $
        defaultSettings

  whenLeft socketFileMode panic

  -- Checks that the provided proxy uri is formated correctly
  when (isMalformedProxyUri $ toS <$> proxy) $
    panic
      "Malformed proxy uri, a correct example: https://example.com:8443/basePath"

  -- Checks that the provided jspath is valid
  whenLeft roleClaimKey $
    panic $ show roleClaimKey

  -- create connection pool with the provided settings, returns either
  -- a 'Connection' or a 'ConnectionError'. Does not throw.
  pool <- P.acquire (configPool conf, configPoolTimeout' conf, dbUri)

  -- No connection to the db at first. This is only used if the dbChannelEnabled is true(LISTENer enabled).
  mvarConnectionStatus <- newEmptyMVar

  -- To be filled in by connectionWorker
  refDbStructure <- newIORef Nothing

  -- Helper ref to make sure just one connectionWorker can run at a time
  refIsWorkerOn <- newIORef False

  -- This is passed to the connectionWorker method so it can kill the main
  -- thread if the PostgreSQL's version is not supported.
  mainTid <- myThreadId

  let connWorker = connectionWorker mainTid pool schemas refDbStructure refIsWorkerOn (dbChannelEnabled, mvarConnectionStatus)

  -- Sets the initial refDbStructure
  connWorker

  -- Only for systems with signals:
  --
  -- releases the connection pool whenever the program is terminated,
  -- see https://github.com/PostgREST/postgrest/issues/268
  --
  -- Plus the SIGUSR1 signal updates the internal 'DbStructure' by running
  -- 'connectionWorker' exactly as before.
#ifndef mingw32_HOST_OS
  forM_ [sigINT, sigTERM] $ \sig ->
    void $ installHandler sig (Catch $ do
        P.release pool
        throwTo mainTid UserInterrupt
      ) Nothing

  void $ installHandler sigUSR1 (
    Catch connWorker
    ) Nothing
#endif

  -- run connectionWorker on NOTIFY, in addition to SIGUSR1
  when dbChannelEnabled $
    listener dbUri dbChannel pool schemas refDbStructure mvarConnectionStatus connWorker

  -- ask for the OS time at most once per second
  getTime <- mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}

  let postgrestApplication =
        postgrest
          conf
          refDbStructure
          pool
          getTime
          connWorker

  -- run the postgrest application with user defined socket. Only for UNIX systems.
#ifndef mingw32_HOST_OS
  whenJust maybeSocketAddr $
    runAppInSocket appSettings postgrestApplication socketFileMode
#endif

  -- run the postgrest application
  whenNothing maybeSocketAddr $ do
    putStrLn $ ("Listening on port " :: Text) <> show (configPort conf)
    runSettings appSettings postgrestApplication

{-|
  The purpose of this function is to load the JWT secret from a file if
  configJwtSecret is actually a filepath and replaces some characters if the JWT
  is base64 encoded.

  The reason some characters need to be replaced is because JWT is actually
  base64url encoded which must be turned into just base64 before decoding.

  To check if the JWT secret is provided is in fact a file path, it must be
  decoded as 'Text' to be processed.

  decodeUtf8: Decode a ByteString containing UTF-8 encoded text that is known to
  be valid.
-}
loadSecretFile :: AppConfig -> IO AppConfig
loadSecretFile conf = extractAndTransform mSecret
  where
    mSecret = decodeUtf8 <$> configJwtSecret conf
    isB64 = configJwtSecretIsBase64 conf
    --
    -- The Text (variable name secret) here is mSecret from above which is the JWT
    -- decoded as Utf8
    --
    -- stripPrefix: Return the suffix of the second string if its prefix matches
    -- the entire first string.
    --
    -- The configJwtSecret is a filepath instead of the JWT secret itself if the
    -- secret has @ as its prefix.
    extractAndTransform :: Maybe Text -> IO AppConfig
    extractAndTransform Nothing = return conf
    extractAndTransform (Just secret) =
      fmap setSecret $
      transformString isB64 =<<
      case stripPrefix "@" secret of
        Nothing       -> return . encodeUtf8 $ secret
        Just filename -> chomp <$> BS.readFile (toS filename)
      where
        chomp bs = fromMaybe bs (BS.stripSuffix "\n" bs)
    --
    -- Turns the Base64url encoded JWT into Base64
    transformString :: Bool -> ByteString -> IO ByteString
    transformString False t = return t
    transformString True t =
      case B64.decode $ encodeUtf8 $ strip $ replaceUrlChars $ decodeUtf8 t of
        Left errMsg -> panic $ pack errMsg
        Right bs    -> return bs
    setSecret bs = conf {configJwtSecret = Just bs}
    --
    -- replace: Replace every occurrence of one substring with another
    replaceUrlChars =
      replace "_" "/" . replace "-" "+" . replace "." "="

{-
  Load database uri from a separate file if `db-uri` is a filepath.
-}
loadDbUriFile :: AppConfig -> IO AppConfig
loadDbUriFile conf = extractDbUri mDbUri
  where
    mDbUri = configDbUri conf
    extractDbUri :: Text -> IO AppConfig
    extractDbUri dbUri =
      fmap setDbUri $
      case stripPrefix "@" dbUri of
        Nothing       -> return dbUri
        Just filename -> strip <$> readFile (toS filename)
    setDbUri dbUri = conf {configDbUri = dbUri}

-- Utilitarian functions.
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pass

whenNothing :: Applicative f => Maybe a -> f () -> f ()
whenNothing Nothing f = f
whenNothing _       _ = pass
