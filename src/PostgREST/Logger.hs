{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
{-|
Module      : PostgREST.Logger
Description : Logging based on the Observation.hs module. Access logs get sent to stdout and server diagnostic get sent to stderr.
-}
-- TODO log with buffering enabled to not lose throughput on logging levels higher than LogError
module PostgREST.Logger
  ( middleware
  , observationLogger
  , init
  , LoggerState
  ) where

import           Control.AutoUpdate                (defaultUpdateSettings,
                                                    mkAutoUpdate,
                                                    updateAction)
import           Control.Debounce
import qualified Data.ByteString.Char8             as BS
import qualified Data.Text.Encoding                as T
import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Snippet   as SQL hiding (sql)
import qualified Hasql.DynamicStatements.Statement as SQL
import qualified Hasql.Statement                   as SQL

import Data.Time (ZonedTime, defaultTimeLocale, formatTime,
                  getZonedTime)

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

import Network.HTTP.Types.Status (Status, status400, status500)
import System.IO.Unsafe          (unsafePerformIO)

import PostgREST.Config      (LogLevel (..), Verbosity (..))
import PostgREST.Observation
import PostgREST.Query       (MainQuery (..))

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Hasql.Connection           as SQL
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Pool.Observation     as SQL
import           Numeric                    (showFFloat)
import           PostgREST.Config.PgVersion (pgvName)
import qualified PostgREST.Error            as Error
import           Protolude

data LoggerState = LoggerState
  { stateGetZTime               :: IO ZonedTime  -- ^ Time with time zone used for logs
  , stateLogDebouncePoolTimeout :: IO ()         -- ^ Logs with a debounce
  }

init :: IO LoggerState
init = mdo
  let
    oneSecond = 1000000
    loggerState = LoggerState zTime debouncePoolTimeout
  zTime <- mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
  debouncePoolTimeout <- mkDebounce defaultDebounceSettings
          { debounceAction = logWithZTime loggerState $ observationMessages PoolAcqTimeoutObs
          , debounceFreq = 5*oneSecond
          , debounceEdge = leadingEdge -- logs at the start and the end
          }
  pure loggerState

-- TODO stop using this middleware to reuse the same "observer" pattern for all our logs
middleware :: LogLevel -> (Wai.Request -> Maybe BS.ByteString) -> Wai.Middleware
middleware logLevel getAuthRole =
    unsafePerformIO $
      Wai.mkRequestLogger Wai.defaultRequestLoggerSettings
      { Wai.outputFormat =
         Wai.ApacheWithSettings $
           Wai.defaultApacheSettings &
           Wai.setApacheRequestFilter (\_ res -> shouldLogResponse logLevel $ Wai.responseStatus res) &
           Wai.setApacheUserGetter getAuthRole
      , Wai.autoFlush = True
      , Wai.destination = Wai.Handle stdout
      }

shouldLogResponse :: LogLevel -> Status -> Bool
shouldLogResponse logLevel = case logLevel of
  LogCrit  -> const False
  LogError -> (>= status500)
  LogWarn  -> (>= status400)
  LogInfo  -> const True
  LogDebug -> const True

-- All observations are logged except some that depend on the log-level
observationLogger :: LoggerState -> LogLevel -> ObservationHandler
observationLogger loggerState logLevel obs = case obs of
  PoolAcqTimeoutObs -> do
    when (logLevel >= LogError) $
      stateLogDebouncePoolTimeout loggerState
  o@(QueryErrorCodeHighObs _) -> do
    when (logLevel >= LogError) $ do
      logWithZTime loggerState $ observationMessages o
  o@SchemaCacheEmptyObs ->
    when (logLevel >= LogError) $ do
    logWithZTime loggerState $ observationMessages o
  o@(HasqlPoolObs _) -> do
    when (logLevel >= LogDebug) $ do
      logWithZTime loggerState $ observationMessages o
  QueryObs gq status -> do
    when (shouldLogResponse logLevel status) $
      logMainQ loggerState gq
  o@PoolRequest ->
    when (logLevel >= LogDebug) $ do
      logWithZTime loggerState $ observationMessages o
  o@PoolRequestFullfilled ->
    when (logLevel >= LogDebug) $ do
      logWithZTime loggerState $ observationMessages o
  o@JwtCacheEviction ->
    when (logLevel >= LogDebug) $ do
      logWithZTime loggerState $ observationMessages o
  o@(JwtCacheLookup _) ->
    when (logLevel >= LogDebug) $ do
      logWithZTime loggerState $ observationMessages o
  o ->
    logWithZTime loggerState $ observationMessages o

logWithZTime :: LoggerState -> [Text] -> IO ()
logWithZTime loggerState txts = do
  zTime <- stateGetZTime loggerState
  traverse_ (hPutStrLn stderr . (toS (formatTime defaultTimeLocale "%d/%b/%Y:%T %z: " zTime) <>)) txts

logMainQ :: LoggerState -> MainQuery -> IO ()
logMainQ loggerState MainQuery{mqOpenAPI=(x, y, z),..} =
  let snipts  = renderSnippet <$> [mqTxVars, fromMaybe mempty mqPreReq, mqMain, x, y, z, fromMaybe mempty mqExplain]
      -- Does not log SQL when it's empty (happens on OPTIONS requests and when the openapi queries are not generated)
      logQ q = when (q /= mempty) $ logWithZTime loggerState $ pure $ showOnSingleLine '\n' $ T.decodeUtf8 q in
  mapM_ logQ snipts

-- TODO: maybe patch upstream hasql-dynamic-statements so we have a less hackish way to convert
-- the SQL.Snippet or maybe don't use hasql-dynamic-statements and resort to plain strings for the queries and use regular hasql
renderSnippet :: SQL.Snippet -> ByteString
renderSnippet snippet =
  let SQL.Statement sql _ _ _ = SQL.dynamicallyParameterized snippet decoder prepared
      decoder = HD.noResult -- unused
      prepared = False  -- unused
  in
    sql

observationMessages :: Observation -> [Text]
observationMessages = \case
  AdminStartObs address ->
    pure $ "Admin server listening on " <> address
  AppStartObs ver ->
    pure $ "Starting PostgREST " <> T.decodeUtf8 ver <> "..."
  AppServerAddressObs address ->
    pure $ "API server listening on " <> address
  DBConnectedObs ver ->
    pure $ "Successfully connected to " <> ver
  ExitUnsupportedPgVersion pgVer minPgVer ->
    pure $ "Cannot run in this PostgreSQL version (" <> pgvName pgVer <> "), PostgREST needs at least " <> pgvName minPgVer
  ExitDBNoRecoveryObs ->
    pure "Automatic recovery disabled, exiting."
  ExitDBFatalError ServerAuthError usageErr ->
    pure $ "Failed to establish a connection. " <> jsonMessage usageErr
  ExitDBFatalError ServerPgrstBug usageErr ->
    pure $ "This is probably a bug in PostgREST, please report it at https://github.com/PostgREST/postgrest/issues. " <> jsonMessage usageErr
  ExitDBFatalError ServerError42P05 usageErr ->
    pure $ "If you are using connection poolers in transaction mode, try setting db-prepared-statements to false. " <> jsonMessage usageErr
  ExitDBFatalError ServerError08P01 usageErr ->
    pure $ "Connection poolers in statement mode are not supported." <> jsonMessage usageErr
  SchemaCacheEmptyObs ->
    pure $ T.decodeUtf8 . LBS.toStrict . Error.errorPayload Verbose $ Error.NoSchemaCacheError
  SchemaCacheErrorObs dbSchemas extraPaths usageErr ->
    pure $ "Failed to load the schema cache using "
      <> "db-schemas=" <> T.intercalate "," (toList dbSchemas)
      <> " and "
      <> "db-extra-search-path=" <> T.intercalate "," extraPaths
      <> ". " <> jsonMessage usageErr
  SchemaCacheQueriedObs resultTime ->
    pure $ "Schema cache queried in " <> showMillis resultTime  <> " milliseconds"
  SchemaCacheLoadedObs resultTime summary ->
    [
      "Schema cache loaded " <> summary
    , "Schema cache loaded in " <> showMillis resultTime <> " milliseconds"
    ]
  ConnectionRetryObs delay ->
    pure $ "Attempting to reconnect to the database in " <> (show delay::Text) <> " seconds..."
  QueryPgVersionError usageErr ->
    pure $ "Failed to query the PostgreSQL version. " <> jsonMessage usageErr
  DBListenStart host port fullName channel -> do
    pure $ "Listener connected to " <> fullName <> " on " <> show (fold $ host <> fmap (":" <>) port) <> " and listening for database notifications on the " <> show channel <> " channel"
  DBListenFail channel listenErr ->
    pure $ "Failed listening for database notifications on the " <> show channel <> " channel. " <>
      either showListenerConnError showListenerException listenErr
  DBListenRetry delay ->
    pure $ "Retrying listening for database notifications in " <> (show delay::Text) <> " seconds..."
  DBListenBugHint ->
    pure "HINT:  This is likely a bug in the notification queue, try executing the following to solve it: select pg_notification_queue_usage();"
  DBListenerGotSCacheMsg channel ->
    pure $ "Received a schema cache reload message on the " <> show channel <> " channel"
  DBListenerGotConfigMsg channel ->
    pure $ "Received a config reload message on the " <> show channel <> " channel"
  DBListenerConnectionCleanupFail ex ->
    pure $ "Failed during listener connection cleanup: " <> showOnSingleLine '\t' (show ex)
  QueryObs{} ->
    mempty -- TODO pending refactor: The logic for printing the query cannot be done here. Join the observationMessages function into observationLogger to avoid this mempty.
  ConfigReadErrorObs usageErr ->
    pure $ "Failed to query database settings for the config parameters." <> jsonMessage usageErr
  QueryRoleSettingsErrorObs usageErr ->
    pure $ "Failed to query the role settings. " <> jsonMessage usageErr
  QueryErrorCodeHighObs usageErr ->
    pure $ jsonMessage usageErr
  ConfigInvalidObs err ->
    pure $ "Failed reloading config: " <> err
  ConfigSucceededObs ->
     pure "Config reloaded"
  PoolInit poolSize ->
     pure $ "Connection Pool initialized with a maximum size of " <> show poolSize <> " connections"
  PoolAcqTimeoutObs -> pure $ jsonMessage SQL.AcquisitionTimeoutUsageError
  HasqlPoolObs (SQL.ConnectionObservation uuid status) ->
    pure $ "Connection " <> show uuid <> (
      case status of
        SQL.ConnectingConnectionStatus   -> " is being established"
        SQL.ReadyForUseConnectionStatus  -> " is available"
        SQL.InUseConnectionStatus        -> " is used"
        SQL.TerminatedConnectionStatus reason -> " is terminated due to " <> case reason of
          SQL.AgingConnectionTerminationReason          -> "max lifetime"
          SQL.IdlenessConnectionTerminationReason       -> "max idletime"
          SQL.ReleaseConnectionTerminationReason        -> "release"
          SQL.NetworkErrorConnectionTerminationReason _ -> "network error" -- usage error is already logged, no need to repeat the same message.
    )
  PoolRequest ->
    pure "Trying to borrow a connection from pool"
  PoolRequestFullfilled ->
    pure "Borrowed a connection from the pool"
  JwtCacheLookup _ ->
    pure "Looked up a JWT in JWT cache"
  JwtCacheEviction ->
    pure "Evicted entry from JWT cache"
  WarpErrorObs txt ->
    pure $ "Warp server error: " <> txt
  where
    showMillis :: Double -> Text
    showMillis x = toS $ showFFloat (Just 1) x ""

    jsonMessage err = T.decodeUtf8 . LBS.toStrict . Error.errorPayload Verbose $ Error.PgError False err


    showListenerConnError :: SQL.ConnectionError -> Text
    showListenerConnError = maybe "Connection error" (showOnSingleLine '\t' . T.decodeUtf8)

    showListenerException :: SomeException -> Text
    showListenerException = showOnSingleLine '\t' . show


showOnSingleLine :: Char -> Text -> Text
showOnSingleLine split txt = T.intercalate " " $ T.filter (/= split) <$> T.lines txt -- the errors from hasql-notifications come intercalated with "\t\n"
