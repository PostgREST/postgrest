{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PostgREST.Observation
-- Description : This module holds an Observation type which is the core of Observability for PostgREST.
--               The Observation and ObservationHandler (the observer) are abstractions that allow centralizing logging and metrics concerns,
--               only observer calls with an Observation constructor are applied at different parts in the codebase.
--               The Logger and Metrics modules then decide which observations to expose. Not all observations need to be logged nor all correspond to a metric.
module PostgREST.Observation
  ( Observation (..)
  , ObsFatalError (..)
  , ObservationHandler
  , IsObservation (..)
  , LogLine (..)
  )
where

import GHC.RTS.Flags (RTSFlags)
import Network.HTTP.Types.Status (Status, status400, status500)
import Numeric (showFFloat)
import Protolude

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.Wai qualified as Wai

import PostgREST.Config (LogLevel (..), Verbosity (..))
import PostgREST.Config.PgVersion
import PostgREST.Logger.Apache (apacheFormat)
import PostgREST.Observation.JwtCache (JwtCacheObservation (..), jwtCacheMessage)
import PostgREST.Query (MainQuery (..))
import PostgREST.SchemaCache (QueryTimings, queryTimingsWLabels)

import Hasql.Connection qualified as SQL
import Hasql.Decoders qualified as HD
import Hasql.DynamicStatements.Snippet qualified as SQL hiding (sql)
import Hasql.DynamicStatements.Statement qualified as SQL
import Hasql.Pool qualified as SQL
import Hasql.Pool.Observation qualified as SQL
import Hasql.Statement qualified as SQL
import PostgREST.Error qualified as Error

data Observation
  = AdminStartObs Text
  | AdminServerCrashedObs SomeException
  | AppStartObs ByteString
  | RTSFlagsObs RTSFlags
  | AppServerAddressObs Text
  | ExitUnsupportedPgVersion PgVersion PgVersion
  | ExitDBNoRecoveryObs
  | ExitDBFatalError ObsFatalError SQL.UsageError
  | DBConnectedObs Text
  | SchemaCacheEmptyObs
  | SchemaCacheErrorObs (NonEmpty Text) [Text] SQL.UsageError
  | SchemaCacheQueriedObs Double (Maybe QueryTimings)
  | SchemaCacheLoadedObs Double Text
  | ConnectionRetryObs Int
  | DBListenStart (Maybe ByteString) (Maybe ByteString) Text Text -- host, port, version string, channel
  | DBListenFail Text (Either SQL.ConnectionError SomeException)
  | DBListenRetry Int
  | DBListenBugCallQueryFix
  | DBListenerGotSCacheMsg ByteString
  | DBListenerGotConfigMsg ByteString
  | DBListenerConnectionCleanupFail SomeException
  | QueryObs MainQuery Status
  | SchemaCacheQueryObs [ByteString]
  | LegacyTargetNameWarningObs (Text, Text) ByteString ByteString
  | ConfigReadErrorObs SQL.UsageError
  | ConfigInvalidObs Text
  | ConfigSucceededObs
  | QueryRoleSettingsErrorObs SQL.UsageError
  | QueryErrorCodeHighObs SQL.UsageError
  | QueryPgVersionError SQL.UsageError
  | PoolInit Int
  | PoolAcqTimeoutObs
  | HasqlPoolObs SQL.Observation
  | ResponseObs (Maybe ByteString) Wai.Request Status (Maybe Integer)
  | PoolRequest
  | PoolRequestFullfilled
  | PoolFlushed
  | JwtCacheObs JwtCacheObservation
  | TerminationUnixSignalObs Text
  | WarpServerObs Text
  deriving (Generic)

data ObsFatalError = ServerAuthError | ServerPgrstBug | ServerError42P05 | ServerError08P01

type ObservationHandler = Observation -> IO ()

-- | An observation type, giving the log level of each observation
class IsObservation o where
  obsLevel :: o -> LogLevel
  obsLog :: o -> LogLine

-- | What the logger writes for an observation
data LogLine
  = -- | Lines for stderr, each prefixed with the time
    DiagnosticLog [Text]
  | -- | A line for stdout, given the formatted time
    AccessLog (ByteString -> ByteString)

instance IsObservation Observation where
  obsLevel = \case
    AdminStartObs{} -> LogCrit
    AdminServerCrashedObs{} -> LogCrit
    AppStartObs{} -> LogCrit
    RTSFlagsObs{} -> LogDebug
    AppServerAddressObs{} -> LogCrit
    ExitUnsupportedPgVersion{} -> LogCrit
    ExitDBNoRecoveryObs -> LogCrit
    ExitDBFatalError{} -> LogCrit
    DBConnectedObs{} -> LogCrit
    SchemaCacheEmptyObs -> LogError
    SchemaCacheErrorObs{} -> LogCrit
    SchemaCacheQueriedObs{} -> LogCrit
    SchemaCacheLoadedObs{} -> LogCrit
    ConnectionRetryObs{} -> LogCrit
    DBListenStart{} -> LogCrit
    DBListenFail{} -> LogCrit
    DBListenRetry{} -> LogCrit
    DBListenBugCallQueryFix -> LogCrit
    DBListenerGotSCacheMsg{} -> LogCrit
    DBListenerGotConfigMsg{} -> LogCrit
    DBListenerConnectionCleanupFail{} -> LogCrit
    QueryObs _ status -> statusLevel status
    SchemaCacheQueryObs{} -> LogInfo
    LegacyTargetNameWarningObs{} -> LogCrit
    ConfigReadErrorObs{} -> LogCrit
    ConfigInvalidObs{} -> LogCrit
    ConfigSucceededObs -> LogCrit
    QueryRoleSettingsErrorObs{} -> LogCrit
    QueryErrorCodeHighObs{} -> LogError
    QueryPgVersionError{} -> LogCrit
    PoolInit{} -> LogCrit
    PoolAcqTimeoutObs -> LogError
    HasqlPoolObs{} -> LogDebug
    ResponseObs _ _ status _ -> statusLevel status
    PoolRequest -> LogDebug
    PoolRequestFullfilled -> LogDebug
    PoolFlushed -> LogDebug
    JwtCacheObs{} -> LogDebug
    TerminationUnixSignalObs{} -> LogCrit
    WarpServerObs{} -> LogDebug
  obsLog = \case
    ResponseObs maybeRole req status contentLen -> AccessLog $ \time -> apacheFormat maybeRole time req status contentLen
    o -> DiagnosticLog $ observationMessages o

-- | The log level of the observations about a response
statusLevel :: Status -> LogLevel
statusLevel status
  | status >= status500 = LogError
  | status >= status400 = LogWarn
  | otherwise = LogInfo

-- TODO: maybe patch upstream hasql-dynamic-statements so we have a less hackish way to convert
-- the SQL.Snippet or maybe don't use hasql-dynamic-statements and resort to plain strings for the queries and use regular hasql
renderSnippet :: SQL.Snippet -> ByteString
renderSnippet snippet =
  let
    SQL.Statement sql _ _ _ = SQL.dynamicallyParameterized snippet decoder False
    decoder = HD.noResult -- unused
  in
    sql

observationMessages :: Observation -> [Text]
observationMessages = \case
  AdminStartObs address ->
    pure $ "Admin server listening on " <> address
  AdminServerCrashedObs ex ->
    pure $ "Admin server crashed unexpectedly: " <> (showOnSingleLine '\t' . show) ex
  AppStartObs ver ->
    pure $ "Starting PostgREST " <> T.decodeUtf8 ver <> "..."
  RTSFlagsObs flags ->
    pure $ show flags
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
    pure $
      "Failed to load the schema cache using "
        <> "db-schemas="
        <> T.intercalate "," (toList dbSchemas)
        <> " and "
        <> "db-extra-search-path="
        <> T.intercalate "," extraPaths
        <> ". "
        <> jsonMessage usageErr
  SchemaCacheQueriedObs resultTime timings ->
    ["Schema cache queried in " <> showMillis resultTime <> " milliseconds "]
      <> let showTimings qt = [T.intercalate ", " $ (\(l, v) -> T.decodeUtf8 l <> ": " <> v <> " ms") <$> queryTimingsWLabels qt]
         in  maybe mempty showTimings timings
  SchemaCacheLoadedObs resultTime summary ->
    [ "Schema cache loaded " <> summary,
      "Schema cache loaded in " <> showMillis resultTime <> " milliseconds"
    ]
  ConnectionRetryObs delay ->
    pure $ "Attempting to reconnect to the database in " <> (show delay :: Text) <> " seconds..."
  QueryPgVersionError usageErr ->
    pure $ "Failed to query the PostgreSQL version. " <> jsonMessage usageErr
  DBListenStart host port fullName channel -> do
    pure $ "Listener connected to " <> fullName <> " on " <> show (fold $ host <> fmap (":" <>) port) <> " and listening for database notifications on the " <> show channel <> " channel"
  DBListenFail channel listenErr ->
    pure $
      "Failed listening for database notifications on the "
        <> show channel
        <> " channel. "
        <> either showListenerConnError showListenerException listenErr
  DBListenRetry delay ->
    pure $ "Retrying listening for database notifications in " <> (show delay :: Text) <> " seconds..."
  DBListenBugCallQueryFix ->
    pure "This is likely a PostgreSQL bug in the notification queue, executing the following to try to solve it: SELECT pg_notification_queue_usage();"
  DBListenerGotSCacheMsg channel ->
    pure $ "Received a schema cache reload message on the " <> show channel <> " channel"
  DBListenerGotConfigMsg channel ->
    pure $ "Received a config reload message on the " <> show channel <> " channel"
  DBListenerConnectionCleanupFail ex ->
    pure $ "Failed during listener connection cleanup: " <> showOnSingleLine '\t' (show ex)
  (QueryObs MainQuery{mqOpenAPI = (x, y, z), ..} _) ->
    let snipts = renderSnippet <$> [mqTxVars, fromMaybe mempty mqPreReq, mqMain, x, y, z, fromMaybe mempty mqExplain]
    in  showOnSingleLine '\n' . T.decodeUtf8 <$> filter (/= mempty) snipts
  SchemaCacheQueryObs queries ->
    -- Queries are formatted with newlines for readability in SchemaCache.hs so we need to join them so each query appears on a single log line
    showOnSingleLine '\n' . T.decodeUtf8 <$> queries
  LegacyTargetNameWarningObs (warningMsg, warningHints) requestMethod requestTarget ->
    [ "WARNING: " <> warningMsg,
      "Update filters, orders or limits that use " <> warningHints <> " in " <> "`" <> T.decodeUtf8 (requestMethod <> " " <> requestTarget) <> "`"
    ]
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
    pure $
      "Connection "
        <> show uuid
        <> ( case status of
               SQL.ConnectingConnectionStatus -> " is being established"
               SQL.ReadyForUseConnectionStatus reason ->
                 " is available due to " <> case reason of
                   SQL.EstablishedConnectionReadyForUseReason -> "connection establishment"
                   SQL.SessionFailedConnectionReadyForUseReason _ -> "session failure"
                   SQL.SessionSucceededConnectionReadyForUseReason -> "session success"
               SQL.InUseConnectionStatus -> " is used"
               SQL.TerminatedConnectionStatus reason ->
                 " is terminated due to " <> case reason of
                   SQL.AgingConnectionTerminationReason -> "max lifetime"
                   SQL.IdlenessConnectionTerminationReason -> "max idletime"
                   SQL.ReleaseConnectionTerminationReason -> "release"
                   SQL.NetworkErrorConnectionTerminationReason _ -> "network error" -- usage error is already logged, no need to repeat the same message.
                   SQL.InitializationErrorTerminationReason _ -> "init failure"
           )
  PoolRequest ->
    pure "Trying to borrow a connection from pool"
  PoolRequestFullfilled ->
    pure "Borrowed a connection from the pool"
  PoolFlushed ->
    pure "Database connection pool flushed"
  JwtCacheObs o ->
    pure $ jwtCacheMessage o
  TerminationUnixSignalObs signal ->
    pure $ "Received termination unix signal " <> signal
  WarpServerObs txt ->
    pure $ "Warp server: " <> txt
  ResponseObs{} ->
    mempty -- Control flow never reaches here, the response is logged as an AccessLog
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
