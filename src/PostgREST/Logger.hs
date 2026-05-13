{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
{-|
Module      : PostgREST.Logger
Description : Logging based on the Observation.hs module. Access logs get sent to stdout and server diagnostic get sent to stderr.
-}
-- TODO log with buffering enabled to not lose throughput on logging levels higher than LogError
module PostgREST.Logger
  (observationLogger
  , init
  , LoggerState
  ) where

import           Control.AutoUpdate                (defaultUpdateSettings,
                                                    mkAutoUpdate,
                                                    updateAction)
import qualified Data.ByteString.Char8             as BS
import qualified Data.Text.Encoding                as T
import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Snippet   as SQL hiding (sql)
import qualified Hasql.DynamicStatements.Statement as SQL
import qualified Hasql.Statement                   as SQL

import Data.Time (ZonedTime, defaultTimeLocale, formatTime,
                  getZonedTime)

import Network.HTTP.Types.Status (Status, status400, status500)

import PostgREST.Config        (LogLevel (..), Verbosity (..))
import PostgREST.Debounce      (makeDebouncer)
import PostgREST.Logger.Apache (apacheFormat)
import PostgREST.Observation
import PostgREST.Query         (MainQuery (..))
import PostgREST.SchemaCache   (queryTimingsWLabels)

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
  debouncePoolTimeout <- makeDebouncer $
    logWithZTime loggerState poolAcqTimeoutObsMessage *> threadDelay (5 * oneSecond) -- Enclose the log action in the debouncer, call stateLogDebouncePoolTimeout to actually run the log action
  pure loggerState
    where
      poolAcqTimeoutObsMessage = [jsonMessage SQL.AcquisitionTimeoutUsageError]

shouldLogResponse :: LogLevel -> Status -> Bool
shouldLogResponse logLevel = case logLevel of
  LogCrit  -> const False
  LogError -> (>= status500)
  LogWarn  -> (>= status400)
  LogInfo  -> const True
  LogDebug -> const True

logWithZTime :: LoggerState -> [Text] -> IO ()
logWithZTime loggerState txts = do
  zTime <- stateGetZTime loggerState
  let prefix = toS (formatZonedTime zTime) <> ": "
  traverse_ (hPutStrLn stderr . (prefix <>)) txts

formatZonedTime :: ZonedTime -> [Char]
formatZonedTime = formatTime defaultTimeLocale "%d/%b/%Y:%T %z"

-- TODO: maybe patch upstream hasql-dynamic-statements so we have a less hackish way to convert
-- the SQL.Snippet or maybe don't use hasql-dynamic-statements and resort to plain strings for the queries and use regular hasql
renderSnippet :: SQL.Snippet -> ByteString
renderSnippet snippet =
  let SQL.Statement sql _ _ _ = SQL.dynamicallyParameterized snippet decoder False
      decoder = HD.noResult -- unused
  in
    sql

-- | Log Observations using LoggerState
observationLogger :: LoggerState -> LogLevel -> Observation -> IO ()
observationLogger loggerState logLevel = \case
  -- Log on all Log Levels
  -- =====================
  AdminStartObs address ->
    logAnyLvl ["Admin server listening on " <> address]
  AppStartObs ver ->
    logAnyLvl ["Starting PostgREST " <> T.decodeUtf8 ver <> "..."]
  AppServerAddressObs address ->
    logAnyLvl ["API server listening on " <> address]
  DBConnectedObs ver ->
    logAnyLvl ["Successfully connected to " <> ver]
  ExitUnsupportedPgVersion pgVer minPgVer ->
    logAnyLvl ["Cannot run in this PostgreSQL version (" <> pgvName pgVer <> "), PostgREST needs at least " <> pgvName minPgVer]
  ExitDBNoRecoveryObs ->
    logAnyLvl ["Automatic recovery disabled, exiting."]
  ExitDBFatalError ServerAuthError usageErr ->
    logAnyLvl ["Failed to establish a connection. " <> jsonMessage usageErr]
  ExitDBFatalError ServerPgrstBug usageErr ->
    logAnyLvl ["This is probably a bug in PostgREST, please report it at https://github.com/PostgREST/postgrest/issues. " <> jsonMessage usageErr]
  ExitDBFatalError ServerError42P05 usageErr ->
    logAnyLvl ["If you are using connection poolers in transaction mode, try setting db-prepared-statements to false. " <> jsonMessage usageErr]
  ExitDBFatalError ServerError08P01 usageErr ->
    logAnyLvl ["Connection poolers in statement mode are not supported." <> jsonMessage usageErr]
  SchemaCacheErrorObs dbSchemas extraPaths usageErr ->
    logAnyLvl ["Failed to load the schema cache using "
      <> "db-schemas=" <> T.intercalate "," (toList dbSchemas)
      <> " and "
      <> "db-extra-search-path=" <> T.intercalate "," extraPaths
      <> ". " <> jsonMessage usageErr
    ]
  SchemaCacheQueriedObs resultTime timings ->
    logAnyLvl $ [ "Schema cache queried in " <> showMillis resultTime  <> " milliseconds " ] <>
    let showTimings qt = [ T.intercalate ", " $ (\(l, v) -> T.decodeUtf8 l <> ": " <> v <> " ms") <$> queryTimingsWLabels qt ] in
    maybe mempty showTimings timings
  SchemaCacheLoadedObs resultTime summary ->
    logAnyLvl [
      "Schema cache loaded " <> summary
    , "Schema cache loaded in " <> showMillis resultTime <> " milliseconds"
    ]
  ConnectionRetryObs delay ->
    logAnyLvl ["Attempting to reconnect to the database in " <> (show delay::Text) <> " seconds..."]
  QueryPgVersionError usageErr ->
    logAnyLvl ["Failed to query the PostgreSQL version. " <> jsonMessage usageErr]
  DBListenStart host port fullName channel -> do
    logAnyLvl ["Listener connected to " <> fullName <> " on " <> show (fold $ host <> fmap (":" <>) port) <> " and listening for database notifications on the " <> show channel <> " channel"]
  DBListenFail channel listenErr ->
    logAnyLvl ["Failed listening for database notifications on the " <> show channel <> " channel. " <>
      either showListenerConnError showListenerException listenErr
    ]
  DBListenRetry delay ->
    logAnyLvl ["Retrying listening for database notifications in " <> (show delay::Text) <> " seconds..."]
  DBListenBugCallQueryFix ->
    logAnyLvl ["This is likely a PostgreSQL bug in the notification queue, executing the following to try to solve it: SELECT pg_notification_queue_usage();"]
  DBListenerGotSCacheMsg channel ->
    logAnyLvl ["Received a schema cache reload message on the " <> show channel <> " channel"]
  DBListenerGotConfigMsg channel ->
    logAnyLvl ["Received a config reload message on the " <> show channel <> " channel"]
  DBListenerConnectionCleanupFail ex ->
    logAnyLvl ["Failed during listener connection cleanup: " <> showOnSingleLine '\t' (show ex)]
  ConfigReadErrorObs usageErr ->
    logAnyLvl ["Failed to query database settings for the config parameters." <> jsonMessage usageErr]
  QueryRoleSettingsErrorObs usageErr ->
    logAnyLvl ["Failed to query the role settings. " <> jsonMessage usageErr]
  ConfigInvalidObs err ->
    logAnyLvl ["Failed reloading config: " <> err]
  ConfigSucceededObs ->
     logAnyLvl ["Config reloaded"]
  PoolInit poolSize ->
     logAnyLvl ["Connection Pool initialized with a maximum size of " <> show poolSize <> " connections"]
  TerminationUnixSignalObs signal ->
    logAnyLvl ["Received termination unix signal " <> signal]

  -- Log on Log Level >= Debug
  -- =========================
  HasqlPoolObs (SQL.ConnectionObservation uuid status) ->
    logAboveLvl LogDebug ["Connection " <> show uuid <> connStatus]
      where
        connStatus = case status of
          SQL.ConnectingConnectionStatus   -> " is being established"
          SQL.ReadyForUseConnectionStatus reason -> " is available due to " <> case reason of
            SQL.EstablishedConnectionReadyForUseReason      -> "connection establishment"
            SQL.SessionFailedConnectionReadyForUseReason _  -> "session failure"
            SQL.SessionSucceededConnectionReadyForUseReason -> "session success"
          SQL.InUseConnectionStatus        -> " is used"
          SQL.TerminatedConnectionStatus reason -> " is terminated due to " <> case reason of
            SQL.AgingConnectionTerminationReason          -> "max lifetime"
            SQL.IdlenessConnectionTerminationReason       -> "max idletime"
            SQL.ReleaseConnectionTerminationReason        -> "release"
            SQL.NetworkErrorConnectionTerminationReason _ -> "network error" -- usage error is already logged, no need to repeat the same message.
            SQL.InitializationErrorTerminationReason _    -> "init failure"

  JwtCacheEviction ->
    logAboveLvl LogDebug ["Evicted entry from JWT cache"]
  JwtCacheLookup _ ->
    logAboveLvl LogDebug ["Looked up a JWT in JWT cache"]
  PoolFlushed ->
    logAboveLvl LogDebug ["Database connection pool flushed"]
  PoolRequest ->
    logAboveLvl LogDebug ["Trying to borrow a connection from pool"]
  PoolRequestFullfilled ->
    logAboveLvl LogDebug ["Borrowed a connection from the pool"]
  WarpServerObs txt ->
    logAboveLvl LogDebug ["Warp server: " <> txt]

  -- Log on Log Level >= Error
  -- =========================
  QueryErrorCodeHighObs usageErr ->
    logAboveLvl LogError [jsonMessage usageErr]
  SchemaCacheEmptyObs ->
    logAboveLvl LogError [T.decodeUtf8 . LBS.toStrict . Error.errorPayload Verbose $ Error.NoSchemaCacheError]

  -- Log on Log Level >= Error with Debounce
  -- =======================================
  PoolAcqTimeoutObs ->
    when (logLevel >= LogError) $ stateLogDebouncePoolTimeout loggerState -- The log message is enclosed inside the debouncer at logger initialization time

  -- Log based on Log Level and HTTP status
  -- ======================================
  (QueryObs MainQuery{mqOpenAPI=(x, y, z),..} status) ->
      let snipts  = renderSnippet <$> [mqTxVars, fromMaybe mempty mqPreReq, mqMain, x, y, z, fromMaybe mempty mqExplain]
      in
        when (shouldLogResponse logLevel status) $ logWithZTime loggerState $ showOnSingleLine '\n' . T.decodeUtf8 <$> filter (/= mempty) snipts
  ResponseObs maybeRole req status contentLen ->
    when (shouldLogResponse logLevel status) $ do
      zTime <- stateGetZTime loggerState
      putStr $ apacheFormat maybeRole (BS.pack $ formatZonedTime zTime) req status contentLen -- putStr prints to stdout
  where
    logAnyLvl = logWithZTime loggerState

    logAboveLvl :: LogLevel -> [Text] -> IO ()
    logAboveLvl lvl obs = when (logLevel >= lvl) $ logWithZTime loggerState obs

    showMillis :: Double -> Text
    showMillis x = toS $ showFFloat (Just 1) x ""

    showListenerConnError :: SQL.ConnectionError -> Text
    showListenerConnError = maybe "Connection error" (showOnSingleLine '\t' . T.decodeUtf8)

    showListenerException :: SomeException -> Text
    showListenerException = showOnSingleLine '\t' . show

jsonMessage :: SQL.UsageError -> Text
jsonMessage err = T.decodeUtf8 . LBS.toStrict . Error.errorPayload Verbose $ Error.PgError False err

showOnSingleLine :: Char -> Text -> Text
showOnSingleLine split txt = T.intercalate " " $ T.filter (/= split) <$> T.lines txt -- the errors from hasql-notifications come intercalated with "\t\n"
