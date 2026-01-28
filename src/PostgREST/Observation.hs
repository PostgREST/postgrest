{-# LANGUAGE LambdaCase #-}
{-|
Module      : PostgREST.Observation
Description : This module holds an Observation type which is the core of Observability for PostgREST.
              The Observation and ObservationHandler (the observer) are abstractions that allow centralizing logging and metrics concerns,
              only observer calls with an Observation constructor are applied at different parts in the codebase.
              The Logger and Metrics modules then decide which observations to expose. Not all observations need to be logged nor all correspond to a metric.
-}
module PostgREST.Observation
  ( Observation(..)
  , ObsFatalError(..)
  , observationMessage
  , ObservationHandler
  , showOnSingleLine
  ) where

import qualified Data.ByteString.Lazy       as LBS
import           Data.List.NonEmpty         (toList)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Hasql.Connection           as SQL
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Pool.Observation     as SQL
import           Network.HTTP.Types.Status  (Status)
import           Numeric                    (showFFloat)
import           PostgREST.Config.PgVersion
import qualified PostgREST.Error            as Error
import           PostgREST.Query            (MainQuery)

import Protolude hiding (toList)

data Observation
  = AdminStartObs Text
  | AppStartObs ByteString
  | AppServerAddressObs Text
  | ExitUnsupportedPgVersion PgVersion PgVersion
  | ExitDBNoRecoveryObs
  | ExitDBFatalError ObsFatalError SQL.UsageError
  | DBConnectedObs Text
  | SchemaCacheEmptyObs
  | SchemaCacheErrorObs (NonEmpty Text) [Text] SQL.UsageError
  | SchemaCacheQueriedObs Double
  | SchemaCacheSummaryObs Text
  | SchemaCacheLoadedObs Double
  | ConnectionRetryObs Int
  | DBListenStart Text
  | DBListenFail Text (Either SQL.ConnectionError SomeException)
  | DBListenRetry Int
  | DBListenerGotSCacheMsg ByteString
  | DBListenerGotConfigMsg ByteString
  | DBListenerConnectionCleanupFail SomeException
  | QueryObs MainQuery Status
  | ConfigReadErrorObs SQL.UsageError
  | ConfigInvalidObs Text
  | ConfigSucceededObs
  | QueryRoleSettingsErrorObs SQL.UsageError
  | QueryErrorCodeHighObs SQL.UsageError
  | QueryPgVersionError SQL.UsageError
  | PoolInit Int
  | PoolAcqTimeoutObs SQL.UsageError
  | HasqlPoolObs SQL.Observation
  | PoolRequest
  | PoolRequestFullfilled
  | JwtCacheLookup Bool
  | JwtCacheEviction

data ObsFatalError = ServerAuthError | ServerPgrstBug | ServerError42P05 | ServerError08P01

type ObservationHandler = Observation -> IO ()

observationMessage :: Observation -> Text
observationMessage = \case
  AdminStartObs address ->
    "Admin server listening on " <> address
  AppStartObs ver ->
    "Starting PostgREST " <> T.decodeUtf8 ver <> "..."
  AppServerAddressObs address ->
    "API server listening on " <> address
  DBConnectedObs ver ->
    "Successfully connected to " <> ver
  ExitUnsupportedPgVersion pgVer minPgVer ->
    "Cannot run in this PostgreSQL version (" <> pgvName pgVer <> "), PostgREST needs at least " <> pgvName minPgVer
  ExitDBNoRecoveryObs ->
    "Automatic recovery disabled, exiting."
  ExitDBFatalError ServerAuthError usageErr ->
    "Failed to establish a connection. " <> jsonMessage usageErr
  ExitDBFatalError ServerPgrstBug usageErr ->
    "This is probably a bug in PostgREST, please report it at https://github.com/PostgREST/postgrest/issues. " <> jsonMessage usageErr
  ExitDBFatalError ServerError42P05 usageErr ->
    "If you are using connection poolers in transaction mode, try setting db-prepared-statements to false. " <> jsonMessage usageErr
  ExitDBFatalError ServerError08P01 usageErr ->
    "Connection poolers in statement mode are not supported." <> jsonMessage usageErr
  SchemaCacheEmptyObs ->
    T.decodeUtf8 . LBS.toStrict . Error.errorPayload $ Error.NoSchemaCacheError
  SchemaCacheErrorObs dbSchemas extraPaths usageErr ->
    "Failed to load the schema cache using "
      <> "db-schemas=" <> T.intercalate "," (toList dbSchemas)
      <> " and "
      <> "db-extra-search-path=" <> T.intercalate "," extraPaths
      <> ". " <> jsonMessage usageErr
  SchemaCacheQueriedObs resultTime ->
    "Schema cache queried in " <> showMillis resultTime  <> " milliseconds"
  SchemaCacheSummaryObs summary ->
    "Schema cache loaded " <> summary
  SchemaCacheLoadedObs resultTime ->
    "Schema cache loaded in " <> showMillis resultTime <> " milliseconds"
  ConnectionRetryObs delay ->
    "Attempting to reconnect to the database in " <> (show delay::Text) <> " seconds..."
  QueryPgVersionError usageErr ->
    "Failed to query the PostgreSQL version. " <> jsonMessage usageErr
  DBListenStart channel -> do
    "Listening for database notifications on the " <> show channel <> " channel"
  DBListenFail channel listenErr ->
    "Failed listening for database notifications on the " <> show channel <> " channel. " <>
      either showListenerConnError showListenerException listenErr
  DBListenRetry delay ->
    "Retrying listening for database notifications in " <> (show delay::Text) <> " seconds..."
  DBListenerGotSCacheMsg channel ->
    "Received a schema cache reload message on the " <> show channel <> " channel"
  DBListenerGotConfigMsg channel ->
    "Received a config reload message on the " <> show channel <> " channel"
  DBListenerConnectionCleanupFail ex ->
    "Failed during listener connection cleanup: " <> showOnSingleLine '\t' (show ex)
  QueryObs{} ->
    mempty -- TODO pending refactor: The logic for printing the query cannot be done here. Join the observationMessage function into observationLogger to avoid this mempty.
  ConfigReadErrorObs usageErr ->
    "Failed to query database settings for the config parameters." <> jsonMessage usageErr
  QueryRoleSettingsErrorObs usageErr ->
    "Failed to query the role settings. " <> jsonMessage usageErr
  QueryErrorCodeHighObs usageErr ->
    jsonMessage usageErr
  ConfigInvalidObs err ->
    "Failed reloading config: " <> err
  ConfigSucceededObs ->
     "Config reloaded"
  PoolInit poolSize ->
     "Connection Pool initialized with a maximum size of " <> show poolSize <> " connections"
  PoolAcqTimeoutObs usageErr ->
    jsonMessage usageErr
  HasqlPoolObs (SQL.ConnectionObservation uuid status) ->
    "Connection " <> show uuid <> (
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
    "Trying to borrow a connection from pool"
  PoolRequestFullfilled ->
    "Borrowed a connection from the pool"
  JwtCacheLookup _ ->
    "Looked up a JWT in JWT cache"
  JwtCacheEviction ->
    "Evicted entry from JWT cache"
  where
    showMillis :: Double -> Text
    showMillis x = toS $ showFFloat (Just 1) x ""

    jsonMessage err = T.decodeUtf8 . LBS.toStrict . Error.errorPayload $ Error.PgError False err


    showListenerConnError :: SQL.ConnectionError -> Text
    showListenerConnError = maybe "Connection error" (showOnSingleLine '\t' . T.decodeUtf8)

    showListenerException :: SomeException -> Text
    showListenerException = showOnSingleLine '\t' . show


showOnSingleLine :: Char -> Text -> Text
showOnSingleLine split txt = T.intercalate " " $ T.filter (/= split) <$> T.lines txt -- the errors from hasql-notifications come intercalated with "\t\n"
