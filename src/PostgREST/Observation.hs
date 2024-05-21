{-# LANGUAGE LambdaCase #-}
{-|
Module      : PostgREST.Observation
Description : Module for observability types
-}
module PostgREST.Observation
  ( Observation(..)
  , ObsFatalError(..)
  , observationMessage
  , ObservationHandler
  ) where

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Hasql.Connection           as SQL
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Pool.Observation     as SQL
import qualified Network.Socket             as NS
import           Numeric                    (showFFloat)
import           PostgREST.Config.PgVersion
import qualified PostgREST.Error            as Error

import Protolude
import Protolude.Partial (fromJust)

data Observation
  = AdminStartObs (Maybe Int)
  | AppStartObs ByteString
  | AppServerPortObs NS.PortNumber
  | AppServerUnixObs FilePath
  | DBConnectAttemptObs
  | ExitUnsupportedPgVersion PgVersion PgVersion
  | ExitDBNoRecoveryObs
  | ExitDBFatalError ObsFatalError SQL.UsageError
  | DBConnectedObs Text
  | SchemaCacheErrorObs SQL.UsageError
  | SchemaCacheQueriedObs Double
  | SchemaCacheSummaryObs Text
  | SchemaCacheLoadedObs Double
  | ConnectionRetryObs Int
  | ConnectionPgVersionErrorObs SQL.UsageError
  | DBListenStart Text
  | DBListenFail Text (Either SQL.ConnectionError (Either SomeException ()))
  | DBListenRetry Int
  | DBListenerGotSCacheMsg ByteString
  | DBListenerGotConfigMsg ByteString
  | ConfigReadErrorObs SQL.UsageError
  | ConfigInvalidObs Text
  | ConfigSucceededObs
  | QueryRoleSettingsErrorObs SQL.UsageError
  | QueryErrorCodeHighObs SQL.UsageError
  | PoolAcqTimeoutObs SQL.UsageError
  | HasqlPoolObs SQL.Observation
  | PoolRequest
  | PoolRequestFullfilled

data ObsFatalError = ServerAuthError | ServerPgrstBug | ServerError42P05 | ServerError08P01

type ObservationHandler = Observation -> IO ()

observationMessage :: Observation -> Text
observationMessage = \case
  AdminStartObs port ->
    "Admin server listening on port " <> show (fromIntegral (fromJust port) :: Integer)
  AppStartObs ver ->
    "Starting PostgREST " <> T.decodeUtf8 ver <> "..."
  AppServerPortObs port ->
    "Listening on port " <> show port
  AppServerUnixObs sock ->
    "Listening on unix socket " <> show sock
  DBConnectAttemptObs ->
    "Attempting to connect to the database..."
  DBConnectedObs ver ->
    "Successfully connected to " <> ver
  ExitUnsupportedPgVersion pgVer minPgVer ->
    "Cannot run in this PostgreSQL version (" <> pgvName pgVer <> "), PostgREST needs at least " <> pgvName minPgVer
  ExitDBNoRecoveryObs ->
    "Automatic recovery disabled, exiting."
  ExitDBFatalError ServerAuthError usageErr ->
    jsonMessage usageErr
  ExitDBFatalError ServerPgrstBug usageErr ->
    "This is probably a bug in PostgREST, please report it at https://github.com/PostgREST/postgrest/issues. " <> jsonMessage usageErr
  ExitDBFatalError ServerError42P05 usageErr ->
    "If you are using connection poolers in transaction mode, try setting db-prepared-statements to false. " <> jsonMessage usageErr
  ExitDBFatalError ServerError08P01 usageErr ->
    "Connection poolers in statement mode are not supported." <> jsonMessage usageErr
  SchemaCacheErrorObs usageErr ->
    "Failed to load the schema cache. " <> jsonMessage usageErr
  SchemaCacheQueriedObs resultTime ->
    "Schema cache queried in " <> showMillis resultTime  <> " milliseconds"
  SchemaCacheSummaryObs summary ->
    "Schema cache loaded " <> summary
  SchemaCacheLoadedObs resultTime ->
    "Schema cache loaded in " <> showMillis resultTime <> " milliseconds"
  ConnectionRetryObs delay ->
    "Attempting to reconnect to the database in " <> (show delay::Text) <> " seconds..."
  ConnectionPgVersionErrorObs usageErr ->
    jsonMessage usageErr
  DBListenStart channel -> do
    "Listening for notifications on the " <> show channel <> " channel"
  DBListenFail channel listenErr ->
    "Failed listening for notifications on the " <> show channel <> " channel. " <> (
      case listenErr of
        Left err  -> show err
        Right err -> showListenerError err
    )
  DBListenRetry delay ->
    "Retrying listening for notifications in " <> (show delay::Text) <> " seconds..."
  DBListenerGotSCacheMsg channel ->
    "Received a schema cache reload message on the " <> show channel <> " channel"
  DBListenerGotConfigMsg channel ->
    "Received a config reload message on the " <> show channel <> " channel"
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
  _ -> mempty
  where
    showMillis :: Double -> Text
    showMillis x = toS $ showFFloat (Just 1) (x * 1000) ""

    jsonMessage err = T.decodeUtf8 . LBS.toStrict . Error.errorPayload $ Error.PgError False err

    showListenerError :: Either SomeException () -> Text
    showListenerError (Right _) = "Failed getting notifications" -- should not happen as the listener will never finish (hasql-notifications uses `forever` internally) with a Right result
    showListenerError (Left e)  =
      let showOnSingleLine txt = T.intercalate " " $ T.filter (/= '\t') <$> T.lines txt in -- the errors from hasql-notifications come intercalated with "\t\n"
      showOnSingleLine $ show e
