{-# LANGUAGE LambdaCase #-}
{-|
Module      : PostgREST.Observation
Description : Module for observability types
-}
module PostgREST.Observation
  ( Observation(..)
  , observationMessage
  ) where

import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text.Encoding    as T
import qualified Hasql.Pool            as SQL
import qualified Network.Socket        as NS
import           Numeric               (showFFloat)
import qualified PostgREST.Error       as Error
import           PostgREST.SchemaCache (SchemaCache, showSummary)

import Protolude
import Protolude.Partial (fromJust)

data Observation
  = AdminStartObs (Maybe Int)
  | AppServerStartObs ByteString
  | AppServerPortObs NS.PortNumber
  | AppServerUnixObs FilePath
  | DBConnectAttemptObs
  | ExitFatalObs Text
  | ExitDBNoRecoveryObs
  | DBConnectedObs Text
  | SchemaCacheFatalErrorObs SQL.UsageError Text
  | SchemaCacheNormalErrorObs SQL.UsageError
  | SchemaCacheQueriedObs Double
  | SchemaCacheSummaryObs SchemaCache
  | SchemaCacheLoadedObs Double
  | ConnectionRetryObs Int
  | ConnectionPgVersionErrorObs SQL.UsageError
  | DBListenerStart Text
  | DBListenerFailNoRecoverObs
  | DBListenerFailRecoverObs Text
  | ConfigReadErrorObs
  | ConfigReadErrorFatalObs SQL.UsageError Text
  | ConfigReadErrorNotFatalObs SQL.UsageError
  | ConfigInvalidObs Text
  | ConfigSucceededObs
  | QueryRoleSettingsErrorObs SQL.UsageError
  | QueryErrorCodeHighObs SQL.UsageError
  | PoolAcqTimeoutObs SQL.UsageError

observationMessage :: Observation -> Text
observationMessage = \case
  AdminStartObs port ->
    "Admin server listening on port " <> show (fromIntegral (fromJust port) :: Integer)
  AppServerStartObs ver ->
    "Starting PostgREST " <> T.decodeUtf8 ver <> "..."
  AppServerPortObs port ->
    "Listening on port " <> show port
  AppServerUnixObs sock ->
    "Listening on unix socket " <> show sock
  DBConnectAttemptObs ->
    "Attempting to connect to the database..."
  ExitFatalObs reason ->
    "Fatal error encountered. " <> reason
  ExitDBNoRecoveryObs ->
    "Automatic recovery disabled, exiting."
  DBConnectedObs ver ->
    "Successfully connected to " <> ver
  SchemaCacheFatalErrorObs usageErr hint ->
    "A fatal error ocurred when loading the schema cache. " <> hint <> ". " <> jsonMessage usageErr
  SchemaCacheNormalErrorObs usageErr ->
    "An error ocurred when loading the schema cache. " <> jsonMessage usageErr
  SchemaCacheQueriedObs resultTime ->
    "Schema cache queried in " <> showMillis resultTime  <> " milliseconds"
  SchemaCacheSummaryObs sCache ->
    "Schema cache loaded " <> showSummary sCache
  SchemaCacheLoadedObs resultTime ->
    "Schema cache loaded in " <> showMillis resultTime <> " milliseconds"
  ConnectionRetryObs delay ->
    "Attempting to reconnect to the database in " <> (show delay::Text) <> " seconds..."
  ConnectionPgVersionErrorObs usageErr ->
    jsonMessage usageErr
  DBListenerStart channel -> do
    "Listening for notifications on the " <> channel <> " channel"
  DBListenerFailNoRecoverObs ->
    "Automatic recovery disabled, exiting."
  DBListenerFailRecoverObs channel ->
    "Retrying listening for notifications on the " <> channel <> " channel.."
  ConfigReadErrorObs ->
    "An error ocurred when trying to query database settings for the config parameters"
  ConfigReadErrorFatalObs usageErr hint ->
    hint <> ". " <> jsonMessage usageErr
  ConfigReadErrorNotFatalObs usageErr ->
    jsonMessage usageErr
  QueryRoleSettingsErrorObs usageErr ->
    "An error ocurred when trying to query the role settings. " <> jsonMessage usageErr
  QueryErrorCodeHighObs usageErr ->
    jsonMessage usageErr
  ConfigInvalidObs err ->
    "Failed reloading config: " <> err
  ConfigSucceededObs ->
     "Config reloaded"
  PoolAcqTimeoutObs usageErr ->
    jsonMessage usageErr
  where
    showMillis :: Double -> Text
    showMillis x = toS $ showFFloat (Just 1) (x * 1000) ""

    jsonMessage err = T.decodeUtf8 . LBS.toStrict . Error.errorPayload $ Error.PgError False err
