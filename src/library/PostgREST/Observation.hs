{-# LANGUAGE LambdaCase #-}

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
  )
where

import GHC.RTS.Flags (RTSFlags)
import Network.HTTP.Types.Status (Status, status400, status500)
import Protolude hiding (toList)

import Network.Wai qualified as Wai

import PostgREST.Config (LogLevel (..))
import PostgREST.Config.PgVersion
import PostgREST.Observation.JwtCache (JwtCacheObservation (..))
import PostgREST.Query (MainQuery)
import PostgREST.SchemaCache (QueryTimings)

import Hasql.Connection qualified as SQL
import Hasql.Pool qualified as SQL
import Hasql.Pool.Observation qualified as SQL

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

-- | The log level of the observations about a response
statusLevel :: Status -> LogLevel
statusLevel status
  | status >= status500 = LogError
  | status >= status400 = LogWarn
  | otherwise = LogInfo
