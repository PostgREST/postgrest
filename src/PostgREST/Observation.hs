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
  , ObservationHandler
  ) where

import qualified Hasql.Connection           as SQL
import qualified Hasql.Pool                 as SQL
import qualified Hasql.Pool.Observation     as SQL
import           Network.HTTP.Types.Status  (Status)
import           PostgREST.Config.PgVersion
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
  | SchemaCacheLoadedObs Double Text
  | ConnectionRetryObs Int
  | DBListenStart (Maybe ByteString) (Maybe ByteString) Text Text -- host, port, version string, channel
  | DBListenFail Text (Either SQL.ConnectionError SomeException)
  | DBListenRetry Int
  | DBListenBugHint -- https://github.com/PostgREST/postgrest/issues/3147
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
  | PoolAcqTimeoutObs
  | HasqlPoolObs SQL.Observation
  | PoolRequest
  | PoolRequestFullfilled
  | JwtCacheLookup Bool
  | JwtCacheEviction
  | WarpErrorObs Text

data ObsFatalError = ServerAuthError | ServerPgrstBug | ServerError42P05 | ServerError08P01

type ObservationHandler = Observation -> IO ()
