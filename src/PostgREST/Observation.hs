{-|
Module      : PostgREST.Observation
Description : Module for observability types
-}
module PostgREST.Observation
  ( Observation(..)
  ) where

import qualified Hasql.Pool            as SQL
import qualified Network.Socket        as NS
import           PostgREST.SchemaCache (SchemaCache)

import Protolude

data Observation
  = AdminStartObs (Maybe Int)
  | AppStartObs ByteString
  | AppServerPortObs NS.PortNumber
  | AppServerUnixObs FilePath
  | AppDBConnectAttemptObs
  | AppExitFatalObs Text
  | AppExitDBNoRecoveryObs
  | AppDBConnectedObs Text
  | AppSCacheFatalErrorObs SQL.UsageError Text
  | AppSCacheNormalErrorObs SQL.UsageError
  | AppSCacheLoadSuccessObs SchemaCache Double
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
