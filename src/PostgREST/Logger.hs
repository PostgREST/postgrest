{-|
Module      : PostgREST.Logger
Description : Wai Middleware to log requests to stdout.
-}
module PostgREST.Logger
  ( middleware
  , logObservation
  , init
  ) where

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as T
import           Data.Time            (ZonedTime, defaultTimeLocale,
                                       formatTime, getZonedTime)
import qualified Hasql.Pool           as SQL

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import           Numeric                              (showFFloat)


import Network.HTTP.Types.Status (status400, status500)
import System.IO.Unsafe          (unsafePerformIO)

import PostgREST.Config      (LogLevel (..))
import PostgREST.Observation

import qualified PostgREST.Auth  as Auth
import qualified PostgREST.Error as Error

import PostgREST.SchemaCache (showSummary)


import Protolude
import Protolude.Partial (fromJust)

newtype LoggerState = LoggerState
  { stateGetZTime                 :: IO ZonedTime -- ^ Time with time zone used for logs
  }

init :: IO LoggerState
init = do
  zTime <- mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
  pure $ LoggerState zTime

logWithZTime :: LoggerState -> Text -> IO ()
logWithZTime loggerState txt = do
  zTime <- stateGetZTime loggerState
  hPutStrLn stderr $ toS (formatTime defaultTimeLocale "%d/%b/%Y:%T %z: " zTime) <> txt

logPgrstError :: LoggerState -> SQL.UsageError -> IO ()
logPgrstError loggerState e = logWithZTime loggerState . T.decodeUtf8 . LBS.toStrict $ Error.errorPayload $ Error.PgError False e

middleware :: LogLevel -> Wai.Middleware
middleware logLevel = case logLevel of
  LogInfo  -> requestLogger (const True)
  LogWarn  -> requestLogger (>= status400)
  LogError -> requestLogger (>= status500)
  LogCrit  -> requestLogger (const False)
  where
    requestLogger filterStatus = unsafePerformIO $ Wai.mkRequestLogger Wai.defaultRequestLoggerSettings
      { Wai.outputFormat = Wai.ApacheWithSettings $
          Wai.defaultApacheSettings
            & Wai.setApacheRequestFilter (\_ res -> filterStatus $ Wai.responseStatus res)
            & Wai.setApacheUserGetter Auth.getRole
      }

logObservation :: LoggerState -> Observation -> IO ()
logObservation loggerState obs =
  case obs of
    AdminStartObs port ->
      logWithZTime loggerState $ "Admin server listening on port " <> show (fromIntegral (fromJust port) :: Integer)
    AppStartObs ver ->
      logWithZTime loggerState $ "Starting PostgREST " <> T.decodeUtf8 ver <> "..."
    AppServerPortObs port ->
      logWithZTime loggerState $ "Listening on port " <> show port
    AppServerUnixObs sock ->
      logWithZTime loggerState $ "Listening on unix socket " <> show sock
    AppDBConnectAttemptObs ->
      logWithZTime loggerState "Attempting to connect to the database..."
    AppExitFatalObs reason ->
      logWithZTime loggerState $ "Fatal error encountered. " <> reason
    AppExitDBNoRecoveryObs ->
      logWithZTime loggerState "Automatic recovery disabled, exiting."
    AppDBConnectedObs ver ->
      logWithZTime loggerState $ "Successfully connected to " <> ver
    AppSCacheFatalErrorObs usageErr hint -> do
      logWithZTime loggerState "A fatal error ocurred when loading the schema cache"
      logPgrstError loggerState usageErr
      logWithZTime loggerState hint
    AppSCacheNormalErrorObs usageErr -> do
      logWithZTime loggerState "An error ocurred when loading the schema cache"
      logPgrstError loggerState usageErr
    AppSCacheLoadSuccessObs sCache resultTime -> do
      logWithZTime loggerState $ "Schema cache queried in " <> showMillis resultTime  <> " milliseconds"
      logWithZTime loggerState $ "Schema cache loaded " <> showSummary sCache
    ConnectionRetryObs delay -> do
      logWithZTime loggerState $ "Attempting to reconnect to the database in " <> (show delay::Text) <> " seconds..."
    ConnectionPgVersionErrorObs usageErr ->
      logPgrstError loggerState usageErr
    DBListenerStart channel -> do
      logWithZTime loggerState $ "Listening for notifications on the " <> channel <> " channel"
    DBListenerFailNoRecoverObs ->
      logWithZTime loggerState "Automatic recovery disabled, exiting."
    DBListenerFailRecoverObs channel ->
      logWithZTime loggerState $ "Retrying listening for notifications on the " <> channel <> " channel.."
    ConfigReadErrorObs ->
      logWithZTime loggerState "An error ocurred when trying to query database settings for the config parameters"
    ConfigReadErrorFatalObs usageErr hint -> do
      logPgrstError loggerState usageErr
      logWithZTime loggerState hint
    ConfigReadErrorNotFatalObs usageErr -> do
      logPgrstError loggerState usageErr
    QueryRoleSettingsErrorObs usageErr -> do
      logWithZTime loggerState "An error ocurred when trying to query the role settings"
      logPgrstError loggerState usageErr
    QueryErrorCodeHighObs usageErr -> do
      logPgrstError loggerState usageErr
    ConfigInvalidObs err -> do
      logWithZTime loggerState $ "Failed reloading config: " <> err
    ConfigSucceededObs -> do
      logWithZTime loggerState "Config reloaded"
    PoolAcqTimeoutObs usageErr -> do
      logPgrstError loggerState usageErr
  where
    showMillis :: Double -> Text
    showMillis x = toS $ showFFloat (Just 1) (x * 1000) ""
