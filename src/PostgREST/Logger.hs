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

import           Control.AutoUpdate    (defaultUpdateSettings,
                                        mkAutoUpdate, updateAction)
import           Control.Debounce
import qualified Data.ByteString.Char8 as BS

import Data.Time (ZonedTime, defaultTimeLocale, formatTime,
                  getZonedTime)

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

import Network.HTTP.Types.Status (Status, status400, status500)
import System.IO.Unsafe          (unsafePerformIO)

import PostgREST.Config      (LogLevel (..))
import PostgREST.Observation

import Protolude

data LoggerState = LoggerState
  { stateGetZTime               :: IO ZonedTime  -- ^ Time with time zone used for logs
  , stateLogDebouncePoolTimeout :: MVar (IO ())  -- ^ Logs with a debounce
  }

init :: IO LoggerState
init = do
  zTime <- mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
  LoggerState zTime <$> newEmptyMVar

logWithDebounce :: LoggerState -> IO () -> IO ()
logWithDebounce loggerState action = do
  debouncer <- tryReadMVar $ stateLogDebouncePoolTimeout loggerState
  case debouncer of
    Just d -> d
    Nothing -> do
      newDebouncer <-
        let oneSecond = 1000000 in
        mkDebounce defaultDebounceSettings
           { debounceAction = action
           , debounceFreq = 5*oneSecond
           , debounceEdge = leadingEdge -- logs at the start and the end
           }
      putMVar (stateLogDebouncePoolTimeout loggerState) newDebouncer
      newDebouncer

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
  o@(PoolAcqTimeoutObs _) -> do
    when (logLevel >= LogError) $ do
      logWithDebounce loggerState $
        logWithZTime loggerState $ observationMessage o
  o@(QueryErrorCodeHighObs _) -> do
    when (logLevel >= LogError) $ do
      logWithZTime loggerState $ observationMessage o
  o@(HasqlPoolObs _) -> do
    when (logLevel >= LogDebug) $ do
      logWithZTime loggerState $ observationMessage o
  o@(DBQuery sql status) -> do
    -- Does not log SQL when it's empty (for OPTIONS requests or for the default OpenAPI output)
    when (sql /= mempty && shouldLogResponse logLevel status) $ do
      logWithZTime loggerState $ observationMessage o
  PoolRequest ->
    pure ()
  PoolRequestFullfilled ->
    pure ()
  o ->
    logWithZTime loggerState $ observationMessage o

logWithZTime :: LoggerState -> Text -> IO ()
logWithZTime loggerState txt = do
  zTime <- stateGetZTime loggerState
  hPutStrLn stderr $ toS (formatTime defaultTimeLocale "%d/%b/%Y:%T %z: " zTime) <> txt
