-- TODO log with buffering enabled to not lose throughput on logging levels higher than LogError

-- |
-- Module      : PostgREST.Logger
-- Description : Logging based on the Observation.hs module. Access logs get sent to stdout and server diagnostic get sent to stderr.
module PostgREST.Logger
  ( observationLogger
  , init
  , LoggerState
  )
where

import Data.Time (ZonedTime, defaultTimeLocale, formatTime, getZonedTime)
import Protolude

import Data.ByteString.Char8 qualified as BS

import PostgREST.Config (LogLevel (..))
import PostgREST.Observation

newtype LoggerState = LoggerState
  { -- | Get LogLevel from Config
    getLogLevel :: IO LogLevel
  }

init :: IO LogLevel -> IO LoggerState
init = pure . LoggerState

-- Observations are logged when the log-level is at least theirs
observationLogger :: (IsObservation o) => LoggerState -> o -> IO ()
observationLogger loggerState obs = do
  logLevel <- getLogLevel loggerState -- We need to do the IO action to read the "log-level" config value because it can be reloaded
  when (logLevel >= obsLevel obs) $
    case obsLog obs of
      DiagnosticLog msgs ->
        logWithZTime msgs
      AccessLog line -> do
        zTime <- getZonedTime
        putStr $ line (BS.pack $ formatZonedTime zTime) -- putStr prints to stdout

logWithZTime :: [Text] -> IO ()
logWithZTime txts = do
  zTime <- getZonedTime
  let prefix = toS (formatZonedTime zTime) <> ": "
  traverse_ (hPutStrLn stderr . (prefix <>)) txts

formatZonedTime :: ZonedTime -> [Char]
formatZonedTime = formatTime defaultTimeLocale "%d/%b/%Y:%T %z"
