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

import Data.Aeson ((.=))
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Time (UTCTime, ZonedTime, defaultTimeLocale, formatTime, getCurrentTime, getZonedTime, utcToLocalZonedTime)
import Protolude

import Data.Aeson qualified as JSON
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import PostgREST.Config (LogFormat (..), LogLevel (..), dumpLogLevel)
import PostgREST.Observation

data LoggerState = LoggerState
  { -- | Get LogLevel from Config
    getLogLevel :: IO LogLevel,
    -- | Get LogFormat from Config
    getLogFormat :: IO LogFormat
  }

init :: IO LogLevel -> IO LogFormat -> IO LoggerState
init getLogLvl getLogFmt = pure $ LoggerState getLogLvl getLogFmt

-- Observations are logged when the log-level is at least theirs
observationLogger :: (IsObservation o) => LoggerState -> o -> IO ()
observationLogger loggerState obs = do
  logLevel <- getLogLevel loggerState -- We need to do the IO action to read the "log-level" config value because it can be reloaded
  when (logLevel >= obsLevel obs) $ do
    logFormat <- getLogFormat loggerState
    case logFormat of
      LogFormatText ->
        case obsLog obs of
          DiagnosticLog msgs ->
            logWithZTime msgs
          AccessLog line -> do
            zTime <- getZonedTime
            putStr $ line (BS.pack $ formatZonedTime zTime) -- putStr prints to stdout
      LogFormatJson ->
        logJson obs

logWithZTime :: [Text] -> IO ()
logWithZTime txts = do
  zTime <- getZonedTime
  let prefix = toS (formatZonedTime zTime) <> ": "
  traverse_ (hPutStrLn stderr . (prefix <>)) txts

-- | Write the observation as one JSON object on a line, to the same output as in text
logJson :: (IsObservation o) => o -> IO ()
logJson obs = do
  time <- getCurrentTime
  (output, msg) <- case obsLog obs of
    DiagnosticLog msgs -> pure (stderr, T.intercalate "\n" msgs)
    AccessLog line -> do
      zTime <- utcToLocalZonedTime time
      pure (stdout, T.decodeUtf8Lenient . BS.dropWhileEnd (== '\n') $ line (BS.pack $ formatZonedTime zTime))
  let fields =
        mconcat
          [ "time" .= formatUTCTime time,
            "level" .= dumpLogLevel (obsLevel obs),
            "event" .= obsEvent obs,
            "msg" .= msg,
            obsFields obs
          ]
  -- one write per line, so lines of concurrent requests don't mix
  BS.hPut output . LBS.toStrict $ encodingToLazyByteString (JSON.pairs fields) <> "\n"

formatZonedTime :: ZonedTime -> [Char]
formatZonedTime = formatTime defaultTimeLocale "%d/%b/%Y:%T %z"

-- | RFC 3339 in UTC, with milliseconds
formatUTCTime :: UTCTime -> [Char]
formatUTCTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"
