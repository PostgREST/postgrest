{-|
Module      : PostgREST.Logger
Description : Wai Middleware to log requests to stdout.
-}
module PostgREST.Logger
  ( middleware
  , observationLogger
  , init
  ) where

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Control.Debounce

import Data.Time (ZonedTime, defaultTimeLocale, formatTime,
                  getZonedTime)

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

import Network.HTTP.Types.Status (status400, status500)
import System.IO.Unsafe          (unsafePerformIO)

import PostgREST.Config      (LogLevel (..))
import PostgREST.Observation

import qualified PostgREST.Auth as Auth

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

observationLogger :: LoggerState -> ObservationHandler
observationLogger loggerState obs = case obs of
  o@(PoolAcqTimeoutObs _) -> do
    logWithDebounce loggerState $
      logWithZTime loggerState $ observationMessage o
  o ->
    logWithZTime loggerState $ observationMessage o

logWithZTime :: LoggerState -> Text -> IO ()
logWithZTime loggerState txt = do
  zTime <- stateGetZTime loggerState
  hPutStrLn stderr $ toS (formatTime defaultTimeLocale "%d/%b/%Y:%T %z: " zTime) <> txt
