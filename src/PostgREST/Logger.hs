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

newtype LoggerState = LoggerState
  { stateGetZTime                 :: IO ZonedTime -- ^ Time with time zone used for logs
  }

init :: IO LoggerState
init = do
  zTime <- mkAutoUpdate defaultUpdateSettings { updateAction = getZonedTime }
  pure $ LoggerState zTime

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
observationLogger loggerState obs = logWithZTime loggerState $ observationMessage obs

logWithZTime :: LoggerState -> Text -> IO ()
logWithZTime loggerState txt = do
  zTime <- stateGetZTime loggerState
  hPutStrLn stderr $ toS (formatTime defaultTimeLocale "%d/%b/%Y:%T %z: " zTime) <> txt
