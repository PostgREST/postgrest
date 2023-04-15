{-|
Module      : PostgREST.Logger
Description : Wai Middleware to log requests to stdout.
-}
module PostgREST.Logger (middleware) where

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

import Network.HTTP.Types.Status (status400, status500)
import System.IO.Unsafe          (unsafePerformIO)

import qualified PostgREST.Auth   as Auth
import           PostgREST.Config (LogLevel (..))

import Protolude

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
