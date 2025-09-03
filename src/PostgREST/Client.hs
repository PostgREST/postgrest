{-|
Module      : PostgREST.Client
Description : PostgREST HTTP client
-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PostgREST.Client
  ( ready
  ) where

import qualified Data.Text           as T
import qualified Network.HTTP.Client as HC

import Network.HTTP.Client       (HttpException (..))
import Network.HTTP.Types.Status (Status (..))
import PostgREST.Config          (AppConfig (..))

import Protolude

ready :: AppConfig -> IO Status
ready AppConfig{configAdminServerHost, configAdminServerPort} = do

  client <- HC.newManager HC.defaultManagerSettings
  req <- HC.parseRequest $ -- Create HTTP Request
    case configAdminServerPort of
      Just port -> "http://" <> host <> ":" <> show port <> "/ready"
      Nothing   -> panic "Admin Server is not running. Please check your configuration."

  resp <- HC.httpLbs req client `catch` \(_ :: HttpException) -> do
    let url = show (HC.getUri req)
    hPutStrLn stderr $ T.pack $ "postgrest: health check request failed - connection refused to " <> url
    exitWith $ ExitFailure 1

  return $ HC.responseStatus resp
    where
      host = escapeHostName $ T.unpack configAdminServerHost
      -- We don't need to resolve the host name except the postgrest special
      -- addresses. The http-client package automatically resolves the other
      -- host names for us.
      escapeHostName = \case
        "*"  -> "0.0.0.0"
        "*4" -> "0.0.0.0"
        "!4" -> "0.0.0.0"
        "*6" -> "[::1]" -- TODO: Not covered by tests
        "!6" -> "[::1]" -- TODO: Not covered by tests
        h    -> h
