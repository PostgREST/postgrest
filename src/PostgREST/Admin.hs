{-# LANGUAGE NamedFieldPuns #-}

module PostgREST.Admin
  ( runAdmin
  ) where

import qualified Data.Aeson                as JSON
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai               as Wai
import qualified Network.Wai.Handler.Warp  as Warp

import Control.Monad.Extra (whenJust)

import qualified Data.ByteString.Lazy as LBS

import Network.Socket
import Network.Socket.ByteString

import PostgREST.AppState    (AppState)
import PostgREST.Config      (AppConfig (..))
import PostgREST.Metrics     (metricsToText)
import PostgREST.Network     (resolveHost)
import PostgREST.Observation (Observation (..))

import qualified PostgREST.AppState as AppState
import qualified PostgREST.Config   as Config


import Protolude

runAdmin :: AppState -> Warp.Settings -> IO ()
runAdmin appState settings = do
  AppConfig{configAdminServerPort} <- AppState.getConfig appState
  whenJust (AppState.getSocketAdmin appState) $ \adminSocket -> do
    host <- resolveHost adminSocket
    observer $ AdminStartObs host configAdminServerPort
    void . forkIO $ Warp.runSettingsSocket settings adminSocket adminApp
  where
    adminApp = admin appState
    observer = AppState.getObserver appState

-- | PostgREST admin application
admin :: AppState.AppState -> Wai.Application
admin appState req respond  = do
  isMainAppReachable  <- isRight <$> reachMainApp (AppState.getSocketREST appState)
  isLoaded <- AppState.isLoaded appState
  isPending <- AppState.isPending appState

  case Wai.pathInfo req of
    ["live"] ->
      respond $ Wai.responseLBS (if isMainAppReachable then HTTP.status200 else HTTP.status500) [] mempty
    ["ready"] ->
      let
        status | not isMainAppReachable = HTTP.status500
               | isPending              = HTTP.status503
               | isLoaded               = HTTP.status200
               | otherwise              = HTTP.status500
      in
      respond $ Wai.responseLBS status [] mempty
    ["config"] -> do
      config <- AppState.getConfig appState
      respond $ Wai.responseLBS HTTP.status200 [] (LBS.fromStrict $ encodeUtf8 $ Config.toText config)
    ["schema_cache"] -> do
      sCache <- AppState.getSchemaCache appState
      respond $ Wai.responseLBS HTTP.status200 [] (maybe mempty JSON.encode sCache)
    ["metrics"] -> do
      mets <- metricsToText
      respond $ Wai.responseLBS HTTP.status200 [] mets
    _ ->
      respond $ Wai.responseLBS HTTP.status404 [] mempty

-- Try to connect to the main app socket
-- Note that it doesn't even send a valid HTTP request, we just want to check that the main app is accepting connections
reachMainApp :: Socket -> IO (Either IOException ())
reachMainApp appSock = do
  sockAddr <- getSocketName appSock
  sock <- socket (addrFamily sockAddr) Stream defaultProtocol
  try $ do
    connect sock sockAddr
    withSocketsDo $ bracket (pure sock) close sendEmpty
  where
    sendEmpty sock = void $ send sock mempty
    addrFamily (SockAddrInet _ _) = AF_INET
    addrFamily (SockAddrInet6 {}) = AF_INET6
    addrFamily (SockAddrUnix _)   = AF_UNIX
