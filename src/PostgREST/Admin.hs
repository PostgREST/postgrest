{-# LANGUAGE NamedFieldPuns #-}

module PostgREST.Admin
  ( runAdmin
  ) where

import qualified Data.Aeson                as JSON
import qualified Hasql.Session             as SQL
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai               as Wai
import qualified Network.Wai.Handler.Warp  as Warp

import Control.Monad.Extra (whenJust)

import qualified Data.ByteString.Lazy as LBS

import Network.Socket
import Network.Socket.ByteString

import PostgREST.AppState (AppState)
import PostgREST.Config   (AppConfig (..))

import qualified PostgREST.AppState as AppState
import qualified PostgREST.Config   as Config

import Protolude
import Protolude.Partial (fromJust)

runAdmin :: AppConfig -> AppState -> Warp.Settings -> IO ()
runAdmin conf@AppConfig{configAdminServerPort} appState settings =
  whenJust (AppState.getSocketAdmin appState) $ \adminSocket -> do
    AppState.logWithZTime appState $ "Admin server listening on port " <> show (fromIntegral (fromJust configAdminServerPort) :: Integer)
    void . forkIO $ Warp.runSettingsSocket settings adminSocket adminApp
  where
    adminApp = admin appState conf

-- | PostgREST admin application
admin :: AppState.AppState -> AppConfig -> Wai.Application
admin appState appConfig req respond  = do
  isMainAppReachable  <- isRight <$> reachMainApp (AppState.getSocketREST appState)
  isSchemaCacheLoaded <- isJust <$> AppState.getSchemaCache appState
  isConnectionUp      <-
    if configDbChannelEnabled appConfig
      then AppState.getIsListenerOn appState
      else isRight <$> AppState.usePool appState appConfig (SQL.sql "SELECT 1")

  case Wai.pathInfo req of
    ["ready"] ->
      respond $ Wai.responseLBS (if isMainAppReachable && isConnectionUp && isSchemaCacheLoaded then HTTP.status200 else HTTP.status503) [] mempty
    ["live"] ->
      respond $ Wai.responseLBS (if isMainAppReachable then HTTP.status200 else HTTP.status503) [] mempty
    ["config"] -> do
      config <- AppState.getConfig appState
      respond $ Wai.responseLBS HTTP.status200 [] (LBS.fromStrict $ encodeUtf8 $ Config.toText config)
    ["schema_cache"] -> do
      sCache <- AppState.getSchemaCache appState
      respond $ Wai.responseLBS HTTP.status200 [] (maybe mempty JSON.encode sCache)
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
