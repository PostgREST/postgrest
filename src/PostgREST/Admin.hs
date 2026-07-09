{-# LANGUAGE LambdaCase #-}

module PostgREST.Admin
  ( runAdmin
  ) where

import qualified Data.Aeson                as JSON
import           Data.List                 (lookup)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai               as Wai
import qualified Network.Wai.Handler.Warp  as Warp

import Control.Monad.Extra (whenJust)

import PostgREST.AppState    (AppState, getConfig)
import PostgREST.Config      (AppConfig (..))
import PostgREST.MediaType   (MediaType (..), toContentType)
import PostgREST.Metrics     (metricsToText)
import PostgREST.Network     (resolveSocketToAddress)
import PostgREST.Observation (Observation (..))

import qualified PostgREST.AppState as AppState

import qualified Network.Socket as NS
import           Protolude

runAdmin :: AppState -> Maybe NS.Socket -> IO Bool -> Warp.Settings -> IO ()
runAdmin appState maybeAdminSocket checkMainAppLive settings = do
  conf <- getConfig appState
  whenJust maybeAdminSocket $ \adminSocket -> do
    address <- resolveSocketToAddress adminSocket
    void . forkIO $ handle (onError adminSocket) $
      Warp.runSettingsSocket (adminServerSettings conf address) adminSocket adminApp
  where
    adminApp = admin appState checkMainAppLive
    observer = AppState.getObserver appState
    adminServerSettings config addr=
      settings
        & Warp.setBeforeMainLoop (observer $ AdminStartObs addr)
        & maybe identity Warp.setPort (configAdminServerPort config)

    onError adminSock ex = do
      observer $ AdminServerCrashedObs ex
      NS.close adminSock -- we close the socket so request doesn't hang

-- | PostgREST admin application
admin :: AppState.AppState -> IO Bool -> Wai.Application
admin appState checkMainAppLive req respond = do
  isMainAppLive <- checkMainAppLive
  isLoaded <- AppState.isLoaded appState
  isPending <- AppState.isPending appState

  case Wai.pathInfo req of
    ["live"] ->
      respond $ Wai.responseLBS (if isMainAppLive then HTTP.status200 else HTTP.status500) [] mempty
    ["ready"] ->
      case tenantParam req of
        Nothing ->
          let
            status | isPending         = HTTP.status503
                   | not isMainAppLive = HTTP.status500
                   | isLoaded          = HTTP.status200
                   | otherwise         = HTTP.status500
          in
          respond $ Wai.responseLBS status [] mempty
        Just tenantKey -> do
          status <- tenantReadyStatus appState isMainAppLive tenantKey
          respond $ Wai.responseLBS status [] mempty
    ["schema_cache"] -> do
      case tenantParam req of
        Nothing -> do
          sCache <- AppState.getSchemaCache appState
          respond $ Wai.responseLBS HTTP.status200 [] (maybe mempty JSON.encode sCache)
        Just tenantKey -> do
          tenantSchemaCacheResponse appState tenantKey respond
    ["metrics"] -> do
      mets <- metricsToText
      respond $ Wai.responseLBS HTTP.status200 [toContentType MTTextPlain] mets -- Content-Type is required for prometheus compliance
    _ ->
      respond $ Wai.responseLBS HTTP.status404 [] mempty

tenantParam :: Wai.Request -> Maybe ByteString
tenantParam req =
  join $ lookup "tenant" $ Wai.queryString req

tenantReadyStatus :: AppState.AppState -> Bool -> ByteString -> IO HTTP.Status
tenantReadyStatus appState isMainAppReachable tenantKey
  | not isMainAppReachable = pure HTTP.status500
  | otherwise =
      AppState.getResidentTenantSchemaCache appState tenantKey <&> \case
        Left{} -> HTTP.status400
        Right (AppState.ResidentTenantSchemaCacheLoaded{}) -> HTTP.status200
        Right AppState.ResidentTenantMissing -> HTTP.status503
        Right AppState.ResidentTenantSchemaCachePending -> HTTP.status503

tenantSchemaCacheResponse :: AppState.AppState -> ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
tenantSchemaCacheResponse appState tenantKey respond =
  AppState.getResidentTenantSchemaCache appState tenantKey >>= \case
    Left{} ->
      respond $ Wai.responseLBS HTTP.status400 [] mempty
    Right AppState.ResidentTenantMissing ->
      respond $ Wai.responseLBS HTTP.status404 [] mempty
    Right AppState.ResidentTenantSchemaCachePending ->
      respond $ Wai.responseLBS HTTP.status503 [] mempty
    Right (AppState.ResidentTenantSchemaCacheLoaded sCache) ->
      respond $ Wai.responseLBS HTTP.status200 [] $ JSON.encode sCache
