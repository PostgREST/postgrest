module PostgREST.Admin
  ( runAdmin
  , admin
  )
where

import Control.Monad.Extra (whenJust)
import Protolude

import Data.Aeson qualified as JSON
import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket qualified as NS
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import PostgREST.Config (AppConfig (..))
import PostgREST.MediaType (MediaType (..), toContentType)
import PostgREST.Metrics (metricsToText)
import PostgREST.Network (resolveSocketToAddress)
import PostgREST.Observation (Observation (..), ObservationHandler)

import PostgREST.AppState qualified as AppState

runAdmin :: ObservationHandler -> IO () -> AppConfig -> Maybe NS.Socket -> Warp.Settings -> Wai.Application -> IO ()
runAdmin observer kill conf maybeAdminSocket settings adminApp =
  whenJust maybeAdminSocket $ \adminSocket -> do
    address <- resolveSocketToAddress adminSocket
    void . forkIO $
      handle onError $
        Warp.runSettingsSocket (adminServerSettings conf address) adminSocket adminApp
  where
    adminServerSettings config addr =
      settings
        & Warp.setBeforeMainLoop (observer $ AdminStartObs addr)
        & maybe identity Warp.setPort (configAdminServerPort config)

    onError ex = do
      observer $ AdminServerCrashedObs ex
      kill -- Admin server crash is deemed unrecoverable, so we kill postgrest

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
      let status
            | isPending = HTTP.status503
            | not isMainAppLive = HTTP.status500
            | isLoaded = HTTP.status200
            | otherwise = HTTP.status500
      in  respond $ Wai.responseLBS status [] mempty
    ["schema_cache"] -> do
      sCache <- AppState.getSchemaCache appState
      respond $ Wai.responseLBS HTTP.status200 [] (maybe mempty JSON.encode sCache)
    ["metrics"] -> do
      mets <- metricsToText
      respond $ Wai.responseLBS HTTP.status200 [toContentType MTTextPlain] mets -- Content-Type is required for prometheus compliance
    _ ->
      respond $ Wai.responseLBS HTTP.status404 [] mempty
