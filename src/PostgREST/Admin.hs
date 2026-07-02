{-# LANGUAGE MonadComprehensions #-}
module PostgREST.Admin
  ( runAdmin
  ) where

import qualified Data.Aeson                as JSON
import           Foreign.C.Error           (Errno (..), eMFILE)
import           GHC.IO.Exception
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai               as Wai
import qualified Network.Wai.Handler.Warp  as Warp

import Control.Monad.Extra (whenJust)

import PostgREST.AppState    (AppState, getConfig)
import PostgREST.Config      (AppConfig (..))
import PostgREST.Debounce    (makeDebouncer)
import PostgREST.MediaType   (MediaType (..), toContentType)
import PostgREST.Metrics     (metricsToText)
import PostgREST.Network     (resolveSocketToAddress)
import PostgREST.Observation (Observation (..))

import qualified PostgREST.AppState as AppState

import qualified Network.Socket as NS
import           Protolude

runAdmin :: AppState -> Maybe NS.Socket -> IO Bool -> Warp.ServerState -> Warp.Settings -> IO ()
runAdmin appState maybeAdminSocket checkMainAppLive serverState settings = do
  conf <- getConfig appState
  whenJust maybeAdminSocket $ \adminSocket -> do
    address <- resolveSocketToAddress adminSocket
    -- log EMFILE at most once every 10s
    logEMFILE <- makeDebouncer $ observer AdminServerAcceptEMFILEFailure *> threadDelay 10_000_000
    let
      safeAccept sock = NS.accept sock `catchEMFile` do
        -- Keep accepting on eMFILE
        logEMFILE

        connCount <- Warp.currentOpenConnections serverState
        if connCount > 0 then
          -- There are open connections. Wait for closing some.
          atomically $
            check . (connCount >) =<< Warp.currentOpenConnectionsSTM serverState
        else
          -- Backoff for 1ms so that we don't enter busy accept loop
          -- this should let the system recover fd's once the pressure is gone
          threadDelay 1_000

        -- Restart accepting
        safeAccept sock

    void . forkIO $ handle (onError adminSocket) $
      Warp.runSettingsSocket (adminServerSettings conf address safeAccept) adminSocket adminApp
  where
    adminApp = admin appState checkMainAppLive
    observer = AppState.getObserver appState
    isEMFILE err = Just eMFILE == fmap Errno (ioe_errno err)
    catchEMFile action handler = handleJust (\e -> [ e | isEMFILE e]) (const handler) action
    adminServerSettings config addr safeAccept = do
      settings
        & Warp.setAccept safeAccept
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
      let
        status | isPending              = HTTP.status503
               | not isMainAppLive      = HTTP.status500
               | isLoaded               = HTTP.status200
               | otherwise              = HTTP.status500
      in
      respond $ Wai.responseLBS status [] mempty
    ["schema_cache"] -> do
      sCache <- AppState.getSchemaCache appState
      respond $ Wai.responseLBS HTTP.status200 [] (maybe mempty JSON.encode sCache)
    ["metrics"] -> do
      mets <- metricsToText
      respond $ Wai.responseLBS HTTP.status200 [toContentType MTTextPlain] mets -- Content-Type is required for prometheus compliance
    _ ->
      respond $ Wai.responseLBS HTTP.status404 [] mempty
