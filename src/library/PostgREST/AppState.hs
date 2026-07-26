{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE RecursiveDo      #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

module PostgREST.AppState
  ( AppState
  , destroy
  , getConfig
  , getSchemaCache
  , getPgVersion
  , getNextDelay
  , getTime
  , getJwtCacheState
  , init
  , initWithPool
  , killApp
  , putConfig -- For tests TODO refactoring
  , putSchemaCache
  , putPgVersion
  , putIsListenerOn
  , usePool
  , readInDbConfig
  , schemaCacheLoader
  , getObserver
  , isLoaded
  , isPending
  , waitForSchemaCacheInit
  , waitForSchemaCacheLoaded
  ) where

import qualified Hasql.Pool              as SQL
import qualified Hasql.Session           as SQL
import qualified PostgREST.Auth.JwtCache as JwtCache
import qualified PostgREST.Logger        as Logger
import qualified PostgREST.Metrics       as Metrics
import           PostgREST.Observation
import           PostgREST.Version       (prettyVersion)

import Control.AutoUpdate         (defaultUpdateSettings, mkAutoUpdate,
                                   updateAction)
import Control.Concurrent.STM     (newEmptyTMVarIO, tryPutTMVar)
import Data.IORef                 (atomicModifyIORef, newIORef, readIORef)
import Data.Time.Clock            (getCurrentTime)
import PostgREST.AppState.Pool    (destroy, initPool, usePool)
import PostgREST.AppState.Reload  (isSchemaCacheLoaded, readInDbConfig,
                                   retryingSchemaCacheLoad,
                                   waitForSchemaCacheInit,
                                   waitForSchemaCacheLoaded)
import PostgREST.AppState.Types
import PostgREST.Config           (AppConfig (..))
import PostgREST.Config.PgVersion (minimumPgVersion)
import PostgREST.Debounce         (makeDebouncer)
import PostgREST.SchemaCache      (SchemaCache (..))

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client  as HC
import           Network.URI          (URI (..), URIAuth (..), parseURI,
                                       unEscapeString)
import           System.IO.Error      (userError)

import Protolude

init :: AppConfig -> IO () -> Maybe Text -> IO AppState
init conf@AppConfig{configLogLevel, configDbPoolSize} appKiller schemaCacheLoadUri = do
  loggerState  <- Logger.init
  metricsState <- Metrics.init configDbPoolSize
  let observer = liftA2 (>>) (Logger.observationLogger loggerState configLogLevel) (Metrics.observationMetrics metricsState)

  observer $ AppStartObs prettyVersion

  pool <- initPool conf observer
  appState <- initWithPool pool conf loggerState metricsState observer appKiller

  runInitialSchemaCacheLoader observer schemaCacheLoadUri appState

  pure appState

runInitialSchemaCacheLoader :: ObservationHandler -> Maybe Text -> AppState -> IO ()
runInitialSchemaCacheLoader observer schemaCacheLoadUri AppState{stateSchemaCache, stateSCacheStatus=SchemaCacheStatus{getSCStatusTMVar}} = do
  void $ forkIO $
    traverse (fetchInitialSchemaCache observer) schemaCacheLoadUri >>= foldMap setInitialSchemaCache . join
  where
    setInitialSchemaCache sc =
      whenM (atomically $ tryPutTMVar getSCStatusTMVar True) $
        atomicModifyIORef stateSchemaCache $ (, ()) . maybe (Just sc) Just

fetchInitialSchemaCache :: ObservationHandler -> Text -> IO (Maybe SchemaCache)
fetchInitialSchemaCache observer uri = flip catches [
  Handler (handleError @IOException),
  Handler (handleError @HC.HttpException),
  Handler (handleError @JSON.AesonException)
  ] $ maybe (throwIO $ userError "Invalid schema cache dump URI") pure =<< traverse fetchURI (parseURI $ toS uri)
  where
    handleError :: Show e => e -> IO (Maybe a)
    handleError = (Nothing <$) . observer . SchemaCacheInitialLoadFailureObs uri . show

    fetchURI URI{uriScheme, ..}
      | uriScheme == "file:" = do
          path <- fileURIPath uriAuthority uriPath
          Just <$> (JSON.throwDecode =<< LBS.readFile path)
      | uriScheme `elem` ["http:", "https:"] = do
          request <- HC.parseUrlThrow $ toS uri
          manager <- HC.newManager HC.defaultManagerSettings
          HC.withResponse request manager $ \response ->
            Just <$> (JSON.throwDecode . LBS.fromChunks =<< HC.brConsume (HC.responseBody response))
      | otherwise =
          throwIO $ userError $ "Unsupported schema cache dump URI scheme: " <> uriScheme

    fileURIPath Nothing path = pure $ unEscapeString path
    fileURIPath (Just URIAuth{uriRegName=""}) path = pure $ unEscapeString path
    fileURIPath (Just URIAuth{uriRegName="localhost"}) path = pure $ unEscapeString path
    fileURIPath _ _ = throwIO $ userError "Only local file URIs are supported"

initWithPool :: SQL.Pool -> AppConfig -> Logger.LoggerState -> Metrics.MetricsState -> ObservationHandler -> IO () -> IO AppState
initWithPool pool conf loggerState metricsState observer appKiller = mdo

  appState <- AppState pool
    <$> newIORef minimumPgVersion -- assume we're in a supported version when starting, this will be corrected on a later step
    <*> newIORef Nothing
    <*> newSchemaCacheStatus
    <*> newIORef False
    <*> makeDebouncer (retryingSchemaCacheLoad appState *> threadDelay 100000)  -- 100ms cooldown
    <*> newIORef conf
    <*> mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }
    <*> pure appKiller
    <*> newIORef 0
    <*> pure observer
    <*> JwtCache.init conf observer
    <*> pure loggerState
    <*> pure metricsState

  return appState

isConnEstablished :: AppState -> IO Bool
isConnEstablished appState = do
  AppConfig{..} <- getConfig appState
  if configDbChannelEnabled then -- if the listener is enabled, we can be sure the connection is up
    readIORef $ stateIsListenerOn appState
  else -- otherwise the only way to check the connection is to make a query
    isRight <$> usePool appState (SQL.sql "SELECT 1")

isLoaded :: AppState -> IO Bool
isLoaded x = do
  scacheLoaded <- isSchemaCacheLoaded x
  connEstablished <- isConnEstablished x
  return $ scacheLoaded && connEstablished

isPending :: AppState -> IO Bool
isPending x = do
  scacheLoaded <- isSchemaCacheLoaded x
  connEstablished <- isConnEstablished x
  return $ not scacheLoaded || not connEstablished

newSchemaCacheStatus :: IO SchemaCacheStatus
newSchemaCacheStatus = SchemaCacheStatus <$> newEmptyTMVarIO
