{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
module Feature.MetricsSpec

where

import Network.Wai (Application)

import qualified PostgREST.AppState    as AppState
import           PostgREST.Metrics     (MetricsState (..))
import           PostgREST.Observation
import           Protolude
import           SpecHelper
import           Test.Hspec            (SpecWith, describe,
                                        expectationFailure, it)
import           Test.Hspec.Wai        (getState)

untilM :: Int -> (a -> Bool) -> IO a -> IO (Maybe a)
untilM timeout cond act = rightToMaybe <$> race (threadDelay timeout) (fix $
  \loop -> do
    value <- act
    if cond value then
      pure value
    else
      loop)

waitForSchemaReload :: Int -> IO Observation -> IO (Maybe Observation)
waitForSchemaReload timeout = untilM timeout $ \case
  SchemaCacheLoadedObs _ -> True
  _ -> False

spec :: SpecWith ((MetricsState, AppState.AppState, IO Observation), Application)
spec = describe "Server started with metrics enabled" $
  it "Should increase pool flushes metric when schema cache is reloaded" $ do
    (metrics, appState, readObs) <- getState

    checkState' metrics

      [
        expectCounter @"poolFlushes" (+1)
      ] $

      liftIO $ do
        AppState.schemaCacheLoader appState
        waitForSchemaReload 1000000 readObs >>=
          maybe (expectationFailure "Timeout waiting for schema reloading") mempty
