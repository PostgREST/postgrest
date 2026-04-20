module Main where

import qualified Hasql.Pool                 as P
import qualified Hasql.Pool.Config          as P
import qualified Hasql.Transaction.Sessions as HT

import Data.Function    (id)
import System.Directory (findExecutable)

import           Network.Wai               (Application)
import           PostgREST.App             (postgrest)
import qualified PostgREST.AppState        as AppState
import           PostgREST.Config          (AppConfig (..))
import           PostgREST.Config.Database (queryPgVersion)
import qualified PostgREST.Logger          as Logger
import qualified PostgREST.Metrics         as Metrics
import           PostgREST.OpenTelemetry   (withTracer)
import           PostgREST.SchemaCache     (querySchemaCache)

import qualified Observation.JwtCache
import qualified Observation.MetricsSpec

import qualified Observation.OpenTelemetry
import qualified Observation.SchemaCacheSpec

import ObsHelper
import OTelHelper
import PostgREST.Observation (Observation (HasqlPoolObs))
import Protolude             hiding (toList)
import System.IO.Temp        (createTempDirectory,
                              getCanonicalTemporaryDirectory)
import Test.Hspec

main :: IO ()
main = do
  poolChan <- newChan
  -- make sure poolChan is not growing indefinitely
  -- start a thread that drains the channel
  -- this is necessary because test cases operate on
  -- copies so poolChan is never read from
  -- this means we have another thread running for the entire duration of the spec but this shouldn't be a problem since Haskell green threads are lightweight
  void $ forkIO $ forever $ readChan poolChan
  metricsState <- Metrics.init (configDbPoolSize testCfg)
  pool <- P.acquire $ P.settings
    [ P.size 3
    , P.acquisitionTimeout 10
    , P.agingTimeout 60
    , P.idlenessTimeout 60
    , P.staticConnectionSettings (toUtf8 $ configDbUri testCfg)
    -- make sure metrics are updated and pool observations published to poolChan
    , P.observationHandler $ (writeChan poolChan <> Metrics.observationMetrics metricsState) . HasqlPoolObs
    ]

  actualPgVersion <- either (panic . show) id <$> P.use pool (queryPgVersion False)

  -- cached schema cache so most tests run fast
  baseSchemaCache <- loadSCache pool testCfg
  loggerState <- Logger.init

  let
    initApp sCache config tracer = do
      -- duplicate poolChan as a starting point
      obsChan <- dupChan poolChan
      stateObsChan <- newObsChan obsChan
      appState <- AppState.initWithPool pool config loggerState metricsState tracer (Metrics.observationMetrics metricsState <> writeChan obsChan)
      AppState.putPgVersion appState actualPgVersion
      AppState.putSchemaCache appState (Just sCache)
      return (SpecState appState metricsState stateObsChan, postgrest (configLogLevel config) appState (pure ()))

    initJwtApp = initApp baseSchemaCache testCfgJwtCache Nothing

    initMetricsApp = initApp baseSchemaCache testCfg Nothing

    -- Dedicated initializer for the OTel spec: start collector, configure env, then create OTel-enabled app.
    initOTelApp :: ActionWith (Maybe Collector, Application) -> IO ()
    initOTelApp action = do
      mCollectorBin <- findExecutable "otelcol"
      case mCollectorBin of
        Nothing -> do
          -- app is still initialized so the state type remains consistent; the spec will mark itself pending.
          (_, app) <- initApp baseSchemaCache testCfgOTel Nothing
          action (Nothing, app)
        Just collectorBin -> do
          tmpDir <- (`createTempDirectory` "pgrst-otel-") =<< getCanonicalTemporaryDirectory
          bracket
            (startCollector tmpDir collectorBin)
            stopCollector
            (\collector -> do
              configureOTelEnv (collectorEndpoint collector)
              withTracer $ \tracer -> do
                (_, app) <- initApp baseSchemaCache testCfgOTel (Just tracer)
                action (Just collector, app)
            )

  -- Run all test modules
  hspec $ do
    before initJwtApp $
      describe "Observation.JwtCacheObs" Observation.JwtCache.spec
    before initMetricsApp $
      describe "Feature.MetricsSpec" Observation.MetricsSpec.spec
    before (initApp baseSchemaCache testCfg Nothing) $
      describe "Feature.SchemaCacheSpec" Observation.SchemaCacheSpec.spec

    around initOTelApp $
      describe "Observation.OpenTelemetry" Observation.OpenTelemetry.spec

  where
    loadSCache pool conf =
      either (panic.show) id <$> P.use pool (HT.transaction HT.ReadCommitted HT.Read $ querySchemaCache conf)
