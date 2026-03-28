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
import qualified Observation.OpenTelemetry

import ObsHelper
import Protolude  hiding (toList)
import Test.Hspec

main :: IO ()
main = do
  pool <- P.acquire $ P.settings
    [ P.size 3
    , P.acquisitionTimeout 10
    , P.agingTimeout 60
    , P.idlenessTimeout 60
    , P.staticConnectionSettings (toUtf8 $ configDbUri testCfg)
    ]

  actualPgVersion <- either (panic . show) id <$> P.use pool (queryPgVersion False)

  -- cached schema cache so most tests run fast
  baseSchemaCache <- loadSCache pool testCfg
  loggerState <- Logger.init
  metricsState <- Metrics.init (configDbPoolSize testCfg)
  let observer = Metrics.observationMetrics metricsState

  let
    initApp sCache st config tracer = do
      appState <- AppState.initWithPool pool config loggerState metricsState tracer observer
      AppState.putPgVersion appState actualPgVersion
      AppState.putSchemaCache appState (Just sCache)
      return (st, postgrest (configLogLevel config) appState (pure ()))

    initJwtApp = initApp baseSchemaCache metricsState testCfgJwtCache Nothing

    -- Dedicated initializer for the OTel spec: start collector, configure env, then create OTel-enabled app.
    initOTelApp :: ActionWith (Maybe Collector, Application) -> IO ()
    initOTelApp action = do
      mCollectorBin <- findExecutable "otelcol"
      case mCollectorBin of
        Nothing -> do
          -- app is still initialized so the state type remains consistent; the spec will mark itself pending.
          app <- initApp baseSchemaCache Nothing testCfgOTel Nothing
          action app
        Just collectorBin ->
          bracket
            (startCollector collectorBin)
            stopCollector
            (\collector -> do
              configureOTelEnv (collectorEndpoint collector)
              withTracer $ \tracer -> do
                app <- initApp baseSchemaCache (Just collector) testCfgOTel (Just tracer)
                action app
            )

  -- Run all test modules
  hspec $ do
    before initJwtApp $
      describe "Observation.JwtCacheObs" Observation.JwtCache.spec

    around initOTelApp $
      describe "Observation.OpenTelemetry" Observation.OpenTelemetry.spec

  where
    loadSCache pool conf =
      either (panic.show) id <$> P.use pool (HT.transaction HT.ReadCommitted HT.Read $ querySchemaCache conf)
