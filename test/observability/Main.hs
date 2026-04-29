module Main where

import qualified Hasql.Pool                 as P
import qualified Hasql.Pool.Config          as P
import qualified Hasql.Transaction.Sessions as HT

import Data.Function (id)

import           PostgREST.App             (postgrest)
import qualified PostgREST.AppState        as AppState
import           PostgREST.Config          (AppConfig (..),
                                            toConnectionSettings)
import           PostgREST.Config.Database (queryPgVersion)
import qualified PostgREST.Logger          as Logger
import qualified PostgREST.Metrics         as Metrics
import           PostgREST.SchemaCache     (querySchemaCache)

import qualified Observation.JwtCache
import qualified Observation.MetricsSpec

import qualified Observation.ListenerSpec
import qualified Observation.SchemaCacheSpec
import           ObsHelper
import           PostgREST.Observation       (Observation (HasqlPoolObs))
import           Protolude                   hiding (toList, toS)
import           Test.Hspec

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
    , P.staticConnectionSettings $ toConnectionSettings identity testCfg
    -- make sure metrics are updated and pool observations published to poolChan
    , P.observationHandler $ (writeChan poolChan <> Metrics.observationMetrics metricsState) . HasqlPoolObs
    ]

  actualPgVersion <- either (panic . show) id <$> P.use pool queryPgVersion

  -- cached schema cache so most tests run fast
  baseSchemaCache <- loadSCache pool testCfg
  loggerState <- Logger.init

  let
    initApp sCache config = do
      -- duplicate poolChan as a starting point
      obsChan <- dupChan poolChan
      stateObsChan <- newObsChan obsChan
      appState <- AppState.initWithPool pool config loggerState metricsState (Metrics.observationMetrics metricsState <> writeChan obsChan)
      AppState.putPgVersion appState actualPgVersion
      AppState.putSchemaCache appState (Just sCache)
      return (SpecState appState metricsState stateObsChan, postgrest appState (pure ()))

  -- Run all test modules
  hspec $ do
    before (initApp baseSchemaCache testCfgJwtCache) $
      describe "Observation.JwtCacheObs" Observation.JwtCache.spec
    before (initApp baseSchemaCache testCfg) $
      describe "Feature.MetricsSpec" Observation.MetricsSpec.spec
    before (initApp baseSchemaCache testCfg) $
      describe "Feature.SchemaCacheSpec" Observation.SchemaCacheSpec.spec
    before (initApp baseSchemaCache testCfg) $
      describe "Observation.ListenerSpec" Observation.ListenerSpec.spec

  where
    loadSCache pool conf =
      either (panic.show) id <$> P.use pool (HT.transaction HT.ReadCommitted HT.Read $ querySchemaCache conf)
