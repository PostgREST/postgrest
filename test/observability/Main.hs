module Main where

import qualified Hasql.Pool                 as P
import qualified Hasql.Pool.Config          as P
import qualified Hasql.Transaction.Sessions as HT

import Data.Function (id)

import           PostgREST.App             (postgrest)
import qualified PostgREST.AppState        as AppState
import           PostgREST.Config          (AppConfig (..))
import           PostgREST.Config.Database (queryPgVersion)
import qualified PostgREST.Logger          as Logger
import qualified PostgREST.Metrics         as Metrics
import           PostgREST.SchemaCache     (querySchemaCache)

import qualified Observation.JwtCache

import ObsHelper
import Protolude  hiding (toList, toS)
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
  sockets <- AppState.initSockets testCfg
  loggerState <- Logger.init
  metricsState <- Metrics.init (configDbPoolSize testCfg)

  let
    initApp sCache st config = do
      appState <- AppState.initWithPool sockets pool config loggerState metricsState (Metrics.observationMetrics metricsState)
      AppState.putPgVersion appState actualPgVersion
      AppState.putSchemaCache appState (Just sCache)
      return (st, postgrest (configLogLevel config) appState (pure ()))

  -- Run all test modules
  hspec $ do
    before (initApp baseSchemaCache metricsState testCfgJwtCache) $
      describe "Observation.JwtCacheObs" Observation.JwtCache.spec

  where
    loadSCache pool conf =
      either (panic.show) id <$> P.use pool (HT.transaction HT.ReadCommitted HT.Read $ querySchemaCache conf)
