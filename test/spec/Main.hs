module Main where

import qualified Hasql.Pool                 as P
import qualified Hasql.Pool.Config          as P
import qualified Hasql.Transaction.Sessions as HT

import Data.Function (id)

import Test.Hspec

import PostgREST.App             (postgrest)
import PostgREST.Config          (AppConfig (..),
                                  toConnectionSettings)
import PostgREST.Config.Database (queryPgVersion)
import PostgREST.SchemaCache     (querySchemaCache)
import Protolude                 hiding (toList, toS)
import SpecHelper

import qualified PostgREST.AppState as AppState
import qualified PostgREST.Logger   as Logger
import qualified PostgREST.Metrics  as Metrics

import qualified Feature.Auth.AsymmetricJwtSpec
import qualified Feature.Auth.AudienceJwtSecretSpec
import qualified Feature.Auth.AuthSpec
import qualified Feature.Auth.BinaryJwtSecretSpec
import qualified Feature.Auth.NoAnonSpec
import qualified Feature.Auth.NoJwtSecretSpec
import qualified Feature.ConcurrentSpec
import qualified Feature.CorsSpec
import qualified Feature.ExtraSearchPathSpec
import qualified Feature.NoSuperuserSpec
import qualified Feature.ObservabilitySpec
import qualified Feature.OpenApi.DisabledOpenApiSpec
import qualified Feature.OpenApi.IgnorePrivOpenApiSpec
import qualified Feature.OpenApi.OpenApiSpec
import qualified Feature.OpenApi.ProxySpec
import qualified Feature.OpenApi.RootSpec
import qualified Feature.OpenApi.SecurityOpenApiSpec
import qualified Feature.OptionsSpec
import qualified Feature.Query.AggregateFunctionsSpec
import qualified Feature.Query.AndOrParamsSpec
import qualified Feature.Query.ComputedRelsSpec
import qualified Feature.Query.CustomMediaSpec
import qualified Feature.Query.DeleteSpec
import qualified Feature.Query.EmbedDisambiguationSpec
import qualified Feature.Query.EmbedInnerJoinSpec
import qualified Feature.Query.ErrorSpec
import qualified Feature.Query.InsertSpec
import qualified Feature.Query.JsonOperatorSpec
import qualified Feature.Query.MultipleSchemaSpec
import qualified Feature.Query.NullsStripSpec
import qualified Feature.Query.PgSafeUpdateSpec
import qualified Feature.Query.PlanSpec
import qualified Feature.Query.PostGISSpec
import qualified Feature.Query.Preferences.HandlingSpec
import qualified Feature.Query.Preferences.MaxAffectedSpec
import qualified Feature.Query.Preferences.TimezoneSpec
import qualified Feature.Query.QueryLimitedSpec
import qualified Feature.Query.QuerySpec
import qualified Feature.Query.RangeSpec
import qualified Feature.Query.RawOutputTypesSpec
import qualified Feature.Query.RelatedQueriesSpec
import qualified Feature.Query.RpcSpec
import qualified Feature.Query.ServerTimingSpec
import qualified Feature.Query.SingularSpec
import qualified Feature.Query.SpreadQueriesSpec
import qualified Feature.Query.UnicodeSpec
import qualified Feature.Query.UpdateSpec
import qualified Feature.Query.UpsertSpec
import qualified Feature.RollbackSpec
import qualified Feature.RpcPreRequestGucsSpec


main :: IO ()
main = do
  pool <- P.acquire $ P.settings
    [ P.size 3
    , P.acquisitionTimeout 10
    , P.agingTimeout 60
    , P.idlenessTimeout 60
    , P.staticConnectionSettings $ toConnectionSettings identity baseCfg
    ]

  actualPgVersion <- either (panic . show) id <$> P.use pool queryPgVersion

  -- cached schema cache so most tests run fast
  baseSchemaCache <- loadSCache pool baseCfg
  loggerState <- Logger.init
  metricsState <- Metrics.init (configDbPoolSize baseCfg)

  let
    initApp sCache config = do
      appState <- AppState.initWithPool pool config loggerState metricsState (Metrics.observationMetrics metricsState)
      AppState.putPgVersion appState actualPgVersion
      AppState.putSchemaCache appState (Just sCache)
      return ((), postgrest appState (pure ()))

    -- For tests that run with the same schema cache
    app = initApp baseSchemaCache

    -- For tests that run with a different SchemaCache (depends on configSchemas)
    appDbs config = do
      customSchemaCache <- loadSCache pool config
      initApp customSchemaCache config

    withConfig config = before (app config)
    withConfigDbs config = before (appDbs config)
    describeWithConfig label spec = describe label $ spec withConfig

  let specs = uncurry describeWithConfig <$> [
          ("Feature.Auth.AsymmetricJwtSpec"                    , Feature.Auth.AsymmetricJwtSpec.spec)
        , ("Feature.Auth.AudienceJwtSecretSpec"                , Feature.Auth.AudienceJwtSecretSpec.disabledSpec)
        , ("Feature.Auth.AudienceJwtSecretSpec"                , Feature.Auth.AudienceJwtSecretSpec.spec)
        , ("Feature.Auth.AuthSpec"                             , Feature.Auth.AuthSpec.spec)
        , ("Feature.Auth.BinaryJwtSecretSpec"                  , Feature.Auth.BinaryJwtSecretSpec.spec)
        , ("Feature.Auth.NoAnonSpec"                           , Feature.Auth.NoAnonSpec.spec)
        , ("Feature.Auth.NoJwtSecretSpec"                      , Feature.Auth.NoJwtSecretSpec.spec)
        , ("Feature.ConcurrentSpec"                            , Feature.ConcurrentSpec.spec)
        , ("Feature.CorsSpec"                                  , Feature.CorsSpec.spec)
        , ("Feature.NoSuperuserSpec"                           , Feature.NoSuperuserSpec.spec)
        , ("Feature.ObservabilitySpec"                         , Feature.ObservabilitySpec.spec)
        , ("Feature.OpenApi.DisabledOpenApiSpec"               , Feature.OpenApi.DisabledOpenApiSpec.spec)
        , ("Feature.OpenApi.OpenApiSpec"                       , Feature.OpenApi.OpenApiSpec.spec)
        , ("Feature.OpenApi.ProxySpec"                         , Feature.OpenApi.ProxySpec.spec)
        , ("Feature.OpenApi.RootSpec"                          , Feature.OpenApi.RootSpec.spec)
        , ("Feature.OpenApi.SecurityOpenApiSpec"               , Feature.OpenApi.SecurityOpenApiSpec.spec)
        , ("Feature.OptionsSpec"                               , Feature.OptionsSpec.spec)
        , ("Feature.Query.AggregateFunctionsSpec.allowed"      , Feature.Query.AggregateFunctionsSpec.allowed)
        , ("Feature.Query.AggregateFunctionsSpec.disallowed"   , Feature.Query.AggregateFunctionsSpec.disallowed)
        , ("Feature.Query.AndOrParamsSpec"                     , Feature.Query.AndOrParamsSpec.spec)
        , ("Feature.Query.ComputedRelsSpec"                    , Feature.Query.ComputedRelsSpec.spec)
        , ("Feature.Query.CustomMediaSpec"                     , Feature.Query.CustomMediaSpec.spec)
        , ("Feature.Query.DeleteSpec"                          , Feature.Query.DeleteSpec.spec)
        , ("Feature.Query.EmbedDisambiguationSpec"             , Feature.Query.EmbedDisambiguationSpec.spec)
        , ("Feature.Query.EmbedInnerJoinSpec"                  , Feature.Query.EmbedInnerJoinSpec.spec)
        , ("Feature.Query.ErrorSpec.pgErrorCodeMapping"        , Feature.Query.ErrorSpec.pgErrorCodeMapping)
        , ("Feature.Query.InsertSpec"                          , Feature.Query.InsertSpec.spec)
        , ("Feature.Query.JsonOperatorSpec"                    , Feature.Query.JsonOperatorSpec.spec)
        , ("Feature.Query.NullsStripSpec"                      , Feature.Query.NullsStripSpec.spec)
        , ("Feature.Query.PgSafeUpdateSpec.disabledSpec"       , Feature.Query.PgSafeUpdateSpec.disabledSpec)
        , ("Feature.Query.PlanSpec.disabledSpec"               , Feature.Query.PlanSpec.disabledSpec)
        , ("Feature.Query.PlanSpec.spec"                       , Feature.Query.PlanSpec.spec)
        , ("Feature.Query.Preferences.HandlingSpec"            , Feature.Query.Preferences.HandlingSpec.spec)
        , ("Feature.Query.Preferences.MaxAffectedSpec"         , Feature.Query.Preferences.MaxAffectedSpec.spec)
        , ("Feature.Query.Preferences.TimezoneSpec.enabledSpec", Feature.Query.Preferences.TimezoneSpec.enabledSpec)
        , ("Feature.Query.QueryLimitedSpec"                    , Feature.Query.QueryLimitedSpec.spec)
        , ("Feature.Query.QuerySpec"                           , Feature.Query.QuerySpec.spec)
        , ("Feature.Query.RangeSpec"                           , Feature.Query.RangeSpec.spec)
        , ("Feature.Query.RawOutputTypesSpec"                  , Feature.Query.RawOutputTypesSpec.spec)
        , ("Feature.Query.RelatedQueriesSpec"                  , Feature.Query.RelatedQueriesSpec.spec)
        , ("Feature.Query.RpcSpec"                             , Feature.Query.RpcSpec.spec actualPgVersion)
        , ("Feature.Query.ServerTimingSpec"                    , Feature.Query.ServerTimingSpec.spec)
        , ("Feature.Query.SingularSpec"                        , Feature.Query.SingularSpec.spec)
        , ("Feature.Query.SpreadQueriesSpec"                   , Feature.Query.SpreadQueriesSpec.spec)
        , ("Feature.Query.UpdateSpec"                          , Feature.Query.UpdateSpec.spec)
        , ("Feature.Query.UpsertSpec"                          , Feature.Query.UpsertSpec.spec)
        , ("Feature.RpcPreRequestGucsSpec"                     , Feature.RpcPreRequestGucsSpec.spec)
        ]

  hspec $ do
    mapM_ parallel specs

    parallel $ describe "Feature.Query.UnicodeSpec" $ Feature.Query.UnicodeSpec.spec withConfigDbs
    parallel $ describe "Feature.OpenApi.IgnorePrivOpenApiSpec" $ Feature.OpenApi.IgnorePrivOpenApiSpec.spec withConfigDbs
    parallel $ describe "Feature.ExtraSearchPathSpec" $ Feature.ExtraSearchPathSpec.spec withConfigDbs
    parallel $ describe "Feature.Query.PostGISSpec" $ Feature.Query.PostGISSpec.spec withConfigDbs
    parallel $ describe "Feature.Query.MultipleSchemaSpec" $ Feature.Query.MultipleSchemaSpec.spec withConfigDbs
    parallel $ describe "Feature.Query.Preferences.TimezoneSpec.disabledSpec" $ Feature.Query.Preferences.TimezoneSpec.disabledSpec withConfigDbs

    -- Note: the rollback tests can not run in parallel, because they test persistence and
    -- this results in race conditions
    describe "Feature.RollbackAllowedSpec" $ Feature.RollbackSpec.allowed withConfig
    describe "Feature.RollbackDisallowedSpec" $ Feature.RollbackSpec.disallowed withConfig
    describe "Feature.RollbackForcedSpec" $ Feature.RollbackSpec.forced withConfig

    -- This test runs with a pre request to enable the pg-safeupdate library per-session.
    -- This needs to run last, because once pg safe update is loaded, it can't be unloaded again.
    describe "Feature.Query.PgSafeUpdateSpec.spec" $ Feature.Query.PgSafeUpdateSpec.spec withConfig

  where
    loadSCache pool conf =
      either (panic.show) id <$> P.use pool (HT.transaction HT.ReadCommitted HT.Read $ querySchemaCache conf)
