module Main where

import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction.Sessions as HT

import Data.Function (id)

import Test.Hspec

import PostgREST.App             (postgrest)
import PostgREST.Config          (AppConfig (..))
import PostgREST.Config.Database (queryPgVersion)
import PostgREST.SchemaCache     (querySchemaCache)
import Protolude                 hiding (toList, toS)
import SpecHelper

import qualified PostgREST.AppState as AppState

import qualified Feature.Auth.AsymmetricJwtSpec
import qualified Feature.Auth.AudienceJwtSecretSpec
import qualified Feature.Auth.AuthSpec
import qualified Feature.Auth.BinaryJwtSecretSpec
import qualified Feature.Auth.NoAnonSpec
import qualified Feature.Auth.NoJwtSpec
import qualified Feature.ConcurrentSpec
import qualified Feature.CorsSpec
import qualified Feature.ExtraSearchPathSpec
import qualified Feature.LegacyGucsSpec
import qualified Feature.NoSuperuserSpec
import qualified Feature.ObservabilitySpec
import qualified Feature.OpenApi.DisabledOpenApiSpec
import qualified Feature.OpenApi.IgnorePrivOpenApiSpec
import qualified Feature.OpenApi.OpenApiSpec
import qualified Feature.OpenApi.ProxySpec
import qualified Feature.OpenApi.RootSpec
import qualified Feature.OpenApi.SecurityOpenApiSpec
import qualified Feature.OptionsSpec
import qualified Feature.Query.AndOrParamsSpec
import qualified Feature.Query.ComputedRelsSpec
import qualified Feature.Query.DeleteSpec
import qualified Feature.Query.EmbedDisambiguationSpec
import qualified Feature.Query.EmbedInnerJoinSpec
import qualified Feature.Query.ErrorSpec
import qualified Feature.Query.HtmlRawOutputSpec
import qualified Feature.Query.InsertSpec
import qualified Feature.Query.JsonOperatorSpec
import qualified Feature.Query.MultipleSchemaSpec
import qualified Feature.Query.PgSafeUpdateSpec
import qualified Feature.Query.PlanSpec
import qualified Feature.Query.PostGISSpec
import qualified Feature.Query.QueryLimitedSpec
import qualified Feature.Query.QuerySpec
import qualified Feature.Query.RangeSpec
import qualified Feature.Query.RawOutputTypesSpec
import qualified Feature.Query.RelatedQueriesSpec
import qualified Feature.Query.RpcSpec
import qualified Feature.Query.SingularSpec
import qualified Feature.Query.SpreadQueriesSpec
import qualified Feature.Query.UnicodeSpec
import qualified Feature.Query.UpdateSpec
import qualified Feature.Query.UpsertSpec
import qualified Feature.RollbackSpec
import qualified Feature.RpcPreRequestGucsSpec


main :: IO ()
main = do
  pool <- P.acquire 3 10 60 60 $ toUtf8 $ configDbUri testCfg

  actualPgVersion <- either (panic . show) id <$> P.use pool (queryPgVersion False)

  -- cached schema cache so most tests run fast
  baseSchemaCache <- loadSchemaCache pool testCfg

  let
    -- For tests that run with the same refSchemaCache
    app config = do
      appState <- AppState.initWithPool pool config
      AppState.putPgVersion appState actualPgVersion
      AppState.putSchemaCache appState (Just baseSchemaCache)
      return ((), postgrest config appState $ pure ())

    -- For tests that run with a different SchemaCache(depends on configSchemas)
    appDbs config = do
      customSchemaCache <- loadSchemaCache pool config
      appState <- AppState.initWithPool pool config
      AppState.putPgVersion appState actualPgVersion
      AppState.putSchemaCache appState (Just customSchemaCache)
      return ((), postgrest config appState $ pure ())

  let withApp              = app testCfg
      maxRowsApp           = app testMaxRowsCfg
      disabledOpenApi      = app testDisabledOpenApiCfg
      securityOpenApi      = app testSecurityOpenApiCfg
      proxyApp             = app testProxyCfg
      noAnonApp            = app testCfgNoAnon
      noJwtApp             = app testCfgNoJWT
      binaryJwtApp         = app testCfgBinaryJWT
      audJwtApp            = app testCfgAudienceJWT
      asymJwkApp           = app testCfgAsymJWK
      asymJwkSetApp        = app testCfgAsymJWKSet
      rootSpecApp          = app testCfgRootSpec
      htmlRawOutputApp     = app testCfgHtmlRawOutput
      responseHeadersApp   = app testCfgResponseHeaders
      disallowRollbackApp  = app testCfgDisallowRollback
      forceRollbackApp     = app testCfgForceRollback
      testCfgLegacyGucsApp = app testCfgLegacyGucs
      planEnabledApp       = app testPlanEnabledCfg
      pgSafeUpdateApp      = app testPgSafeUpdateEnabledCfg
      obsApp               = app testObservabilityCfg

      extraSearchPathApp   = appDbs testCfgExtraSearchPath
      unicodeApp           = appDbs testUnicodeCfg
      nonexistentSchemaApp = appDbs testNonexistentSchemaCfg
      multipleSchemaApp    = appDbs testMultipleSchemaCfg
      ignorePrivOpenApi    = appDbs testIgnorePrivOpenApiCfg


  let analyze :: IO ()
      analyze = do
        analyzeTable "items"
        analyzeTable "child_entities"

      specs = uncurry describe <$> [
          ("Feature.Query.AndOrParamsSpec"               , Feature.Query.AndOrParamsSpec.spec actualPgVersion)
        , ("Feature.Auth.AuthSpec"                       , Feature.Auth.AuthSpec.spec actualPgVersion)
        , ("Feature.ConcurrentSpec"                      , Feature.ConcurrentSpec.spec)
        , ("Feature.CorsSpec"                            , Feature.CorsSpec.spec)
        , ("Feature.Query.DeleteSpec"                    , Feature.Query.DeleteSpec.spec)
        , ("Feature.Query.EmbedDisambiguationSpec"       , Feature.Query.EmbedDisambiguationSpec.spec)
        , ("Feature.Query.EmbedInnerJoinSpec"            , Feature.Query.EmbedInnerJoinSpec.spec)
        , ("Feature.Query.InsertSpec"                    , Feature.Query.InsertSpec.spec actualPgVersion)
        , ("Feature.Query.JsonOperatorSpec"              , Feature.Query.JsonOperatorSpec.spec actualPgVersion)
        , ("Feature.OpenApi.OpenApiSpec"                 , Feature.OpenApi.OpenApiSpec.spec actualPgVersion)
        , ("Feature.OptionsSpec"                         , Feature.OptionsSpec.spec actualPgVersion)
        , ("Feature.Query.PgSafeUpdateSpec.disabledSpec" , Feature.Query.PgSafeUpdateSpec.disabledSpec)
        , ("Feature.Query.PlanSpec.disabledSpec"         , Feature.Query.PlanSpec.disabledSpec)
        , ("Feature.Query.QuerySpec"                     , Feature.Query.QuerySpec.spec actualPgVersion)
        , ("Feature.Query.RawOutputTypesSpec"            , Feature.Query.RawOutputTypesSpec.spec)
        , ("Feature.Query.RpcSpec"                       , Feature.Query.RpcSpec.spec actualPgVersion)
        , ("Feature.Query.SingularSpec"                  , Feature.Query.SingularSpec.spec)
        , ("Feature.Query.UpdateSpec"                    , Feature.Query.UpdateSpec.spec actualPgVersion)
        , ("Feature.Query.UpsertSpec"                    , Feature.Query.UpsertSpec.spec actualPgVersion)
        , ("Feature.Query.ComputedRelsSpec"              , Feature.Query.ComputedRelsSpec.spec)
        , ("Feature.Query.RelatedQueriesSpec"            , Feature.Query.RelatedQueriesSpec.spec)
        , ("Feature.Query.SpreadQueriesSpec"             , Feature.Query.SpreadQueriesSpec.spec)
        , ("Feature.NoSuperuserSpec"                     , Feature.NoSuperuserSpec.spec)
        ]

  hspec $ do
    mapM_ (parallel . before withApp) specs

    -- we analyze to get accurate results from EXPLAIN
    parallel $ beforeAll_ analyze . before withApp $
      describe "Feature.Query.RangeSpec" Feature.Query.RangeSpec.spec

    -- this test runs with a raw-output-media-types set to text/html
    parallel $ before htmlRawOutputApp $
      describe "Feature.Query.HtmlRawOutputSpec" Feature.Query.HtmlRawOutputSpec.spec

    -- this test runs with a different server flag
    parallel $ before maxRowsApp $
      describe "Feature.Query.QueryLimitedSpec" Feature.Query.QueryLimitedSpec.spec

    -- this test runs with a different schema
    parallel $ before unicodeApp $
      describe "Feature.Query.UnicodeSpec" Feature.Query.UnicodeSpec.spec

    -- this test runs with openapi-mode set to disabled
    parallel $ before disabledOpenApi $
      describe "Feature.DisabledOpenApiSpec" Feature.OpenApi.DisabledOpenApiSpec.spec

    -- this test runs with openapi-mode set to ignore-acl
    parallel $ before ignorePrivOpenApi $
      describe "Feature.OpenApi.IgnorePrivOpenApiSpec" Feature.OpenApi.IgnorePrivOpenApiSpec.spec

    -- this test runs with a proxy
    parallel $ before proxyApp $
      describe "Feature.OpenApi.ProxySpec" Feature.OpenApi.ProxySpec.spec

    -- this test runs with openapi-security-active set to true
    parallel $ before securityOpenApi $
      describe "Feature.OpenApi.SecurityOpenApiSpec" Feature.OpenApi.SecurityOpenApiSpec.spec

    -- this test runs without an anonymous role
    parallel $ before noAnonApp $
      describe "Feature.Auth.NoAnonSpec" Feature.Auth.NoAnonSpec.spec

    -- this test runs without a JWT secret
    parallel $ before noJwtApp $
      describe "Feature.Auth.NoJwtSpec" Feature.Auth.NoJwtSpec.spec

    -- this test runs with a binary JWT secret
    parallel $ before binaryJwtApp $
      describe "Feature.Auth.BinaryJwtSecretSpec" Feature.Auth.BinaryJwtSecretSpec.spec

    -- this test runs with a binary JWT secret and an audience claim
    parallel $ before audJwtApp $
      describe "Feature.Auth.AudienceJwtSecretSpec" Feature.Auth.AudienceJwtSecretSpec.spec

    -- this test runs with asymmetric JWK
    parallel $ before asymJwkApp $
      describe "Feature.Auth.AsymmetricJwtSpec" Feature.Auth.AsymmetricJwtSpec.spec

    -- this test runs with asymmetric JWKSet
    parallel $ before asymJwkSetApp $
      describe "Feature.Auth.AsymmetricJwtSpec" Feature.Auth.AsymmetricJwtSpec.spec

    -- this test runs with a nonexistent db-schema
    parallel $ before nonexistentSchemaApp $
      describe "Feature.Query.ErrorSpec" Feature.Query.ErrorSpec.spec

    -- this test runs with an extra search path
    parallel $ before extraSearchPathApp $ do
      describe "Feature.ExtraSearchPathSpec" Feature.ExtraSearchPathSpec.spec
      describe "Feature.Query.PostGISSpec" $ Feature.Query.PostGISSpec.spec actualPgVersion

    -- this test runs with a root spec function override
    parallel $ before rootSpecApp $
      describe "Feature.OpenApi.RootSpec" Feature.OpenApi.RootSpec.spec

    -- this test runs with a pre request function override
    parallel $ before responseHeadersApp $
      describe "Feature.RpcPreRequestGucsSpec" Feature.RpcPreRequestGucsSpec.spec

    -- this test runs with multiple schemas
    parallel $ before multipleSchemaApp $
      describe "Feature.Query.MultipleSchemaSpec" Feature.Query.MultipleSchemaSpec.spec

    -- this test runs with db-uses-legacy-gucs = false
    parallel $ before testCfgLegacyGucsApp $
      describe "Feature.LegacyGucsSpec" Feature.LegacyGucsSpec.spec

    -- this test runs with db-plan-enabled = true
    parallel $ before planEnabledApp $
      describe "Feature.Query.PlanSpec.spec" $ Feature.Query.PlanSpec.spec actualPgVersion

    -- this test runs with a pre request to enable the pg-safeupdate library per-session
    parallel $ before pgSafeUpdateApp $
      describe "Feature.Query.PgSafeUpdateSpec.spec" Feature.Query.PgSafeUpdateSpec.spec

    -- this test runs with server-trace-header set
    parallel $ before obsApp $
      describe "Feature.ObservabilitySpec.spec" Feature.ObservabilitySpec.spec

    -- Note: the rollback tests can not run in parallel, because they test persistance and
    -- this results in race conditions

    -- this test runs with tx-rollback-all = true and tx-allow-override = true
    before withApp $
      describe"Feature.RollbackAllowedSpec" Feature.RollbackSpec.allowed

    -- this test runs with tx-rollback-all = false and tx-allow-override = false
    before disallowRollbackApp $
      describe "Feature.RollbackDisallowedSpec" Feature.RollbackSpec.disallowed

    -- this test runs with tx-rollback-all = true and tx-allow-override = false
    before forceRollbackApp $
      describe "Feature.RollbackForcedSpec" Feature.RollbackSpec.forced

  where
    loadSchemaCache pool conf =
      either (panic.show) id <$> P.use pool (HT.transaction HT.ReadCommitted HT.Read $ querySchemaCache conf)
