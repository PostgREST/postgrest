module Main where

import qualified Data.Aeson                 as JSON
import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction.Sessions as HT

import Data.Function      (id)
import Data.List.NonEmpty (toList)

import Test.Hspec

import PostgREST.App             (postgrest)
import PostgREST.Config          (AppConfig (..), LogLevel (..))
import PostgREST.Config.Database (queryPgVersion)
import PostgREST.DbStructure     (queryDbStructure)
import Protolude                 hiding (toList, toS)
import Protolude.Conv            (toS)
import SpecHelper

import qualified PostgREST.AppState as AppState

import qualified Feature.AndOrParamsSpec
import qualified Feature.AsymmetricJwtSpec
import qualified Feature.AudienceJwtSecretSpec
import qualified Feature.AuthSpec
import qualified Feature.BinaryJwtSecretSpec
import qualified Feature.ConcurrentSpec
import qualified Feature.CorsSpec
import qualified Feature.DeleteSpec
import qualified Feature.DisabledOpenApiSpec
import qualified Feature.EmbedDisambiguationSpec
import qualified Feature.EmbedInnerJoinSpec
import qualified Feature.ExtraSearchPathSpec
import qualified Feature.HtmlRawOutputSpec
import qualified Feature.IgnorePrivOpenApiSpec
import qualified Feature.InsertSpec
import qualified Feature.JsonOperatorSpec
import qualified Feature.LegacyGucsSpec
import qualified Feature.MultipleSchemaSpec
import qualified Feature.NoJwtSpec
import qualified Feature.NonexistentSchemaSpec
import qualified Feature.OpenApiSpec
import qualified Feature.OptionsSpec
import qualified Feature.ProxySpec
import qualified Feature.QueryLimitedSpec
import qualified Feature.QuerySpec
import qualified Feature.RangeSpec
import qualified Feature.RawOutputTypesSpec
import qualified Feature.RollbackSpec
import qualified Feature.RootSpec
import qualified Feature.RpcPreRequestGucsSpec
import qualified Feature.RpcSpec
import qualified Feature.SingularSpec
import qualified Feature.UnicodeSpec
import qualified Feature.UpdateSpec
import qualified Feature.UpsertSpec


main :: IO ()
main = do
  testDbConn <- getEnvVarWithDefault "PGRST_DB_URI" "postgres://postgrest_test@localhost/postgrest_test"

  pool <- P.acquire (3, 10, toS testDbConn)

  actualPgVersion <- either (panic.show) id <$> P.use pool queryPgVersion

  baseDbStructure <-
    loadDbStructure pool
      (configDbSchemas $ testCfg testDbConn)
      (configDbExtraSearchPath $ testCfg testDbConn)

  let
    -- For tests that run with the same refDbStructure
    app cfg = do
      let config = cfg testDbConn
      appState <- AppState.initWithPool pool config
      AppState.putPgVersion appState actualPgVersion
      AppState.putDbStructure appState baseDbStructure
      when (isJust $ configDbRootSpec config) $
        AppState.putJsonDbS appState $ toS $ JSON.encode baseDbStructure
      return ((), postgrest LogCrit appState $ pure ())

    -- For tests that run with a different DbStructure(depends on configSchemas)
    appDbs cfg = do
      let config = cfg testDbConn
      customDbStructure <-
        loadDbStructure pool
          (configDbSchemas config)
          (configDbExtraSearchPath config)
      appState <- AppState.initWithPool pool config
      AppState.putPgVersion appState actualPgVersion
      AppState.putDbStructure appState customDbStructure
      when (isJust $ configDbRootSpec config) $
        AppState.putJsonDbS appState $ toS $ JSON.encode baseDbStructure
      return ((), postgrest LogCrit appState $ pure ())

  let withApp              = app testCfg
      maxRowsApp           = app testMaxRowsCfg
      disabledOpenApi      = app testDisabledOpenApiCfg
      proxyApp             = app testProxyCfg
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

      extraSearchPathApp   = appDbs testCfgExtraSearchPath
      unicodeApp           = appDbs testUnicodeCfg
      nonexistentSchemaApp = appDbs testNonexistentSchemaCfg
      multipleSchemaApp    = appDbs testMultipleSchemaCfg
      ignorePrivOpenApi    = appDbs testIgnorePrivOpenApiCfg

  let analyze :: IO ()
      analyze = do
        analyzeTable testDbConn "items"
        analyzeTable testDbConn "child_entities"

      specs = uncurry describe <$> [
          ("Feature.AndOrParamsSpec"         , Feature.AndOrParamsSpec.spec actualPgVersion)
        , ("Feature.AuthSpec"                , Feature.AuthSpec.spec actualPgVersion)
        , ("Feature.ConcurrentSpec"          , Feature.ConcurrentSpec.spec)
        , ("Feature.CorsSpec"                , Feature.CorsSpec.spec)
        , ("Feature.DeleteSpec"              , Feature.DeleteSpec.spec)
        , ("Feature.EmbedDisambiguationSpec" , Feature.EmbedDisambiguationSpec.spec)
        , ("Feature.EmbedInnerJoinSpec"      , Feature.EmbedInnerJoinSpec.spec)
        , ("Feature.InsertSpec"              , Feature.InsertSpec.spec actualPgVersion)
        , ("Feature.JsonOperatorSpec"        , Feature.JsonOperatorSpec.spec actualPgVersion)
        , ("Feature.OpenApiSpec"             , Feature.OpenApiSpec.spec actualPgVersion)
        , ("Feature.OptionsSpec"             , Feature.OptionsSpec.spec actualPgVersion)
        , ("Feature.QuerySpec"               , Feature.QuerySpec.spec actualPgVersion)
        , ("Feature.RawOutputTypesSpec"      , Feature.RawOutputTypesSpec.spec)
        , ("Feature.RpcSpec"                 , Feature.RpcSpec.spec actualPgVersion)
        , ("Feature.SingularSpec"            , Feature.SingularSpec.spec)
        , ("Feature.UpdateSpec"              , Feature.UpdateSpec.spec)
        , ("Feature.UpsertSpec"              , Feature.UpsertSpec.spec actualPgVersion)
        ]

  hspec $ do
    mapM_ (parallel . before withApp) specs

    -- we analyze to get accurate results from EXPLAIN
    parallel $ beforeAll_ analyze . before withApp $
      describe "Feature.RangeSpec" Feature.RangeSpec.spec

    -- this test runs with a raw-output-media-types set to text/html
    parallel $ before htmlRawOutputApp $
      describe "Feature.HtmlRawOutputSpec" Feature.HtmlRawOutputSpec.spec

    -- this test runs with a different server flag
    parallel $ before maxRowsApp $
      describe "Feature.QueryLimitedSpec" Feature.QueryLimitedSpec.spec

    -- this test runs with a different schema
    parallel $ before unicodeApp $
      describe "Feature.UnicodeSpec" Feature.UnicodeSpec.spec

    -- this test runs with openapi-mode set to disabled
    parallel $ before disabledOpenApi $
      describe "Feature.DisabledOpenApiSpec" Feature.DisabledOpenApiSpec.spec

    -- this test runs with openapi-mode set to ignore-acl
    parallel $ before ignorePrivOpenApi $
      describe "Feature.IgnorePrivOpenApiSpec" Feature.IgnorePrivOpenApiSpec.spec

    -- this test runs with a proxy
    parallel $ before proxyApp $
      describe "Feature.ProxySpec" Feature.ProxySpec.spec

    -- this test runs without a JWT secret
    parallel $ before noJwtApp $
      describe "Feature.NoJwtSpec" Feature.NoJwtSpec.spec

    -- this test runs with a binary JWT secret
    parallel $ before binaryJwtApp $
      describe "Feature.BinaryJwtSecretSpec" Feature.BinaryJwtSecretSpec.spec

    -- this test runs with a binary JWT secret and an audience claim
    parallel $ before audJwtApp $
      describe "Feature.AudienceJwtSecretSpec" Feature.AudienceJwtSecretSpec.spec

    -- this test runs with asymmetric JWK
    parallel $ before asymJwkApp $
      describe "Feature.AsymmetricJwtSpec" Feature.AsymmetricJwtSpec.spec

    -- this test runs with asymmetric JWKSet
    parallel $ before asymJwkSetApp $
      describe "Feature.AsymmetricJwtSpec" Feature.AsymmetricJwtSpec.spec

    -- this test runs with a nonexistent db-schema
    parallel $ before nonexistentSchemaApp $
      describe "Feature.NonexistentSchemaSpec" Feature.NonexistentSchemaSpec.spec

    -- this test runs with an extra search path
    parallel $ before extraSearchPathApp $
      describe "Feature.ExtraSearchPathSpec" Feature.ExtraSearchPathSpec.spec

    -- this test runs with a root spec function override
    parallel $ before rootSpecApp $
      describe "Feature.RootSpec" Feature.RootSpec.spec
    parallel $ before responseHeadersApp $
      describe "Feature.RpcPreRequestGucsSpec" Feature.RpcPreRequestGucsSpec.spec

    -- this test runs with multiple schemas
    parallel $ before multipleSchemaApp $
      describe "Feature.MultipleSchemaSpec" Feature.MultipleSchemaSpec.spec

    -- this test runs with db-uses-legacy-gucs = false
    parallel $ before testCfgLegacyGucsApp $
      describe "Feature.LegacyGucsSpec" Feature.LegacyGucsSpec.spec

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
    loadDbStructure pool schemas extraSearchPath =
      either (panic.show) id <$> P.use pool (HT.transaction HT.ReadCommitted HT.Read $ queryDbStructure (toList schemas) extraSearchPath True)
