module Main where

import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction.Sessions as HT

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate,
                           updateAction)
import Data.Function      (id)
import Data.Time.Clock    (getCurrentTime)

import Data.IORef
import Test.Hspec

import PostgREST.App         (postgrest)
import PostgREST.DbStructure (getDbStructure, getPgVersion)
import PostgREST.Types       (DbStructure (..), pgVersion95,
                              pgVersion96)
import Protolude
import SpecHelper

import qualified Feature.AndOrParamsSpec
import qualified Feature.AsymmetricJwtSpec
import qualified Feature.AudienceJwtSecretSpec
import qualified Feature.AuthSpec
import qualified Feature.BinaryJwtSecretSpec
import qualified Feature.ConcurrentSpec
import qualified Feature.CorsSpec
import qualified Feature.DeleteSpec
import qualified Feature.EmbedDisambiguationSpec
import qualified Feature.ExtraSearchPathSpec
import qualified Feature.HtmlRawOutputSpec
import qualified Feature.InsertSpec
import qualified Feature.JsonOperatorSpec
import qualified Feature.NoJwtSpec
import qualified Feature.NonexistentSchemaSpec
import qualified Feature.PgVersion95Spec
import qualified Feature.PgVersion96Spec
import qualified Feature.ProxySpec
import qualified Feature.QueryLimitedSpec
import qualified Feature.QuerySpec
import qualified Feature.RangeSpec
import qualified Feature.RawOutputTypesSpec
import qualified Feature.RootSpec
import qualified Feature.RpcSpec
import qualified Feature.SingularSpec
import qualified Feature.StructureSpec
import qualified Feature.UnicodeSpec
import qualified Feature.UpsertSpec


main :: IO ()
main = do
  testDbConn <- getEnvVarWithDefault "POSTGREST_TEST_CONNECTION" "postgres://postgrest_test@localhost/postgrest_test"
  setupDb testDbConn

  pool <- P.acquire (3, 10, toS testDbConn)

  result <- P.use pool $ do
    ver <- getPgVersion
    HT.transaction HT.ReadCommitted HT.Read $ getDbStructure "test" ver

  let dbStructure = either (panic.show) id result

  getTime <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }

  refDbStructure <- newIORef $ Just dbStructure

  let app cfg = return ((), postgrest (cfg testDbConn) refDbStructure (Right pool) getTime $ pure ())

  let withApp              = app testCfg
      maxRowsApp           = app testMaxRowsCfg
      unicodeApp           = app testUnicodeCfg
      proxyApp             = app testProxyCfg
      noJwtApp             = app testCfgNoJWT
      binaryJwtApp         = app testCfgBinaryJWT
      audJwtApp            = app testCfgAudienceJWT
      asymJwkApp           = app testCfgAsymJWK
      asymJwkSetApp        = app testCfgAsymJWKSet
      nonexistentSchemaApp = app testNonexistentSchemaCfg
      extraSearchPathApp   = app testCfgExtraSearchPath
      rootSpecApp          = app testCfgRootSpec
      htmlRawOutputApp     = app testCfgHtmlRawOutput
      responseHeadersApp   = app testCfgResponseHeaders

  let reset, analyze :: IO ()
      reset = resetDb testDbConn
      analyze = do
        analyzeTable testDbConn "items"
        analyzeTable testDbConn "child_entities"

      actualPgVersion = pgVersion dbStructure
      extraSpecs =
        [("Feature.UpsertSpec", Feature.UpsertSpec.spec) | actualPgVersion >= pgVersion95] ++
        [("Feature.PgVersion95Spec", Feature.PgVersion95Spec.spec) | actualPgVersion >= pgVersion95]

      specs = uncurry describe <$> [
          ("Feature.AuthSpec"                , Feature.AuthSpec.spec actualPgVersion)
        , ("Feature.RawOutputTypesSpec"      , Feature.RawOutputTypesSpec.spec)
        , ("Feature.ConcurrentSpec"          , Feature.ConcurrentSpec.spec)
        , ("Feature.CorsSpec"                , Feature.CorsSpec.spec)
        , ("Feature.JsonOperatorSpec"        , Feature.JsonOperatorSpec.spec actualPgVersion)
        , ("Feature.QuerySpec"               , Feature.QuerySpec.spec actualPgVersion)
        , ("Feature.EmbedDisambiguationSpec" , Feature.EmbedDisambiguationSpec.spec)
        , ("Feature.RpcSpec"                 , Feature.RpcSpec.spec actualPgVersion)
        , ("Feature.StructureSpec"           , Feature.StructureSpec.spec)
        , ("Feature.AndOrParamsSpec"         , Feature.AndOrParamsSpec.spec actualPgVersion)
        ] ++ extraSpecs

      mutSpecs = uncurry describe <$> [
          ("Feature.DeleteSpec"             , Feature.DeleteSpec.spec)
        , ("Feature.InsertSpec"             , Feature.InsertSpec.spec actualPgVersion)
        , ("Feature.SingularSpec"           , Feature.SingularSpec.spec)
        ]

  hspec $ do
    -- Only certain Specs need a database reset, this should be used with care as it slows down the whole test suite.
    mapM_ (afterAll_ reset . before withApp) mutSpecs

    mapM_ (before withApp) specs

    -- we analyze to get accurate results from EXPLAIN
    beforeAll_ analyze . before withApp $
      describe "Feature.RangeSpec" Feature.RangeSpec.spec

    -- this test runs with a raw-output-media-types set to text/html
    before htmlRawOutputApp $
      describe "Feature.HtmlRawOutputSpec" Feature.HtmlRawOutputSpec.spec

    -- this test runs with a different server flag
    before maxRowsApp $
      describe "Feature.QueryLimitedSpec" Feature.QueryLimitedSpec.spec

    -- this test runs with a different schema
    before unicodeApp $
      describe "Feature.UnicodeSpec" Feature.UnicodeSpec.spec

    -- this test runs with a proxy
    before proxyApp $
      describe "Feature.ProxySpec" Feature.ProxySpec.spec

    -- this test runs without a JWT secret
    before noJwtApp $
      describe "Feature.NoJwtSpec" Feature.NoJwtSpec.spec

    -- this test runs with a binary JWT secret
    before binaryJwtApp $
      describe "Feature.BinaryJwtSecretSpec" Feature.BinaryJwtSecretSpec.spec

    -- this test runs with a binary JWT secret and an audience claim
    before audJwtApp $
      describe "Feature.AudienceJwtSecretSpec" Feature.AudienceJwtSecretSpec.spec

    -- this test runs with asymmetric JWK
    before asymJwkApp $
      describe "Feature.AsymmetricJwtSpec" Feature.AsymmetricJwtSpec.spec

    -- this test runs with asymmetric JWKSet
    before asymJwkSetApp $
      describe "Feature.AsymmetricJwtSpec" Feature.AsymmetricJwtSpec.spec

    -- this test runs with a nonexistent db-schema
    before nonexistentSchemaApp $
      describe "Feature.NonexistentSchemaSpec" Feature.NonexistentSchemaSpec.spec

    -- this test runs with an extra search path
    before extraSearchPathApp $
      describe "Feature.ExtraSearchPathSpec" Feature.ExtraSearchPathSpec.spec

    -- this test runs with a root spec function override
    when (actualPgVersion >= pgVersion96) $ do
      before rootSpecApp $
        describe "Feature.RootSpec" Feature.RootSpec.spec
      before responseHeadersApp $
        describe "Feature.PgVersion96Spec" Feature.PgVersion96Spec.spec
