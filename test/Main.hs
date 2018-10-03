module Main where

import Test.Hspec
import SpecHelper

import qualified Hasql.Pool as P

import PostgREST.App (postgrest)
import PostgREST.DbStructure (getDbStructure, getPgVersion)
import PostgREST.Types (DbStructure(..), pgVersion95, pgVersion96)
import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate, updateAction)
import Data.Function (id)
import Data.IORef
import Data.Time.Clock (getCurrentTime)

import qualified Feature.AuthSpec
import qualified Feature.AsymmetricJwtSpec
import qualified Feature.BinaryJwtSecretSpec
import qualified Feature.AudienceJwtSecretSpec
import qualified Feature.ConcurrentSpec
import qualified Feature.CorsSpec
import qualified Feature.DeleteSpec
import qualified Feature.InsertSpec
import qualified Feature.JsonOperatorSpec
import qualified Feature.NoJwtSpec
import qualified Feature.QueryLimitedSpec
import qualified Feature.QuerySpec
import qualified Feature.RangeSpec
import qualified Feature.StructureSpec
import qualified Feature.SingularSpec
import qualified Feature.UnicodeSpec
import qualified Feature.ProxySpec
import qualified Feature.AndOrParamsSpec
import qualified Feature.RpcSpec
import qualified Feature.NonexistentSchemaSpec
import qualified Feature.PgVersion95Spec
import qualified Feature.PgVersion96Spec
import qualified Feature.UpsertSpec

import Protolude

main :: IO ()
main = do
  testDbConn <- getEnvVarWithDefault "POSTGREST_TEST_CONNECTION" "postgres://postgrest_test@localhost/postgrest_test"
  setupDb testDbConn

  pool <- P.acquire (3, 10, toS testDbConn)

  result <- P.use pool $ getDbStructure "test" =<< getPgVersion

  dbStructure <- pure $ either (panic.show) id result

  getTime <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }

  refDbStructure <- newIORef $ Just dbStructure

  let withApp              = return $ postgrest (testCfg testDbConn)            refDbStructure pool getTime $ pure ()
      ltdApp               = return $ postgrest (testLtdRowsCfg testDbConn)     refDbStructure pool getTime $ pure ()
      unicodeApp           = return $ postgrest (testUnicodeCfg testDbConn)     refDbStructure pool getTime $ pure ()
      proxyApp             = return $ postgrest (testProxyCfg testDbConn)       refDbStructure pool getTime $ pure ()
      noJwtApp             = return $ postgrest (testCfgNoJWT testDbConn)       refDbStructure pool getTime $ pure ()
      binaryJwtApp         = return $ postgrest (testCfgBinaryJWT testDbConn)   refDbStructure pool getTime $ pure ()
      audJwtApp            = return $ postgrest (testCfgAudienceJWT testDbConn) refDbStructure pool getTime $ pure ()
      asymJwkApp           = return $ postgrest (testCfgAsymJWK testDbConn)     refDbStructure pool getTime $ pure ()
      nonexistentSchemaApp = return $ postgrest (testNonexistentSchemaCfg testDbConn)   refDbStructure pool getTime $ pure ()

  let reset :: IO ()
      reset = resetDb testDbConn

      actualPgVersion = pgVersion dbStructure
      extraSpecs =
        [("Feature.UpsertSpec", Feature.UpsertSpec.spec) | actualPgVersion >= pgVersion95] ++
        [("Feature.PgVersion95Spec", Feature.PgVersion95Spec.spec) | actualPgVersion >= pgVersion95] ++
        [("Feature.PgVersion96Spec", Feature.PgVersion96Spec.spec) | actualPgVersion >= pgVersion96]

      specs = uncurry describe <$> [
          ("Feature.AuthSpec"               , Feature.AuthSpec.spec)
        , ("Feature.ConcurrentSpec"         , Feature.ConcurrentSpec.spec)
        , ("Feature.CorsSpec"               , Feature.CorsSpec.spec)
        , ("Feature.DeleteSpec"             , Feature.DeleteSpec.spec)
        , ("Feature.InsertSpec"             , Feature.InsertSpec.spec)
        , ("Feature.JsonOperatorSpec"       , Feature.JsonOperatorSpec.spec)
        , ("Feature.QuerySpec"              , Feature.QuerySpec.spec)
        , ("Feature.RpcSpec"                , Feature.RpcSpec.spec)
        , ("Feature.RangeSpec"              , Feature.RangeSpec.spec)
        , ("Feature.SingularSpec"           , Feature.SingularSpec.spec)
        , ("Feature.StructureSpec"          , Feature.StructureSpec.spec)
        , ("Feature.AndOrParamsSpec"        , Feature.AndOrParamsSpec.spec)
        , ("Feature.NonexistentSchemaSpec"  , Feature.NonexistentSchemaSpec.spec)
        ] ++ extraSpecs

  hspec $ do
    mapM_ (beforeAll_ reset . before withApp) specs

    -- this test runs with a different server flag
    beforeAll_ reset . before ltdApp $
      describe "Feature.QueryLimitedSpec" Feature.QueryLimitedSpec.spec

    -- this test runs with a different schema
    beforeAll_ reset . before unicodeApp $
      describe "Feature.UnicodeSpec" Feature.UnicodeSpec.spec

    -- this test runs with a proxy
    beforeAll_ reset . before proxyApp $
      describe "Feature.ProxySpec" Feature.ProxySpec.spec

    -- this test runs without a JWT secret
    beforeAll_ reset . before noJwtApp $
      describe "Feature.NoJwtSpec" Feature.NoJwtSpec.spec

    -- this test runs with a binary JWT secret
    beforeAll_ reset . before binaryJwtApp $
      describe "Feature.BinaryJwtSecretSpec" Feature.BinaryJwtSecretSpec.spec

    -- this test runs with a binary JWT secret and an audience claim
    beforeAll_ reset . before audJwtApp $
      describe "Feature.AudienceJwtSecretSpec" Feature.AudienceJwtSecretSpec.spec

    -- this test runs with asymmetric JWK
    beforeAll_ reset . before asymJwkApp $
      describe "Feature.AsymmetricJwtSpec" Feature.AsymmetricJwtSpec.spec

    -- this test runs with a nonexistent db-schema
    beforeAll_ reset . before nonexistentSchemaApp $
      describe "Feature.NonexistentSchemaSpec" Feature.NonexistentSchemaSpec.spec
