module Main where

import Test.Hspec
import SpecHelper

import qualified Hasql.Pool as P

import PostgREST.DbStructure (getDbStructure)
import PostgREST.App (postgrest)
import Data.Function (id)
import Data.IORef

import qualified Feature.AuthSpec
import qualified Feature.AsymmetricJwtSpec
import qualified Feature.BinaryJwtSecretSpec
import qualified Feature.ConcurrentSpec
import qualified Feature.CorsSpec
import qualified Feature.DeleteSpec
import qualified Feature.InsertSpec
import qualified Feature.NoJwtSpec
import qualified Feature.QueryLimitedSpec
import qualified Feature.QuerySpec
import qualified Feature.RangeSpec
import qualified Feature.StructureSpec
import qualified Feature.SingularSpec
import qualified Feature.UnicodeSpec
import qualified Feature.ProxySpec
import qualified Feature.AndOrParamsSpec

import Protolude

main :: IO ()
main = do
  testDbConn <- getEnvVarWithDefault "POSTGREST_TEST_CONNECTION" "postgres://postgrest_test@localhost/postgrest_test"
  setupDb testDbConn

  pool <- P.acquire (3, 10, toS testDbConn)

  result <- P.use pool $ getDbStructure "test"
  refDbStructure <- newIORef $ Just $ either (panic.show) id result
  let withApp      = return $ postgrest (testCfg testDbConn)          refDbStructure pool $ pure ()
      ltdApp       = return $ postgrest (testLtdRowsCfg testDbConn)   refDbStructure pool $ pure ()
      unicodeApp   = return $ postgrest (testUnicodeCfg testDbConn)   refDbStructure pool $ pure ()
      proxyApp     = return $ postgrest (testProxyCfg testDbConn)     refDbStructure pool $ pure ()
      noJwtApp     = return $ postgrest (testCfgNoJWT testDbConn)     refDbStructure pool $ pure ()
      binaryJwtApp = return $ postgrest (testCfgBinaryJWT testDbConn) refDbStructure pool $ pure ()
      asymJwkApp   = return $ postgrest (testCfgAsymJWK testDbConn)   refDbStructure pool $ pure ()

  let reset = resetDb testDbConn
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

    -- this test runs with asymmetric JWK
    beforeAll_ reset . before asymJwkApp $
      describe "Feature.AsymmetricJwtSpec" Feature.AsymmetricJwtSpec.spec

 where
  specs = map (uncurry describe) [
      ("Feature.AuthSpec"         , Feature.AuthSpec.spec)
    , ("Feature.ConcurrentSpec"   , Feature.ConcurrentSpec.spec)
    , ("Feature.CorsSpec"         , Feature.CorsSpec.spec)
    , ("Feature.DeleteSpec"       , Feature.DeleteSpec.spec)
    , ("Feature.InsertSpec"       , Feature.InsertSpec.spec)
    , ("Feature.QuerySpec"        , Feature.QuerySpec.spec)
    , ("Feature.RangeSpec"        , Feature.RangeSpec.spec)
    , ("Feature.SingularSpec"     , Feature.SingularSpec.spec)
    , ("Feature.StructureSpec"    , Feature.StructureSpec.spec)
    , ("Feature.AndOrParamsSpec"  , Feature.AndOrParamsSpec.spec)
    ]
