{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
module ObsHelper where

import qualified Data.ByteString.Base64 as B64 (decodeLenient)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Jose.Jwa               as JWT
import qualified Jose.Jws               as JWT
import qualified Jose.Jwt               as JWT

import PostgREST.Config (AppConfig (..), JSPathExp (..),
                         LogLevel (..), OpenAPIMode (..), parseSecret)

import Data.List.NonEmpty              (fromList)
import Data.String                     (String)
import Prometheus                      (Counter, getCounter)
import Test.Hspec.Expectations.Contrib (annotate)

import Network.HTTP.Types
import Protolude
import Test.Hspec
import Test.Hspec.Wai


baseCfg :: AppConfig
baseCfg = let secret = encodeUtf8 "reallyreallyreallyreallyverysafe" in
  AppConfig {
    configAppSettings               = []
  , configDbAggregates              = False
  , configDbAnonRole                = Just "postgrest_test_anonymous"
  , configDbChannel                 = mempty
  , configDbChannelEnabled          = True
  , configDbExtraSearchPath         = []
  , configDbHoistedTxSettings       = ["default_transaction_isolation","plan_filter.statement_cost_limit","statement_timeout"]
  , configDbMaxRows                 = Nothing
  , configDbPlanEnabled             = False
  , configDbPoolSize                = 10
  , configDbPoolAcquisitionTimeout  = 10
  , configDbPoolMaxLifetime         = 1800
  , configDbPoolMaxIdletime         = 600
  , configDbPoolAutomaticRecovery   = True
  , configDbPreRequest              = Nothing
  , configDbPreparedStatements      = True
  , configDbRootSpec                = Nothing
  , configDbSchemas                 = fromList ["test"]
  , configDbConfig                  = False
  , configDbPreConfig               = Nothing
  , configDbUri                     = "postgresql://"
  , configFilePath                  = Nothing
  , configJWKS                      = rightToMaybe $ parseSecret secret
  , configJwtAudience               = Nothing
  , configJwtRoleClaimKey           = [JSPKey "role"]
  , configJwtSecret                 = Just secret
  , configJwtSecretIsBase64         = False
  , configJwtCacheMaxEntries        = 10
  , configLogLevel                  = LogCrit
  , configLogQuery                  = False
  , configOpenApiMode               = OAFollowPriv
  , configOpenApiSecurityActive     = False
  , configOpenApiServerProxyUri     = Nothing
  , configServerCorsAllowedOrigins  = Nothing
  , configServerHost                = "localhost"
  , configServerPort                = 3000
  , configServerTraceHeader         = Nothing
  , configServerUnixSocket          = Nothing
  , configServerUnixSocketMode      = 432
  , configDbTxAllowOverride         = True
  , configDbTxRollbackAll           = True
  , configAdminServerHost           = "localhost"
  , configAdminServerPort           = Nothing
  , configRoleSettings              = mempty
  , configRoleIsoLvl                = mempty
  , configInternalSCQuerySleep      = Nothing
  , configInternalSCLoadSleep       = Nothing
  , configInternalSCRelLoadSleep    = Nothing
  , configServerTimingEnabled       = True
  }

testCfg :: AppConfig
testCfg = baseCfg

testCfgJwtCache :: AppConfig
testCfgJwtCache =
  baseCfg {
    configJwtSecret = Just generateSecret
  , configJWKS = rightToMaybe $ parseSecret generateSecret
  , configJwtCacheMaxEntries = 2
  }

authHeader :: BS.ByteString -> BS.ByteString -> Header
authHeader typ creds =
  (hAuthorization, typ <> " " <> creds)

authHeaderJWT :: BS.ByteString -> Header
authHeaderJWT = authHeader "Bearer"

generateSecret :: ByteString
generateSecret = B64.decodeLenient "cmVhbGx5cmVhbGx5cmVhbGx5cmVhbGx5dmVyeXNhZmU="

generateJWT :: BL.ByteString -> ByteString
generateJWT claims =
  either mempty JWT.unJwt $ JWT.hmacEncode JWT.HS256 generateSecret (BL.toStrict claims)

-- state check helpers

data StateCheck st m = forall a. StateCheck (st -> (String, m a)) (a -> a -> Expectation)

stateCheck :: (Show a, Eq a) => (c -> m a) -> (st -> (String, c)) -> (a -> a) -> StateCheck st m
stateCheck extractValue extractComponent expect = StateCheck (second extractValue . extractComponent) (flip shouldBe . expect)

expectField :: forall s st a c m. (KnownSymbol s, Show a, Eq a, HasField s st c) => (c -> m a) -> (a -> a) -> StateCheck st m
expectField extractValue = stateCheck extractValue ((symbolVal (Proxy @s),) . getField @s)

checkState :: (Traversable t) => t (StateCheck st (WaiSession st)) -> WaiSession st b -> WaiSession st ()
checkState checks act = getState >>= flip (`checkState'` checks) act

checkState' :: (Traversable t, MonadIO m) => st -> t (StateCheck st m) -> m b -> m ()
checkState' initialState checks act = do
  expectations <- traverse (\(StateCheck g expect) -> let (msg, m) = g initialState in m >>= createExpectation msg m . expect) checks
  void act
  sequenceA_ expectations
  where
    createExpectation msg metrics expect = pure $ metrics >>= liftIO . annotate msg . expect

expectCounter :: forall s st m. (KnownSymbol s, HasField s st Counter, MonadIO m) => (Int -> Int) -> StateCheck st m
expectCounter = expectField @s intCounter
  where
    intCounter = ((round @Double @Int) <$>) . getCounter
