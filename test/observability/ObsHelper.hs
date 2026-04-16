{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
module ObsHelper where

import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base64          as B64
import qualified Data.ByteString.Lazy            as BL
import qualified Data.List                       as DL
import           Data.List.NonEmpty              (fromList)
import           Data.String                     (String)
import qualified Data.Text                       as T
import qualified Jose.Jwa                        as JWT
import qualified Jose.Jws                        as JWT
import qualified Jose.Jwt                        as JWT
import           Network.HTTP.Types
import qualified PostgREST.AppState              as AppState
import           PostgREST.Config                (AppConfig (..),
                                                  JSPathExp (..),
                                                  LogLevel (..),
                                                  OpenAPIMode (..),
                                                  Verbosity (..),
                                                  parseSecret)
import qualified PostgREST.Metrics               as Metrics
import           PostgREST.Observation           (Observation (..))
import           Prometheus                      (Counter, getCounter)
import           Protolude                       hiding (get, toS)
import           System.Timeout                  (timeout)
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib (annotate)

-- helpers used to produce observation diagnostics in waitForObs
-- Implementing the Show instance for Observation is hard due to having many different parameters so instead we use generic programming (`conName`) to obtain the constructor name as `Text`
class HasConstructor f where
  genericConstrName :: f x -> Text

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstrName (M1 x) = genericConstrName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstrName (L1 l) = genericConstrName l
  genericConstrName (R1 r) = genericConstrName r

instance Constructor c => HasConstructor (C1 c f) where
  genericConstrName = T.pack . conName

data SpecState = SpecState {
  specAppState :: AppState.AppState,
  specMetrics  :: Metrics.MetricsState,
  specObsChan  :: ObsChan
}

data StateCheck st m = forall a. StateCheck (st -> (String, m a)) (a -> a -> Expectation)

data TimeoutException = TimeoutException deriving (Show, Exception)

data ObsChan = ObsChan (Chan Observation) (Chan Observation)

constrName :: (HasConstructor (Rep a), Generic a)=> a -> Text
constrName = genericConstrName . from

baseCfg :: AppConfig
baseCfg = let secret = encodeUtf8 "reallyreallyreallyreallyverysafe" in
  AppConfig {
    configAppSettings               = []
  , configClientErrorVerbosity      = Verbose
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
  , configDbTimezoneEnabled         = True
  , configDbUri                     = "postgresql://"
  , configFilePath                  = Nothing
  , configJWKS                      = rightToMaybe $ parseSecret secret
  , configJwtAudience               = Nothing
  , configJwtRoleClaimKey           = [JSPKey "role"]
  , configJwtSchemaClaimKey         = [JSPKey "schema"]
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
stateCheck :: (Show a, Eq a) => (c -> m a) -> (st -> (String, c)) -> (a -> a) -> StateCheck st m
stateCheck extractValue extractComponent expect = StateCheck (second extractValue . extractComponent) (flip shouldBe . expect)

expectField :: forall s st a c m. (KnownSymbol s, Show a, Eq a, HasField s st c) => (c -> m a) -> (a -> a) -> StateCheck st m
expectField extractValue = stateCheck extractValue ((symbolVal (Proxy @s),) . getField @s)

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

accumulateUntilTimeout :: Int -> (s -> a -> s) -> s -> IO a -> IO s
accumulateUntilTimeout t f start act = do
  tid <- myThreadId
  -- mask to make sure TimeoutException is not thrown before starting the loop
  mask $ \unmask -> do
    -- start timeout thread unmasking exceptions
    ttid <- forkIOWithUnmask ($ (threadDelay t *> throwTo tid TimeoutException))
    -- unmask effect
    unmask (fix (\loop accum -> (act >>= loop . f accum) `onTimeout` pure accum) start)
      -- make sure we catch timeout if happens before entering the loop
      `onTimeout` pure start
      -- make sure timer thread is killed on other exceptions
      -- so that it won't throw TimeoutException later
      `onException` killThread ttid
  where
    onTimeout m a = m `catch` \TimeoutException -> a

newObsChan :: Chan Observation -> IO ObsChan
newObsChan = fmap <$> ObsChan <*> dupChan

-- read messages from copy chan and once condition is met drain original to the same point
-- upon timeout report error and messages remaining in the original chan
-- that way we report messages since last successful read
waitForObs :: HasCallStack => ObsChan -> Int -> Text -> (Observation -> Maybe a) -> IO ()
waitForObs (ObsChan orig copy) t msg f =
  timeout t (readUntil copy *> readUntil orig) >>= maybe failTimeout mempty
  where
    failTimeout = takeUntilTimeout decisecond (readChan orig)
        >>= expectationFailure . DL.unlines . fmap show . (failureMessageHeader :) . fmap obsDiagMessage
    failureMessageHeader = "Timeout waiting for " <> msg <> " at " <> loc <> ". Remaining observations:"
    readUntil = void . untilM (pure . not . null . f) . readChan
    loc = fromMaybe "(unknown)" . head $ (T.pack . prettySrcLoc . snd <$> getCallStack callStack)
    -- execute effectful computation until result meets provided condition
    untilM cond m = fix $ \loop -> m >>= \a -> ifM (cond a) (pure a) loop
    -- duplicate the provided channel and construct wairFor function binding both channels
    -- accumulate effecful computation results into a list for specified time
    takeUntilTimeout t' = fmap reverse . accumulateUntilTimeout t' (flip (:)) []
    decisecond = 100000

obsDiagMessage :: Observation -> Text
obsDiagMessage = \case
  (HasqlPoolObs o) -> show o
  o@(DBListenStart host port name channel) -> constrName o <> show (host, port, name, channel)
  o -> constrName o
