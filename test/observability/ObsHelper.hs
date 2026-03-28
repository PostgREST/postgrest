{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NumericUnderscores        #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
module ObsHelper where

import           Control.Retry          (RetryPolicy, constantDelay,
                                         limitRetries, retrying)
import qualified Data.Aeson             as JSON
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.Aeson.Types       as JSONT
import qualified Data.ByteString        as BSS
import qualified Data.ByteString.Base64 as B64 (decodeLenient)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Lazy   as BL
import qualified Data.List              as L
import qualified Data.Map.Strict        as M
import qualified Data.Set               as Set
import qualified Jose.Jwa               as JWT
import qualified Jose.Jws               as JWT
import qualified Jose.Jwt               as JWT
import           Network.Socket         (Family (AF_INET),
                                         SockAddr (SockAddrInet, SockAddrInet6),
                                         SocketOption (ReuseAddr),
                                         SocketType (Stream), bind,
                                         close,
                                         defaultProtocol,
                                         getSocketName,
                                         setSocketOption, socket,
                                         tupleToHostAddress)
import           System.Directory       (doesFileExist,
                                         getTemporaryDirectory,
                                         removeDirectoryRecursive)
import           System.Environment     (setEnv)
import           System.FilePath        ((</>))
import           System.IO              (BufferMode (LineBuffering),
                                         hClose, hSetBuffering)
import           System.IO.Temp         (createTempDirectory)
import           System.Process         (CreateProcess (..),
                                         ProcessHandle,
                                         StdStream (UseHandle),
                                         createProcess,
                                         proc,
                                         terminateProcess,
                                         waitForProcess)

import PostgREST.Config (AppConfig (..), JSPathExp (..),
                         LogLevel (..), OpenAPIMode (..),
                         Verbosity (..), parseSecret)

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
  , configServerOtelEnabled         = False
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

testCfgOTel :: AppConfig
testCfgOTel = baseCfg { configServerOtelEnabled = True }

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

data Collector = Collector
  { collectorHandle     :: ProcessHandle
  , collectorLogHandle  :: Handle
  , collectorTmpDir     :: FilePath
  , collectorLogPath    :: FilePath
  , collectorTracesPath :: FilePath
  , collectorPort       :: Int
  }

data RawSpan = RawSpan
  { rawSpanName     :: Text
  , rawSpanId       :: Maybe Text
  , rawParentSpanId :: Maybe Text
  } deriving (Eq, Show)

data NormalizedSpan = NormalizedSpan
  { spanName   :: Text
  , spanParent :: Maybe Text
  } deriving (Eq, Ord, Show)

data SpanExport
  = SpansReady [NormalizedSpan]
  | SpansPending (Maybe Text)

instance JSON.FromJSON NormalizedSpan where
  parseJSON = JSON.withObject "NormalizedSpan" $ \obj ->
    NormalizedSpan
      <$> obj JSON..: "name"
      <*> obj JSON..:? "parent"

instance JSON.ToJSON NormalizedSpan where
  toJSON NormalizedSpan{spanName, spanParent} =
    JSON.object
      [ "name" JSON..= spanName
      , "parent" JSON..= spanParent
      ]

collectorEndpoint :: Collector -> Text
collectorEndpoint Collector{collectorPort} = "http://127.0.0.1:" <> show collectorPort

startCollector :: FilePath -> IO Collector
startCollector collectorBin =
  bracketOnError createTempDir cleanupTmpDir $ \tmpDir -> do
    port <- reserveCollectorPort
    let configPath = tmpDir </> "otelcol-config.yaml"
        tracesPath = tmpDir </> "traces.json"
        logPath = tmpDir </> "otelcol.log"

    writeFile configPath $ toS $ collectorConfig port tracesPath

    bracketOnError (openCollectorLog logPath) cleanupLogHandle $ \logHandle -> do
      (_, _, _, collectorHandle) <- createProcess $ (proc collectorBin ["--config", configPath])
        { std_out = UseHandle logHandle
        , std_err = UseHandle logHandle
        }

      let collector =
            Collector
              { collectorHandle
              , collectorLogHandle = logHandle
              , collectorTmpDir = tmpDir
              , collectorLogPath = logPath
              , collectorTracesPath = tracesPath
              , collectorPort = port
              }
      pure collector
  where

    openCollectorLog logPath = do
      logHandle <- openFile logPath WriteMode
      hSetBuffering logHandle LineBuffering
      pure logHandle

cleanupLogHandle :: Handle -> IO ()
cleanupLogHandle logHandle =
  void (try (hClose logHandle) :: IO (Either SomeException ()))

cleanupTmpDir :: FilePath -> IO ()
cleanupTmpDir tmpDir =
  void (try (removeDirectoryRecursive tmpDir) :: IO (Either SomeException ()))

stopCollector :: Collector -> IO ()
stopCollector Collector{collectorHandle, collectorLogHandle, collectorTmpDir} = do
  void (try (terminateProcess collectorHandle) :: IO (Either SomeException ()))
  void (try (waitForProcess collectorHandle) :: IO (Either SomeException ExitCode))
  cleanupLogHandle collectorLogHandle
  cleanupTmpDir collectorTmpDir

waitForNormalizedSpans :: Collector -> IO [NormalizedSpan]
waitForNormalizedSpans collector = do
  result <- retrying pollPolicy shouldRetry $ \_ -> do
    let tracesPath = collectorTracesPath collector
    exists <- doesFileExist tracesPath
    if not exists
      then pure $ SpansPending Nothing
      else do
        payload <- BL.readFile tracesPath
        case decodeNormalizedSpans payload of
          Right spans | not (null spans) -> pure $ SpansReady spans
          Right _                        -> pure $ SpansPending Nothing
          Left err                       -> pure $ SpansPending (Just err)

  case result of
    SpansReady spans -> pure spans
    SpansPending lastErr ->
      failCollector collector $
        "Timed out waiting for exported traces at "
          <> toS (collectorTracesPath collector)
          <> maybe mempty ("\nLast decode error:\n" <>) lastErr
  where
    shouldRetry _ (SpansPending _) = pure True
    shouldRetry _ (SpansReady _)   = pure False

decodeNormalizedSpans :: BL.ByteString -> Either Text [NormalizedSpan]
decodeNormalizedSpans payload = do
  values <- decodeValues payload
  let rawSpans = concatMap extractSpans values
  pure $ normalizeSpans rawSpans
  where
    normalizeSpans :: [RawSpan] -> [NormalizedSpan]
    normalizeSpans rawSpans =
      L.sortOn (\NormalizedSpan{spanParent, spanName} -> (spanParent, spanName)) $
        Set.toList . Set.fromList $ map toNormalized filtered
      where
        filtered = filter (\span -> rawSpanName span `Set.member` interestingSpans) rawSpans
        spanIdsToNames = M.fromList [(sid, rawSpanName span) | span <- filtered, Just sid <- [rawSpanId span]]
        toNormalized span =
          NormalizedSpan
            { spanName = rawSpanName span
            , spanParent = rawParentSpanId span >>= (`M.lookup` spanIdsToNames)
            }

decodeValues :: BL.ByteString -> Either Text [JSON.Value]
decodeValues bytes
  | BL.null bytes = Right []
  | otherwise =
      case JSON.eitherDecode bytes of
        Right value -> Right [value]
        Left jsonErr ->
          first
            (\ndjsonErr ->
               "Failed to decode collector payload as JSON ("
                 <> toS jsonErr
                 <> ") or newline-delimited JSON ("
                 <> toS ndjsonErr
                 <> ")"
            )
            ( traverse JSON.eitherDecodeStrict'
                . filter (not . BSS.null)
                . BSC.lines
                $ BL.toStrict bytes
            )

extractSpans :: JSON.Value -> [RawSpan]
extractSpans = collectSpans
  where
    collectSpans value = case value of
      JSON.Array arr -> concatMap collectSpans (toList arr)
      JSON.Object obj ->
        let localSpans = fromMaybe [] $ do
              JSON.Array spans <- KM.lookup "spans" obj
              pure $ mapMaybe spanFromValue (toList spans)
            nestedSpans = concatMap collectSpans (KM.elems obj)
         in localSpans <> nestedSpans
      _ -> []

    spanFromValue :: JSON.Value -> Maybe RawSpan
    spanFromValue = JSONT.parseMaybe $ JSON.withObject "RawSpan" $ \obj -> do
      name <- obj JSON..: "name"
      spanId <- obj JSON..:? "spanId"
      parentSpanId <- obj JSON..:? "parentSpanId"
      pure
        RawSpan
          { rawSpanName = name
          , rawSpanId = spanId
          , rawParentSpanId = parentSpanId
          }

failCollector :: Collector -> Text -> IO a
failCollector collector message = do
  logs <- readCollectorLogs collector
  panic . toS $ message <> renderCollectorLogs logs

readCollectorLogs :: Collector -> IO Text
readCollectorLogs Collector{collectorLogPath} = do
  exists <- doesFileExist collectorLogPath
  if exists
    then readFile collectorLogPath
    else pure ""

renderCollectorLogs :: Text -> Text
renderCollectorLogs logs
  | logs == "" = ""
  | otherwise = "\nCollector logs:\n" <> logs

reserveCollectorPort :: IO Int
reserveCollectorPort =
  bracket
    (socket AF_INET Stream defaultProtocol)
    close
    (\sock -> do
       setSocketOption sock ReuseAddr 1
       bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
       sockAddr <- getSocketName sock
       case sockAddr of
         SockAddrInet port _      -> pure (fromIntegral port)
         SockAddrInet6 port _ _ _ -> pure (fromIntegral port)
         _                        -> panic "Failed to determine reserved OTel collector port"
    )

createTempDir :: IO FilePath
createTempDir = do
  tmpRoot <- getTemporaryDirectory
  createTempDirectory tmpRoot "postgrest-otel-"

collectorConfig :: Int -> FilePath -> Text
collectorConfig port tracesPath =
  unlines
    [ "receivers:"
    , "  otlp:"
    , "    protocols:"
    , "      http:"
    , "        endpoint: " <> collectorReceiverAddress port
    , "exporters:"
    , "  file:"
    , "    path: \"" <> toS tracesPath <> "\""
    , "service:"
    , "  pipelines:"
    , "    traces:"
    , "      receivers: [otlp]"
    , "      exporters: [file]"
    ]

collectorReceiverAddress :: Int -> Text
collectorReceiverAddress port = "127.0.0.1:" <> show port

interestingSpans :: Set.Set Text
interestingSpans = Set.fromList ["request", "parse", "plan", "query", "response"]

pollPolicy :: RetryPolicy
pollPolicy = constantDelay 200_000 <> limitRetries 49

configureOTelEnv :: Text -> IO ()
configureOTelEnv otlpEndpoint = do
  -- Configure the SDK once so spans from the OTel test go to the local collector.
  mapM_
    (uncurry setEnv)
    [ ("OTEL_TRACES_EXPORTER", "otlp"),
      ("OTEL_EXPORTER_OTLP_PROTOCOL", "http/protobuf"),
      ("OTEL_EXPORTER_OTLP_ENDPOINT", toS otlpEndpoint),
      ("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", toS $ otlpEndpoint <> "/v1/traces"),
      ("OTEL_TRACES_SAMPLER", "always_on"),
      ("OTEL_BSP_SCHEDULE_DELAY", "100"),
      ("OTEL_BSP_EXPORT_TIMEOUT", "30000"),
      ("OTEL_SERVICE_NAME", "postgrest-observability-tests")
    ]
