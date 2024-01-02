{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

module OTelHelper where

import           Control.Retry          (constantDelay, limitRetries,
                                         retrying)
import qualified Data.Aeson             as JSON
import qualified Data.Aeson.KeyMap      as KM
import qualified Data.Aeson.Types       as JSONT
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Lazy   as BL
import qualified Data.List              as L
import qualified Data.Map.Strict        as M
import           Data.Streaming.Network (getUnassignedPort)
import           System.Directory       (doesFileExist)
import           System.Environment     (setEnv)
import           System.FilePath        ((</>))
import           System.IO              (BufferMode (LineBuffering),
                                         hClose, hSetBuffering)
import           System.Process         (CreateProcess (..),
                                         ProcessHandle,
                                         StdStream (UseHandle),
                                         createProcess, proc,
                                         terminateProcess,
                                         waitForProcess)

import Protolude

data Collector = Collector
  { collectorHandle     :: ProcessHandle
  , collectorLogHandle  :: Handle
  , collectorLogPath    :: FilePath
  , collectorTracesPath :: FilePath
  , collectorPort       :: Int
  }

data NormalizedSpan = NormalizedSpan
  { spanName   :: Text
  , spanParent :: Maybe Text
  } deriving (Eq, Ord, Show)

instance JSON.FromJSON NormalizedSpan where
  parseJSON = JSON.withObject "NormalizedSpan" $ \obj ->
    NormalizedSpan <$> obj JSON..: "name" <*> obj JSON..:? "parent"

instance JSON.ToJSON NormalizedSpan where
  toJSON NormalizedSpan{..} =
    JSON.object ["name" JSON..= spanName, "parent" JSON..= spanParent]

type RawSpan = (Text, Maybe Text, Maybe Text)

collectorEndpoint :: Collector -> Text
collectorEndpoint Collector{..} = "http://127.0.0.1:" <> show collectorPort

startCollector :: FilePath -> FilePath -> IO Collector
startCollector tmpDir collectorBin = do
  collectorPort <- getUnassignedPort
  let configPath = tmpDir </> "otelcol-config.yaml"
      collectorTracesPath = tmpDir </> "traces.json"
      collectorLogPath = tmpDir </> "otelcol.log"
      openLog = do
        h <- openFile collectorLogPath WriteMode
        hSetBuffering h LineBuffering
        pure h

  writeFile configPath . toS $ collectorConfig collectorPort collectorTracesPath
  bracketOnError openLog cleanupLogHandle $ \collectorLogHandle -> do
    (_, _, _, collectorHandle) <- createProcess (proc collectorBin ["--config", configPath])
      { std_out = UseHandle collectorLogHandle
      , std_err = UseHandle collectorLogHandle
      }
    pure Collector{..}

cleanupLogHandle :: Handle -> IO ()
cleanupLogHandle h = void (try (hClose h) :: IO (Either SomeException ()))

stopCollector :: Collector -> IO ()
stopCollector Collector{..} = do
  void (try (terminateProcess collectorHandle) :: IO (Either SomeException ()))
  void (try (waitForProcess collectorHandle) :: IO (Either SomeException ExitCode))
  cleanupLogHandle collectorLogHandle

waitForNormalizedSpans :: Collector -> IO [NormalizedSpan]
waitForNormalizedSpans collector@Collector{..} =
  retrying (constantDelay 200_000 <> limitRetries 49) shouldRetry (const pollSpans)
    >>= either timeout pure
  where
    pollSpans = do
      exists <- doesFileExist collectorTracesPath
      if not exists
        then pure pending
        else do
          result <- decodeNormalizedSpans <$> BL.readFile collectorTracesPath
          pure $ case result of
            Right spans -> bool pending (Right spans) (not $ null spans)
            Left err    -> Left $ Just err

    pending = Left Nothing
    shouldRetry _ result = pure $ isLeft result
    timeout lastErr =
      failCollector collector $
        "Timed out waiting for exported traces at "
          <> toS collectorTracesPath
          <> maybe mempty ("\nLast decode error:\n" <>) lastErr

decodeNormalizedSpans :: BL.ByteString -> Either Text [NormalizedSpan]
decodeNormalizedSpans = fmap (normalizeSpans . concatMap extractSpans) . decodeValues

normalizeSpans :: [RawSpan] -> [NormalizedSpan]
normalizeSpans spans =
  L.sortOn (\NormalizedSpan{..} -> (spanParent, spanName))
    [ NormalizedSpan spanName (parentSpanId >>= (`M.lookup` spanIdsToNames))
    | (spanName, _, parentSpanId) <- interesting
    ]
  where
    interesting = filter (\(spanName, _, _) -> spanName `elem` ["request", "parse", "plan", "query", "response"]) spans
    spanIdsToNames = M.fromList [(spanId, spanName) | (spanName, Just spanId, _) <- interesting]

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
                . filter (not . BS.null)
                . BSC.lines
                $ BL.toStrict bytes
            )

extractSpans :: JSON.Value -> [RawSpan]
extractSpans = \case
  JSON.Array arr -> concatMap extractSpans (toList arr)
  JSON.Object obj ->
    let directSpans = case KM.lookup "spans" obj of
          Just (JSON.Array spans) -> mapMaybe parseSpan (toList spans)
          _                       -> []
     in directSpans <> concatMap extractSpans (KM.elems obj)
  _ -> []

parseSpan :: JSON.Value -> Maybe RawSpan
parseSpan = JSONT.parseMaybe $ JSON.withObject "Span" $ \obj ->
  (,,) <$> obj JSON..: "name" <*> obj JSON..:? "spanId" <*> obj JSON..:? "parentSpanId"

failCollector :: Collector -> Text -> IO a
failCollector Collector{..} message = do
  logs <- bool (pure "") (readFile collectorLogPath) =<< doesFileExist collectorLogPath
  panic . toS $ message <> bool "" ("\nCollector logs:\n" <> logs) (logs /= "")

collectorConfig :: Int -> FilePath -> Text
collectorConfig port tracesPath =
  unlines
    [ "receivers:"
    , "  otlp:"
    , "    protocols:"
    , "      http:"
    , "        endpoint: 127.0.0.1:" <> show port
    , "exporters:"
    , "  file:"
    , "    path: \"" <> toS tracesPath <> "\""
    , "service:"
    , "  pipelines:"
    , "    traces:"
    , "      receivers: [otlp]"
    , "      exporters: [file]"
    ]

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
