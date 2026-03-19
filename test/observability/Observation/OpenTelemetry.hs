module Observation.OpenTelemetry where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types   (methodGet)
import           Network.Wai          (Application)
import           Test.Hspec           (SpecWith, describe, it,
                                       shouldBe)
import           Test.Hspec.Wai

import Protolude

import ObsHelper (Collector, NormalizedSpan, authHeaderJWT,
                  generateJWT, waitForNormalizedSpans)

spec :: SpecWith (Maybe Collector, Application)
spec =
  describe "OpenTelemetry traces" $
    it "exports normalized request spans and matches golden output" $ do
      mCollector <- getState
      case mCollector of
        Nothing ->
          pendingWith "OTel collector binary not found. Expected otelcol in PATH."
        Just collector -> do
          expected <- liftIO loadGoldenSpans
          let claims :: BL.ByteString
              claims = "{\"exp\":9999999999,\"role\":\"postgrest_test_author\",\"id\":\"otel\"}"
              auth = authHeaderJWT (generateJWT claims)

          request methodGet "/authors_only" [auth] ""
            `shouldRespondWith` 200

          actual <- liftIO $ waitForNormalizedSpans collector
          liftIO $ actual `shouldBe` expected

loadGoldenSpans :: IO [NormalizedSpan]
loadGoldenSpans = do
  res <- JSON.eitherDecodeFileStrict' goldenTracePath
  case res of
    Left err -> panic $ "Failed to decode golden traces from " <> show goldenTracePath <> ": " <> toS err
    Right spans -> pure spans
  where
    goldenTracePath :: FilePath
    goldenTracePath = "test/observability/fixtures/otel-traces.golden.json"
