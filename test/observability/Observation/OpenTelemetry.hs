module Observation.OpenTelemetry where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types   (methodGet)
import           Network.Wai          (Application)
import           Test.Hspec           (SpecWith, describe, it,
                                       shouldBe)
import           Test.Hspec.Wai

import Protolude

import ObsHelper  (authHeaderJWT, generateJWT)
import OTelHelper (Collector, NormalizedSpan, waitForNormalizedSpans)

spec :: SpecWith (Maybe Collector, Application)
spec =
  describe "OpenTelemetry traces" $
    it "exports normalized request spans and matches snapshot output" $ do
      mCollector <- getState
      case mCollector of
        Nothing ->
          pendingWith "OTel collector binary not found. Expected otelcol in PATH."
        Just collector -> do
          expected <- liftIO loadSnapshotSpans
          let claims :: BL.ByteString
              claims = "{\"exp\":9999999999,\"role\":\"postgrest_test_author\",\"id\":\"otel\"}"
              auth = authHeaderJWT (generateJWT claims)

          request methodGet "/authors_only" [auth] ""
            `shouldRespondWith` 200

          actual <- liftIO $ waitForNormalizedSpans collector
          liftIO $ actual `shouldBe` expected

loadSnapshotSpans :: IO [NormalizedSpan]
loadSnapshotSpans = do
  res <- JSON.eitherDecodeFileStrict' snapshotTracePath
  case res of
    Left err -> panic $ "Failed to decode snapshot traces from " <> show snapshotTracePath <> ": " <> toS err
    Right spans -> pure spans
  where
    snapshotTracePath :: FilePath
    snapshotTracePath = "test/observability/fixtures/otel-traces.snapshot.json"
