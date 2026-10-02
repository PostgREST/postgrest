{-# LANGUAGE LambdaCase #-}

module PostgREST.Response.Performance
  ( Metric (..)
  , Timer (..)
  , NoTimer (..)
  , ServerTimer
  , newServerTimer
  , timed
  , serverTimingHeader
  )
where

import Control.Monad.Except (liftEither)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (showFFloat)
import Protolude

import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Types qualified as HTTP

-- $setup
-- >>> import Protolude

-- | The steps of a request, in the order of the Server-Timing header
data Metric
  = Jwt
  | Parse
  | Plan
  | Transaction
  | Response
  deriving (Eq, Ord, Show)

-- | Measures the durations of the steps of a request
class Timer t where
  -- | Start measuring a step; the returned action records its duration
  startTimer :: t -> Metric -> IO (IO ())

  -- | The recorded durations, in milliseconds
  recordedTimings :: t -> IO [(Metric, Double)]

-- | Measures nothing, used when the Server-Timing header is disabled.
-- GHC specialises the code using it so that timing costs nothing.
data NoTimer = NoTimer

instance Timer NoTimer where
  startTimer _ _ = pure (pure ())
  recordedTimings _ = pure []

-- | Records the durations of the steps of a request for the Server-Timing header
newtype ServerTimer = ServerTimer (IORef [(Metric, Double)])

newServerTimer :: IO ServerTimer
newServerTimer = ServerTimer <$> newIORef []

instance Timer ServerTimer where
  startTimer (ServerTimer ref) metric = do
    start <- getMonotonicTimeNSec
    pure $ do
      end <- getMonotonicTimeNSec
      modifyIORef' ref ((metric, fromIntegral (end - start) / 1e6) :)
  recordedTimings (ServerTimer ref) = readIORef ref

-- | Time a step, recording its duration also when it fails
timed :: (MonadError e m, MonadIO m, Timer t) => t -> Metric -> m a -> m a
timed timer metric step = do
  stop <- liftIO $ startTimer timer metric
  res <- (Right <$> step) `catchError` (pure . Left)
  liftIO stop
  liftEither res

-- | Render the Server-Timing header from the recorded durations, if any
-- The duration precision is milliseconds, per the docs
--
-- >>> serverTimingHeader [(Response, 0.3), (Transaction, 0.2), (Plan, 0.1), (Parse, 0.5), (Jwt, 0.4)]
-- Just ("Server-Timing","jwt;dur=0.4, parse;dur=0.5, plan;dur=0.1, transaction;dur=0.2, response;dur=0.3")
serverTimingHeader :: [(Metric, Double)] -> Maybe HTTP.Header
serverTimingHeader [] = Nothing
serverTimingHeader timings =
  Just ("Server-Timing", BS.intercalate ", " $ renderMetric <$> sortOn fst timings)
  where
    renderMetric (metric, dur) = BS.concat [metricName metric, BS.pack $ ";dur=" <> showFFloat (Just 1) dur ""]
    metricName = \case
      Jwt -> "jwt"
      Parse -> "parse"
      Plan -> "plan"
      Transaction -> "transaction"
      Response -> "response"
