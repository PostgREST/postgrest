module PostgREST.Response.Performance
  ( Timing
  , serverTimingHeader
  )
where
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types    as HTTP
import           Numeric               (showFFloat)
import           Protolude

type Timing = (ByteString, Double)

-- | Render the Server-Timing header from a ServerTimingData
-- The duration precision is milliseconds, per the docs
--
-- >>> serverTimingHeader $ ("jwt",0.4) :| [("parse",0.5), ("plan",0.1), ("transaction",0.2), ("response",0.3)]
-- ("Server-Timing","jwt;dur=0.4, parse;dur=0.5, plan;dur=0.1, transaction;dur=0.2, response;dur=0.3")
serverTimingHeader :: NonEmpty Timing -> HTTP.Header
serverTimingHeader timings =
  ("Server-Timing", renderTiming)
  where
    renderMetric (metric, dur) = BS.concat [metric, BS.pack $ ";dur=" <> showFFloat (Just 1) dur ""]
    renderTiming = BS.intercalate ", " $ renderMetric <$> toList timings
