module PostgREST.Response.Performance
  ( serverTimingHeader
  )
where
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types    as HTTP
import           Numeric               (showFFloat)
import           Protolude

-- | Render the Server-Timing header from a ServerTimingData
-- The duration precision is milliseconds, per the docs
--
-- >>> serverTimingHeader ServerTiming { plan=Just 0.1, transaction=Just 0.2, response=Just 0.3, jwt=Just 0.4, parse=Just 0.5}
-- ("Server-Timing","jwt;dur=0.4, parse;dur=0.5, plan;dur=0.1, transaction;dur=0.2, response;dur=0.3")
serverTimingHeader :: NonEmpty (ByteString, Double) -> HTTP.Header
serverTimingHeader timings =
  ("Server-Timing", renderTiming)
  where
    renderMetric metric dur = BS.concat [metric, BS.pack $ ";dur=" <> showFFloat (Just 1) dur ""]
    renderTiming = BS.intercalate ", " $ uncurry renderMetric <$> toList timings
