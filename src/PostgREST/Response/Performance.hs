module PostgREST.Response.Performance
  ( ServerMetric(..)
  , ServerTimingData
  , renderServerTimingHeader
  )
where
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as Map
import qualified Network.HTTP.Types    as HTTP
import           Numeric               (showFFloat)
import           Protolude

data ServerMetric =
    SMJwt
  | SMRender
  | SMPlan
  | SMTransaction
  deriving (Show, Eq, Ord)
type ServerTimingData = Map ServerMetric (Maybe Double)

-- | Render the Server-Timing header from a ServerTimingData
--
-- >>> renderServerTimingHeader $ Map.fromList [(SMPlan, 0.1), (SMTransaction, 0.2), (SMRender, 0.3), (SMJwt, 0.4)]
-- ("Server-Timing","jwt;dur=400000.0, render;dur=300000.0, plan;dur=100000.0, query;dur=200000.0")
renderServerTimingHeader :: ServerTimingData -> HTTP.Header
renderServerTimingHeader timingData =
  ("Server-Timing", BS.intercalate ", " $ map renderTiming $ Map.toList timingData)
renderTiming :: (ServerMetric, Maybe Double) -> BS.ByteString
renderTiming (metric, time) = maybe "" (\x -> BS.concat [renderMetric metric, BS.pack $ ";dur=" <> showFFloat (Just 1) (x * 1000000) ""]) time
  where
    renderMetric SMPlan        = "plan"
    renderMetric SMTransaction = "transaction"
    renderMetric SMRender      = "render"
    renderMetric SMJwt         = "jwt"
