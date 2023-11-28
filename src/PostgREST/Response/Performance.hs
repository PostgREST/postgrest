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
  | SMParse
  | SMPlan
  | SMTransaction
  | SMResp
  deriving (Show, Eq, Ord)
type ServerTimingData = Map ServerMetric (Maybe Double)

-- | Render the Server-Timing header from a ServerTimingData
--
-- >>> renderServerTimingHeader $ Map.fromList [(SMPlan, Just 0.1), (SMTransaction, Just 0.2), (SMResp, Just 0.3), (SMJwt, Just 0.4), (SMParse, Just 0.5)]
-- ("Server-Timing","jwt;dur=400000.0, parse;dur=500000.0, plan;dur=100000.0, transaction;dur=200000.0, response;dur=300000.0")
renderServerTimingHeader :: ServerTimingData -> HTTP.Header
renderServerTimingHeader timingData =
  ("Server-Timing", BS.intercalate ", " $ map renderTiming $ Map.toList timingData)
renderTiming :: (ServerMetric, Maybe Double) -> BS.ByteString
renderTiming (metric, time) = maybe "" (\x -> BS.concat [renderMetric metric, BS.pack $ ";dur=" <> showFFloat (Just 1) (x * 1000000) ""]) time
  where
    renderMetric SMJwt         = "jwt"
    renderMetric SMParse       = "parse"
    renderMetric SMPlan        = "plan"
    renderMetric SMTransaction = "transaction"
    renderMetric SMResp        = "response"
