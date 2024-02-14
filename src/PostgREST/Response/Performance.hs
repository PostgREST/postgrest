{-# LANGUAGE NumericUnderscores #-}
module PostgREST.Response.Performance
  ( ServerTiming (..)
  , serverTimingHeader
  )
where
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types    as HTTP
import           Numeric               (showFFloat)
import           Protolude

-- | ServerTiming represents the timing data for a request, in seconds.
data ServerTiming =
  ServerTiming
    { jwt         :: Maybe Double
    , parse       :: Maybe Double
    , plan        :: Maybe Double
    , transaction :: Maybe Double
    , response    :: Maybe Double
    }
  deriving (Show)

-- | Render the Server-Timing header from a ServerTimingData
-- The duration precision is milliseconds, per the docs
--
-- >>> serverTimingHeader ServerTiming { plan=Just 0.1, transaction=Just 0.2, response=Just 0.3, jwt=Just 0.4, parse=Just 0.5}
-- ("Server-Timing","jwt;dur=400.0, parse;dur=500.0, plan;dur=100.0, transaction;dur=200.0, response;dur=300.0")
serverTimingHeader :: ServerTiming -> HTTP.Header
serverTimingHeader timing =
  ("Server-Timing", renderTiming)
  where
    renderMetric metric = maybe "" (\dur -> BS.concat [metric, BS.pack $ ";dur=" <> showFFloat (Just 1) (dur * 1_000) ""])
    renderTiming = BS.intercalate ", " $ (\(k, v) -> renderMetric k (v timing)) <$>
      [ ("jwt", jwt)
      , ("parse", parse)
      , ("plan", plan)
      , ("transaction", transaction)
      , ("response", response)
      ]
