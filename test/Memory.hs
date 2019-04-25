module Main where

import qualified Data.Aeson           as JSON
import           Data.Map             (fromList)
import           Network.Wai          (Application)
import           SpecHelper           (posgrestTestApp, requestAction,
                                       testCfg)

import           Data.ByteString.Lazy (ByteString, replicate)
import           Network.Wai.Test     (SResponse)
import           Test.Hspec.Wai       (WaiSession, get, patch, post)
import           Weigh                (Weigh, mainWith, maxAllocs,
                                       validateAction)


import           Protolude            hiding (ByteString, get,
                                       replicate)


newtype MegaBytes = MegaBytes Double


main :: IO ()
main = do
  postgrestApp <- posgrestTestApp
  let testMaxAllocationMB = appMaxAlocationTesterMB (postgrestApp testCfg)

  mainWith $ do
    testMaxAllocationMB "root request"
                        (get "/") $
                        MegaBytes 30

    testMaxAllocationMB "post rpc request 1M"
                        (post "/rpc/leak?columns=blob"           $ jsonBlob 1) $
                        MegaBytes 8
    testMaxAllocationMB "post request 1M"
                        (post "/leak?columns=blob"               $ jsonBlob 1) $
                        MegaBytes 8
    testMaxAllocationMB "patch request 1M"
                        (patch "/leak/leak?id=eq.1&columns=blob" $ jsonBlob 1) $
                        MegaBytes 5
    --
    --
    testMaxAllocationMB "post rpc request 10M"
                        (post "/rpc/leak?columns=blob"           $ jsonBlob 10) $
                        MegaBytes 75
    testMaxAllocationMB "post request 10M"
                        (post "/leak?columns=blob"               $ jsonBlob 10) $
                        MegaBytes 75
    testMaxAllocationMB "patch request 10M"
                        (patch "/leak/leak?id=eq.1&columns=blob" $ jsonBlob 10) $
                        MegaBytes 41

    --
    testMaxAllocationMB "post rpc request 50M"
                        (post "/rpc/leak?columns=blob"           $ jsonBlob 50) $
                        MegaBytes 355
    testMaxAllocationMB "post request 50M"
                        (post "/leak?columns=blob"               $ jsonBlob 50) $
                        MegaBytes 355
    testMaxAllocationMB "patch request 50M"
                        (patch "/leak/leak?id=eq.1&columns=blob" $ jsonBlob 50) $
                        MegaBytes 205


appMaxAlocationTesterMB :: Application -> Text -> WaiSession SResponse -> MegaBytes -> Weigh ()
appMaxAlocationTesterMB app testDescription req maxBytes =
  validateAction (toS testDescription) (requestAction app) req guardAllocLimit
    where guardAllocLimit = maxAllocs . inBytes $ maxBytes


inBytes :: (Integral a) => MegaBytes -> a
inBytes (MegaBytes n) = round . (* 1024) . (* 1024) $ n

jsonBlob :: Double -> ByteString
jsonBlob n = JSON.encode $ Data.Map.fromList payload
  where payload :: [(Text, Text)]
        payload = [("blob", blob)]

        blob :: Text
        blob = toS $ replicate (inBytes $ MegaBytes n) 52
