module Main where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as BS

import Data.Aeson       ((.=))
import Network.Wai      (Application)
import Network.Wai.Test (SResponse)
import SpecHelper       (posgrestTestApp, requestAction,
                         testCfgMemory)
import Test.Hspec.Wai   (WaiSession, get, patch, post)
import Weigh            (Weigh, mainWith, maxAllocs, validateAction,
                         wgroup)

import Protolude hiding (get)


newtype MegaBytes = MegaBytes Double


main :: IO ()
main = do
  postgrestApp <- posgrestTestApp
  let test = testMaxAllocationMB $ postgrestApp testCfgMemory

  mainWith $ do
    test "root request" (get "/") $ MegaBytes 30
    jsonKeyTest1M test
    jsonKeyTest10M test
    jsonKeyTest50M test
    jsonArrayTest test

type MemoryCaseTester = (Text -> WaiSession SResponse -> MegaBytes -> Weigh ())

jsonKeyTest1M :: MemoryCaseTester -> Weigh ()
jsonKeyTest1M test = wgroup "json key test - 1M blob" $ do
    test "post rpc request"
         (post "/rpc/leak?columns=blob"           $ jsonBlob 1) $
         MegaBytes 10
    test "post request"
         (post "/leak?columns=blob"               $ jsonBlob 1) $
         MegaBytes 10
    test "patch request"
         (patch "/leak?id=eq.1&columns=blob"      $ jsonBlob 1) $
         MegaBytes 10

jsonKeyTest10M :: MemoryCaseTester -> Weigh ()
jsonKeyTest10M test = wgroup "json key test - 10M blob" $ do
    test "post rpc request"
         (post "/rpc/leak?columns=blob"           $ jsonBlob 10) $
         MegaBytes 75
    test "post request"
         (post "/leak?columns=blob"               $ jsonBlob 10) $
         MegaBytes 75
    test "patch request"
         (patch "/leak?id=eq.1&columns=blob"      $ jsonBlob 10) $
         MegaBytes 75

jsonKeyTest50M :: MemoryCaseTester -> Weigh ()
jsonKeyTest50M test = wgroup "json key test - 50M blob" $ do
    test "post rpc request"
         (post "/rpc/leak?columns=blob"           $ jsonBlob 50) $
         MegaBytes 355
    test "post request"
         (post "/leak?columns=blob"               $ jsonBlob 50) $
         MegaBytes 355
    test "patch request"
         (patch "/leak?id=eq.1&columns=blob"      $ jsonBlob 50) $
         MegaBytes 355

jsonArrayTest :: MemoryCaseTester -> Weigh ()
jsonArrayTest test = wgroup "POST json array test" $ do
    test "1_000 elements"
      (post "/perf_articles/?columns=id,body" $ jsonArray 1000) $
      MegaBytes 10

    test "10_000 elements"
      (post "/perf_articles/?columns=id,body" $ jsonArray 10000) $
      MegaBytes 25

    test "100_000 elements"
      (post "/perf_articles/?columns=id,body" $ jsonArray 100000) $
      MegaBytes 215


testMaxAllocationMB :: Application -> Text -> WaiSession SResponse -> MegaBytes -> Weigh ()
testMaxAllocationMB postgrestApp testDescription req maxBytes =
  validateAction (toS testDescription)
                 (requestAction postgrestApp)
                 req
                 guardAllocLimit
    where guardAllocLimit = maxAllocs . inBytes $ maxBytes

inBytes :: (Integral a) => MegaBytes -> a
inBytes (MegaBytes n) = round . (* 1024) . (* 1024) $ n


jsonBlob :: Double -> BS.ByteString
jsonBlob size = JSON.encode $ JSON.object ["blob" .= blob]
  where blob :: Text
        blob = toS $ BS.replicate (inBytes $ MegaBytes size) 52

jsonArray :: Int -> BS.ByteString
jsonArray size = JSON.encode . take size $ baseObject <$> [1..]
  where baseObject :: Int -> JSON.Value
        baseObject n = JSON.object ["id" .= n, "body" .= ("xxxxxxx" :: Text)]
