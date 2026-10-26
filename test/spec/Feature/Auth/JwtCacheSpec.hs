module Feature.Auth.JwtCacheSpec where

import Network.HTTP.Types
import Network.Wai.Test (SResponse (simpleHeaders, simpleStatus))
import Protolude hiding (get)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as M

import PostgREST.Auth.JwtCache (JwtKey (..))
import PostgREST.Config (AppConfig (..))
import SpecHelper

spec :: SpecWithConfig
spec withConfig = do
  describe "JwtKey" $ do
    let
      inputs =
        [ "",
          "a",
          "eyJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYSJ9.c2lnbmF0dXJl",
          BS.pack $ take 4096 $ cycle [0 .. 255],
          BS.pack $ take 100003 $ cycle [255, 254 .. 0]
        ]
      -- a slice of a larger buffer, as taken from a header
      sliceOf bs = BS.take (BS.length bs) $ BS.drop 7 $ "prefix-" <> bs <> "-suffix"
      stored = Stored . SBS.toShort

    it "hashes and compares a lookup key like the stored copy" $
      forM_ inputs $ \bs -> do
        Lookup bs `shouldBe` stored bs
        stored bs `shouldBe` Lookup bs
        Lookup (sliceOf bs) `shouldBe` stored bs
        Lookup bs `shouldBe` Lookup (sliceOf bs)
        stored bs `shouldBe` stored bs
        hash (Lookup bs) `shouldBe` hash (stored bs)
        forM_ [0, 1, 42, -7, maxBound] $ \salt ->
          hashWithSalt salt (Lookup bs) `shouldBe` hashWithSalt salt (stored bs)

    it "does not equate keys with different bytes" $
      forM_ (filter (not . BS.null) inputs) $ \bs -> do
        let changed = BS.init bs <> BS.singleton (BS.last bs + 1)
        Lookup changed `shouldNotBe` stored bs
        stored bs `shouldNotBe` Lookup changed
        Lookup (BS.init bs) `shouldNotBe` stored bs
        stored bs `shouldNotBe` Lookup (BS.init bs)
        Lookup (bs <> "x") `shouldNotBe` stored bs
        stored (BS.init bs) `shouldNotBe` stored bs

  withConfig baseCfg{configJwtCacheMaxEntries = 86400} $ do
    it "server-timing duration is exposed for JWT with expiry" $ do
      !currentTime <- liftIO $ relativeSeconds 1800 -- 30 minutes, evaluate strictly
      let
        jwtPayload = [json|{ "role": "postgrest_test_author", "exp": #{currentTime} }|]
        auth = authHeaderJWT $ generateJWT jwtPayload

      res1 <- request methodGet "/authors_only" [auth] ""
      let jwtDur1 = M.lookup "jwt" $ parseServerTimingHeader $ simpleHeaders res1
      res2 <- request methodGet "/authors_only" [auth] ""
      let jwtDur2 = M.lookup "jwt" $ parseServerTimingHeader $ simpleHeaders res2

      -- With jwt caching the parse time of second request with the same token
      -- should be at least as fast as the first one
      let dur2IsLessThanEq = fromMaybe False $ liftA2 (<=) jwtDur2 jwtDur1
      liftIO $ dur2IsLessThanEq `shouldBe` True

    it "server-timing duration is exposed for JWT without expiry" $ do
      let
        jwtPayload = [json|{ "role": "postgrest_test_author" }|]
        auth = authHeaderJWT $ generateJWT jwtPayload

      res1 <- request methodGet "/authors_only" [auth] ""
      let jwtDur1 = M.lookup "jwt" $ parseServerTimingHeader $ simpleHeaders res1
      res2 <- request methodGet "/authors_only" [auth] ""
      let jwtDur2 = M.lookup "jwt" $ parseServerTimingHeader $ simpleHeaders res2

      liftIO $ do
        simpleStatus res1 `shouldBe` status200
        simpleStatus res2 `shouldBe` status200

      let dur1Positive = maybe False (>= 0) jwtDur1
      let dur2Positive = maybe False (>= 0) jwtDur2
      liftIO $ do
        dur1Positive `shouldBe` True
        dur2Positive `shouldBe` True

  withConfig baseCfg{configServerTimingEnabled = False, configJwtCacheMaxEntries = 86400} $
    it "JWT cache does not break requests with server-timing disabled" $ do
      let
        jwtPayload = [json|{ "role": "postgrest_test_author" }|]
        auth = authHeaderJWT $ generateJWT jwtPayload

      request methodGet "/authors_only" [auth] "" `shouldRespondWith` 200
      request methodGet "/authors_only" [auth] "" `shouldRespondWith` 200
