-- Tests the performance of JWT verification, see https://github.com/PostgREST/postgrest/issues/5379
module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Lens (view)
import Control.Monad (replicateM_)
import Data.Aeson ((.=))
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (..))
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)
import Prelude

import Crypto.JOSE qualified as JOSE
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as LBS
import Jose.Jwk qualified as JWT
import Jose.Jwt qualified as JWT
import Options.Applicative qualified as O

main :: IO ()
main = do
  algorithm <- readCLI

  -- init
  (jwsAlgorithm, publicKey, privateKey) <- generateKeyPair algorithm
  tokens' <- mapM (generateToken jwsAlgorithm privateKey) [1 .. uniqueTokens]
  tokens <- evaluate $ force tokens' -- force the evaluation before the verification phase, otherwise due to laziness it will be evaluated later

  -- bench
  startNSecs <- getMonotonicTimeNSec
  replicateM_ times $ mapM_ (verify $ JWT.JwkSet [publicKey]) tokens
  endNSecs <- getMonotonicTimeNSec

  -- report
  let
    totalVerifications = uniqueTokens * times
    benchSeconds = fromIntegral (endNSecs - startNSecs) / 1e9 :: Double
    tokensPerSecond = fromIntegral totalVerifications / benchSeconds
   in
    printf "- %s: %.0f verify/s (%d verifications, %.3fs)\n" (show algorithm) tokensPerSecond totalVerifications benchSeconds
  where
    -- These numbers were hand tuned, if we generate too many unique tokens the signing phase takes too long.
    uniqueTokens = 1000
    -- So we verify the uniqueTokens a number of times. Ed25519 is really fast, if we just run times = 2 then `postgrest-loadtest-jwt` fails because the `monitor_pid.py`
    -- doesn't have a chance to capture the process CPU/MEM. So we choose 10 for good balance.
    times = 10

-- | PostgREST supports more algorithms (see https://github.com/PostgREST/postgrest/issues/5379) but we only load test these variants
data CliAlgorithm = RS256 | ES256 | Ed25519 deriving (Show)

readCLI :: IO CliAlgorithm
readCLI = O.execParser $ O.info (O.helper <*> algo) (O.progDesc "JWT Verification Loadtest")
  where
    algo = O.option (O.eitherReader readAlgo) $ O.long "algorithm"
    readAlgo "RS256" = Right RS256
    readAlgo "ES256" = Right ES256
    readAlgo "Ed25519" = Right Ed25519
    readAlgo _ = Left "Unknown algorithm"

verify :: JWT.JwkSet -> ByteString -> IO ()
verify jwks token = do
  result <- JWT.decode (JWT.keys jwks) Nothing token
  case result of
    Left err ->
      ioError $ userError $ show err
    Right _ ->
      pure () -- just discard the value

generateKeyPair :: CliAlgorithm -> IO (JOSE.Alg, JWT.Jwk, JOSE.JWK)
generateKeyPair algorithm = do
  let (jwsAlgorithm, params) = case algorithm of
        RS256 ->
          -- The key size is taken from the example on hs-jose
          -- https://github.com/frasertweedale/hs-jose/blob/ffd6a66c2e2844ec5e506806963f4e46accf1ae9/src/Crypto/JOSE/JWK.hs#L34
          let keySize = 4096 `div` 8
          in  (JOSE.RS256, JOSE.RSAGenParam keySize)
        ES256 -> (JOSE.ES256, JOSE.ECGenParam JOSE.P_256)
        Ed25519 -> (JOSE.EdDSA, JOSE.OKPGenParam JOSE.Ed25519)
  privateKey <- JOSE.genJWK params
  case JSON.fromJSON $ JSON.toJSON $ view JOSE.asPublicKey privateKey of
    JSON.Error err -> ioError $ userError err
    JSON.Success publicKey -> pure (jwsAlgorithm, publicKey, privateKey)

generateToken :: JOSE.Alg -> JOSE.JWK -> Int -> IO ByteString
generateToken alg privateKey tokenId = do
  result <- JOSE.runJOSE $ JOSE.signJWS claims $ Identity (header, privateKey)
  case result :: Either JOSE.Error (JOSE.CompactJWS JOSE.JWSHeader) of
    Left err -> ioError $ userError $ show err
    Right jwt -> pure $ LBS.toStrict $ JOSE.encodeCompact jwt
  where
    header = JOSE.newJWSHeaderProtected alg
    -- This is a representative JWT, similar to one generated on nix/tools/generate_targets.py
    claims = JSON.encode $ JSON.object ["role" .= ("postgrest_test_author" :: String), "sub" .= ("user_" <> show tokenId)]
