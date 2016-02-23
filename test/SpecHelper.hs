module SpecHelper where

import Data.String.Conversions (cs)
import Control.Monad (void)

import Network.HTTP.Types.Header (Header, ByteRange, renderByteRange,
                                  hRange, hAuthorization, hAccept)
import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Text.Regex.TDFA ((=~))
import qualified Data.ByteString.Char8 as BS
import System.Process (readProcess)
import Web.JWT (secret)

import PostgREST.Config (AppConfig(..))

testDbConn :: String
testDbConn = "postgres://postgrest_test_authenticator@localhost:5432/postgrest_test"

testCfg :: AppConfig
testCfg =
  AppConfig testDbConn "postgrest_test_anonymous" "test" 3000 (secret "safe") 10 Nothing True

setupDb :: IO ()
setupDb = do
  void $ readProcess "psql" ["-d", "postgres", "-a", "-f", "test/fixtures/database.sql"] []
  loadFixture "roles"
  loadFixture "schema"
  loadFixture "privileges"
  resetDb

resetDb :: IO ()
resetDb = loadFixture "data"

loadFixture :: FilePath -> IO()
loadFixture name =
  void $ readProcess "psql" ["-U", "postgrest_test", "-d", "postgrest_test", "-a", "-f", "test/fixtures/" ++ name ++ ".sql"] []

rangeHdrs :: ByteRange -> [Header]
rangeHdrs r = [rangeUnit, (hRange, renderByteRange r)]

acceptHdrs :: BS.ByteString -> [Header]
acceptHdrs mime = [(hAccept, mime)]

rangeUnit :: Header
rangeUnit = ("Range-Unit" :: CI BS.ByteString, "items")

matchHeader :: CI BS.ByteString -> String -> [Header] -> Bool
matchHeader name valRegex headers =
  maybe False (=~ valRegex) $ lookup name headers

authHeaderBasic :: String -> String -> Header
authHeaderBasic u p =
  (hAuthorization, cs $ "Basic " ++ encode (u ++ ":" ++ p))

authHeaderJWT :: String -> Header
authHeaderJWT token =
  (hAuthorization, cs $ "Bearer " ++ token)
