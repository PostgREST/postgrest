module SpecHelper where

import Data.String.Conversions (cs)
import Control.Monad (void)

import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Text.Regex.TDFA ((=~))
import qualified Data.ByteString.Char8 as BS
import System.Process (readProcess)

import PostgREST.Config (AppConfig(..))

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai

import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleStatus, simpleHeaders, simpleBody))

import Data.Maybe (fromJust)
import Data.Aeson (decode)
import qualified Data.JsonSchema.Draft4 as D4

import Data.Text

validateOpenApiResponse :: [Header] -> WaiSession ()
validateOpenApiResponse headers = do
  r <- request methodGet "/" headers ""
  liftIO $
    let respStatus = simpleStatus r in
    respStatus `shouldSatisfy`
      \s -> s == Status { statusCode = 200, statusMessage="OK" }
  liftIO $
    let respHeaders = simpleHeaders r in
    respHeaders `shouldSatisfy`
      \hs -> ("Content-Type", "application/openapi+json; charset=utf-8") `elem` hs
  liftIO $
    let respBody = simpleBody r
        schema :: D4.Schema
        schema = D4.emptySchema { D4._schemaRef = Just "openapi.json" }
        schemaContext :: D4.SchemaWithURI D4.Schema
        schemaContext = D4.SchemaWithURI
          { D4._swSchema = schema
          , D4._swURI    = Just "test/fixtures/openapi.json"
          }
       in
       D4.fetchFilesystemAndValidate schemaContext ((fromJust . decode) respBody) `shouldReturn` Right ()

testDbConn :: Text
testDbConn = "postgres://postgrest_test_authenticator@localhost:5432/postgrest_test"

testCfg :: AppConfig
testCfg =
  AppConfig testDbConn "postgrest_test_anonymous" Nothing "test" "localhost" 3000 (Just "safe") 10 Nothing (Just "test.block_bad_role") True

testCfgNoJWT :: AppConfig
testCfgNoJWT =
  AppConfig testDbConn "postgrest_test_anonymous" Nothing "test" "localhost" 3000 Nothing 10 Nothing Nothing True

testUnicodeCfg :: AppConfig
testUnicodeCfg =
  AppConfig testDbConn "postgrest_test_anonymous" Nothing "تست" "localhost" 3000 (Just "safe") 10 Nothing Nothing True

testLtdRowsCfg :: AppConfig
testLtdRowsCfg =
  AppConfig testDbConn "postgrest_test_anonymous" Nothing "test" "localhost" 3000 (Just "safe") 10 (Just 2) Nothing True

testProxyCfg :: AppConfig
testProxyCfg =
  AppConfig testDbConn "postgrest_test_anonymous" (Just "https://postgrest.com/openapi.json") "test" "localhost" 3000 (Just "safe") 10 Nothing Nothing True

setupDb :: IO ()
setupDb = do
  void $ readProcess "psql" ["-d", "postgres", "-a", "-f", "test/fixtures/database.sql"] []
  void $ readProcess "psql" ["-d", "postgrest_test", "-a", "-c", "CREATE EXTENSION IF NOT EXISTS pgcrypto;"] []
  loadFixture "roles"
  loadFixture "schema"
  loadFixture "jwt"
  loadFixture "privileges"
  resetDb

resetDb :: IO ()
resetDb = loadFixture "data"

loadFixture :: FilePath -> IO()
loadFixture name =
  void $ readProcess "psql" ["-U", "postgrest_test", "-d", "postgrest_test", "-a", "-f", "test/fixtures/" ++ name ++ ".sql"] []

rangeHdrs :: ByteRange -> [Header]
rangeHdrs r = [rangeUnit, (hRange, renderByteRange r)]

rangeHdrsWithCount :: ByteRange -> [Header]
rangeHdrsWithCount r = ("Prefer", "count=exact") : rangeHdrs r

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
