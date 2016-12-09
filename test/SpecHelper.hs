module SpecHelper where

import Control.Monad (void)

import qualified System.IO.Error as E
import System.Environment (getEnv)

import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Data.List (lookup)
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

import Protolude

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

getEnvVarWithDefault :: Text -> Text -> IO Text
getEnvVarWithDefault var def = do
  varValue <- getEnv (toS var) `E.catchIOError` const (return $ toS def)
  return $ toS varValue

testCfg :: Text -> AppConfig
testCfg testDbConn =
  AppConfig testDbConn "postgrest_test_anonymous" Nothing "test" "localhost" 3000 (Just "safe") 10 Nothing (Just "test.switch_role") True

testCfgNoJWT :: Text -> AppConfig
testCfgNoJWT testDbConn =
  AppConfig testDbConn "postgrest_test_anonymous" Nothing "test" "localhost" 3000 Nothing 10 Nothing Nothing True

testUnicodeCfg :: Text -> AppConfig
testUnicodeCfg testDbConn =
  AppConfig testDbConn "postgrest_test_anonymous" Nothing "تست" "localhost" 3000 (Just "safe") 10 Nothing Nothing True

testLtdRowsCfg :: Text -> AppConfig
testLtdRowsCfg testDbConn =
  AppConfig testDbConn "postgrest_test_anonymous" Nothing "test" "localhost" 3000 (Just "safe") 10 (Just 2) Nothing True

testProxyCfg :: Text -> AppConfig
testProxyCfg testDbConn =
  AppConfig testDbConn "postgrest_test_anonymous" (Just "https://postgrest.com/openapi.json") "test" "localhost" 3000 (Just "safe") 10 Nothing Nothing True

setupDb :: Text -> IO ()
setupDb dbConn = do
  loadFixture dbConn "database"
  loadFixture dbConn "roles"
  loadFixture dbConn "schema"
  loadFixture dbConn "jwt"
  loadFixture dbConn "privileges"
  resetDb dbConn

resetDb :: Text -> IO ()
resetDb dbConn = loadFixture dbConn "data"

loadFixture :: Text -> FilePath -> IO()
loadFixture dbConn name =
  void $ readProcess "psql" [toS dbConn, "-a", "-f", "test/fixtures/" ++ name ++ ".sql"] []

rangeHdrs :: ByteRange -> [Header]
rangeHdrs r = [rangeUnit, (hRange, renderByteRange r)]

rangeHdrsWithCount :: ByteRange -> [Header]
rangeHdrsWithCount r = ("Prefer", "count=exact") : rangeHdrs r

acceptHdrs :: BS.ByteString -> [Header]
acceptHdrs mime = [(hAccept, mime)]

rangeUnit :: Header
rangeUnit = ("Range-Unit" :: CI BS.ByteString, "items")

matchHeader :: CI BS.ByteString -> BS.ByteString -> [Header] -> Bool
matchHeader name valRegex headers =
  maybe False (=~ valRegex) $ lookup name headers

authHeaderBasic :: BS.ByteString -> BS.ByteString -> Header
authHeaderBasic u p =
  (hAuthorization, "Basic " <> (toS . encode . toS $ u <> ":" <> p))

authHeaderJWT :: BS.ByteString -> Header
authHeaderJWT token =
  (hAuthorization, "Bearer " <> token)
