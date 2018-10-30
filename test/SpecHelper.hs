module SpecHelper where

import Control.Monad (void)

import qualified System.IO.Error as E
import System.Environment (getEnv)

import qualified Data.ByteString.Base64 as B64 (encode, decodeLenient)
import Data.CaseInsensitive (CI(..))
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (lookup)
import Text.Regex.TDFA ((=~))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           System.Process (readProcess)
import           Text.Heredoc

import PostgREST.Config (AppConfig(..))
import PostgREST.Types  (JSPathExp(..))

import Test.Hspec
import Test.Hspec.Wai

import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleStatus, simpleHeaders, simpleBody))

import Data.Maybe (fromJust)
import Data.Aeson (decode, Value(..))
import qualified JSONSchema.Draft4 as D4

import Protolude

matchContentTypeJson :: MatchHeader
matchContentTypeJson = "Content-Type" <:> "application/json; charset=utf-8"

matchContentTypeSingular :: MatchHeader
matchContentTypeSingular = "Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"

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
getEnvVarWithDefault var def = toS <$>
  getEnv (toS var) `E.catchIOError` const (return $ toS def)

_baseCfg :: AppConfig
_baseCfg =  -- Connection Settings
  AppConfig mempty "postgrest_test_anonymous" Nothing "test" "localhost" 3000
            -- Jwt settings
            (Just $ encodeUtf8 "reallyreallyreallyreallyverysafe") False Nothing
            -- Connection Modifiers
            10 Nothing (Just "test.switch_role")
            -- Debug Settings
            True
            [ ("app.settings.app_host", "localhost")
            , ("app.settings.external_api_secret", "0123456789abcdef")
            ]
            -- Default role claim key
            (Right [JSPKey "role"])

testCfg :: Text -> AppConfig
testCfg testDbConn = _baseCfg { configDatabase = testDbConn }

testCfgNoJWT :: Text -> AppConfig
testCfgNoJWT testDbConn = (testCfg testDbConn) { configJwtSecret = Nothing }

testUnicodeCfg :: Text -> AppConfig
testUnicodeCfg testDbConn = (testCfg testDbConn) { configSchema = "تست" }

testLtdRowsCfg :: Text -> AppConfig
testLtdRowsCfg testDbConn = (testCfg testDbConn) { configMaxRows = Just 2 }

testProxyCfg :: Text -> AppConfig
testProxyCfg testDbConn = (testCfg testDbConn) { configProxyUri = Just "https://postgrest.com/openapi.json" }

testCfgBinaryJWT :: Text -> AppConfig
testCfgBinaryJWT testDbConn = (testCfg testDbConn) {
    configJwtSecret = Just . B64.decodeLenient $
      "cmVhbGx5cmVhbGx5cmVhbGx5cmVhbGx5dmVyeXNhZmU="
  }

testCfgAudienceJWT :: Text -> AppConfig
testCfgAudienceJWT testDbConn = (testCfg testDbConn) {
    configJwtSecret = Just . B64.decodeLenient $
      "cmVhbGx5cmVhbGx5cmVhbGx5cmVhbGx5dmVyeXNhZmU=",
    configJwtAudience = Just "youraudience"
  }

testCfgAsymJWK :: Text -> AppConfig
testCfgAsymJWK testDbConn = (testCfg testDbConn) {
    configJwtSecret = Just $ encodeUtf8
      [str|{"alg":"RS256","e":"AQAB","key_ops":["verify"],"kty":"RSA","n":"0etQ2Tg187jb04MWfpuogYGV75IFrQQBxQaGH75eq_FpbkyoLcEpRUEWSbECP2eeFya2yZ9vIO5ScD-lPmovePk4Aa4SzZ8jdjhmAbNykleRPCxMg0481kz6PQhnHRUv3nF5WP479CnObJKqTVdEagVL66oxnX9VhZG9IZA7k0Th5PfKQwrKGyUeTGczpOjaPqbxlunP73j9AfnAt4XCS8epa-n3WGz1j-wfpr_ys57Aq-zBCfqP67UYzNpeI1AoXsJhD9xSDOzvJgFRvc3vm2wjAW4LEMwi48rCplamOpZToIHEPIaPzpveYQwDnB1HFTR1ove9bpKJsHmi-e2uzQ","use":"sig"}|]
  }

testNonexistentSchemaCfg :: Text -> AppConfig
testNonexistentSchemaCfg testDbConn = (testCfg testDbConn) { configSchema = "nonexistent" }

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
  (hAuthorization, "Basic " <> (toS . B64.encode . toS $ u <> ":" <> p))

authHeaderJWT :: BS.ByteString -> Header
authHeaderJWT token =
  (hAuthorization, "Bearer " <> token)

-- | Tests whether the text can be parsed as a json object comtaining
-- the key "message", and optional keys "details", "hint", "code",
-- and no extraneous keys
isErrorFormat :: BL.ByteString -> Bool
isErrorFormat s =
  "message" `S.member` keys &&
    S.null (S.difference keys validKeys)
 where
  obj = decode s :: Maybe (M.Map Text Value)
  keys = maybe S.empty M.keysSet obj
  validKeys = S.fromList ["message", "details", "hint", "code"]
