module SpecHelper where

import qualified Data.ByteString.Base64 as B64 (decodeLenient, encode)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import qualified System.IO.Error        as E

import Data.Aeson           (Value (..), decode, encode)
import Data.CaseInsensitive (CI (..), original)
import Data.List            (lookup)
import Data.List.NonEmpty   (fromList)
import Network.Wai.Test     (SResponse (simpleBody, simpleHeaders, simpleStatus))
import System.Environment   (getEnv)
import System.Process       (readProcess)
import Text.Regex.TDFA      ((=~))


import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Text.Heredoc

import PostgREST.Config (AppConfig (..), LogLevel (..), parseSecret)
import PostgREST.Types  (JSPathExp (..))
import Protolude        hiding (toS)
import Protolude.Conv   (toS)

matchContentTypeJson :: MatchHeader
matchContentTypeJson = "Content-Type" <:> "application/json; charset=utf-8"

matchContentTypeSingular :: MatchHeader
matchContentTypeSingular = "Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"

matchHeaderAbsent :: HeaderName -> MatchHeader
matchHeaderAbsent name = MatchHeader $ \headers _body ->
  case lookup name headers of
    Just _  -> Just $ "unexpected header: " <> toS (original name) <> "\n"
    Nothing -> Nothing

validateOpenApiResponse :: [Header] -> WaiSession () ()
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
  let Just body = decode (simpleBody r)
  Just schema <- liftIO $ decode <$> BL.readFile "test/fixtures/openapi.json"
  let args :: M.Map Text Value
      args = M.fromList
        [ ( "schema", schema )
        , ( "data", body ) ]
      hdrs = acceptHdrs "application/json"
  request methodPost "/rpc/validate_json_schema" hdrs (encode args)
      `shouldRespondWith` "true"
      { matchStatus = 200
      , matchHeaders = []
      }


getEnvVarWithDefault :: Text -> Text -> IO Text
getEnvVarWithDefault var def = toS <$>
  getEnv (toS var) `E.catchIOError` const (return $ toS def)

_baseCfg :: AppConfig
_baseCfg = let secret = Just $ encodeUtf8 "reallyreallyreallyreallyverysafe" in
  AppConfig {
    configAppSettings           = [ ("app.settings.app_host", "localhost") , ("app.settings.external_api_secret", "0123456789abcdef") ]
  , configDbAnonRole            = "postgrest_test_anonymous"
  , configDbChannel             = mempty
  , configDbChannelEnabled      = False
  , configDbExtraSearchPath     = []
  , configDbMaxRows             = Nothing
  , configDbPoolSize            = 10
  , configDbPoolTimeout         = 10
  , configDbPreRequest          = Just "test.switch_role"
  , configDbPreparedStatements  = True
  , configDbRootSpec            = Nothing
  , configDbSchemas             = fromList ["test"]
  , configDbConfig              = False
  , configDbUri                 = mempty
  , configJWKS                  = parseSecret <$> secret
  , configJwtAudience           = Nothing
  , configJwtRoleClaimKey       = [JSPKey "role"]
  , configJwtSecret             = secret
  , configJwtSecretIsBase64     = False
  , configLogLevel              = LogCrit
  , configOpenApiServerProxyUri = Nothing
  , configRawMediaTypes         = []
  , configServerHost            = "localhost"
  , configServerPort            = 3000
  , configServerUnixSocket      = Nothing
  , configServerUnixSocketMode  = 432
  , configDbTxAllowOverride     = True
  , configDbTxRollbackAll       = True
  }

testCfg :: Text -> AppConfig
testCfg testDbConn = _baseCfg { configDbUri = testDbConn }

testCfgDisallowRollback :: Text -> AppConfig
testCfgDisallowRollback testDbConn = (testCfg testDbConn) { configDbTxAllowOverride = False, configDbTxRollbackAll = False }

testCfgForceRollback :: Text -> AppConfig
testCfgForceRollback testDbConn = (testCfg testDbConn) { configDbTxAllowOverride = False, configDbTxRollbackAll = True }

testCfgNoJWT :: Text -> AppConfig
testCfgNoJWT testDbConn = (testCfg testDbConn) { configJwtSecret = Nothing, configJWKS = Nothing }

testUnicodeCfg :: Text -> AppConfig
testUnicodeCfg testDbConn = (testCfg testDbConn) { configDbSchemas = fromList ["تست"] }

testMaxRowsCfg :: Text -> AppConfig
testMaxRowsCfg testDbConn = (testCfg testDbConn) { configDbMaxRows = Just 2 }

testProxyCfg :: Text -> AppConfig
testProxyCfg testDbConn = (testCfg testDbConn) { configOpenApiServerProxyUri = Just "https://postgrest.com/openapi.json" }

testCfgBinaryJWT :: Text -> AppConfig
testCfgBinaryJWT testDbConn =
  let secret = Just . B64.decodeLenient $ "cmVhbGx5cmVhbGx5cmVhbGx5cmVhbGx5dmVyeXNhZmU=" in
  (testCfg testDbConn) {
    configJwtSecret = secret
  , configJWKS = parseSecret <$> secret
  }

testCfgAudienceJWT :: Text -> AppConfig
testCfgAudienceJWT testDbConn =
  let secret = Just . B64.decodeLenient $ "cmVhbGx5cmVhbGx5cmVhbGx5cmVhbGx5dmVyeXNhZmU=" in
  (testCfg testDbConn) {
    configJwtSecret = secret
  , configJwtAudience = Just "youraudience"
  , configJWKS = parseSecret <$> secret
  }

testCfgAsymJWK :: Text -> AppConfig
testCfgAsymJWK testDbConn =
  let secret = Just $ encodeUtf8 [str|{"alg":"RS256","e":"AQAB","key_ops":["verify"],"kty":"RSA","n":"0etQ2Tg187jb04MWfpuogYGV75IFrQQBxQaGH75eq_FpbkyoLcEpRUEWSbECP2eeFya2yZ9vIO5ScD-lPmovePk4Aa4SzZ8jdjhmAbNykleRPCxMg0481kz6PQhnHRUv3nF5WP479CnObJKqTVdEagVL66oxnX9VhZG9IZA7k0Th5PfKQwrKGyUeTGczpOjaPqbxlunP73j9AfnAt4XCS8epa-n3WGz1j-wfpr_ys57Aq-zBCfqP67UYzNpeI1AoXsJhD9xSDOzvJgFRvc3vm2wjAW4LEMwi48rCplamOpZToIHEPIaPzpveYQwDnB1HFTR1ove9bpKJsHmi-e2uzQ","use":"sig"}|]
  in (testCfg testDbConn) {
    configJwtSecret = secret
  , configJWKS = parseSecret <$> secret
  }

testCfgAsymJWKSet :: Text -> AppConfig
testCfgAsymJWKSet testDbConn =
  let secret = Just $ encodeUtf8 [str|{"keys": [{"alg":"RS256","e":"AQAB","key_ops":["verify"],"kty":"RSA","n":"0etQ2Tg187jb04MWfpuogYGV75IFrQQBxQaGH75eq_FpbkyoLcEpRUEWSbECP2eeFya2yZ9vIO5ScD-lPmovePk4Aa4SzZ8jdjhmAbNykleRPCxMg0481kz6PQhnHRUv3nF5WP479CnObJKqTVdEagVL66oxnX9VhZG9IZA7k0Th5PfKQwrKGyUeTGczpOjaPqbxlunP73j9AfnAt4XCS8epa-n3WGz1j-wfpr_ys57Aq-zBCfqP67UYzNpeI1AoXsJhD9xSDOzvJgFRvc3vm2wjAW4LEMwi48rCplamOpZToIHEPIaPzpveYQwDnB1HFTR1ove9bpKJsHmi-e2uzQ","use":"sig"}]}|]
  in (testCfg testDbConn) {
    configJwtSecret = secret
  , configJWKS = parseSecret <$> secret
  }

testNonexistentSchemaCfg :: Text -> AppConfig
testNonexistentSchemaCfg testDbConn = (testCfg testDbConn) { configDbSchemas = fromList ["nonexistent"] }

testCfgExtraSearchPath :: Text -> AppConfig
testCfgExtraSearchPath testDbConn = (testCfg testDbConn) { configDbExtraSearchPath = ["public", "extensions"] }

testCfgRootSpec :: Text -> AppConfig
testCfgRootSpec testDbConn = (testCfg testDbConn) { configDbRootSpec = Just "root"}

testCfgHtmlRawOutput :: Text -> AppConfig
testCfgHtmlRawOutput testDbConn = (testCfg testDbConn) { configRawMediaTypes = ["text/html"] }

testCfgResponseHeaders :: Text -> AppConfig
testCfgResponseHeaders testDbConn = (testCfg testDbConn) { configDbPreRequest = Just "custom_headers" }

testMultipleSchemaCfg :: Text -> AppConfig
testMultipleSchemaCfg testDbConn = (testCfg testDbConn) { configDbSchemas = fromList ["v1", "v2"] }

resetDb :: Text -> IO ()
resetDb dbConn = loadFixture dbConn "data"

analyzeTable :: Text -> Text -> IO ()
analyzeTable dbConn tableName =
  void $ readProcess "psql" ["--set", "ON_ERROR_STOP=1", toS dbConn, "-a", "-c", toS $ "ANALYZE test.\"" <> tableName <> "\""] []

loadFixture :: Text -> FilePath -> IO()
loadFixture dbConn name =
  void $ readProcess "psql" ["--set", "ON_ERROR_STOP=1", toS dbConn, "-q", "-f", "test/fixtures/" ++ name ++ ".sql"] []

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

noBlankHeader :: [Header] -> Bool
noBlankHeader = notElem mempty

noProfileHeader :: [Header] -> Bool
noProfileHeader headers = isNothing $ find ((== "Content-Profile") . fst) headers

authHeader :: BS.ByteString -> BS.ByteString -> Header
authHeader typ creds =
  (hAuthorization, typ <> " " <> creds)

authHeaderBasic :: BS.ByteString -> BS.ByteString -> Header
authHeaderBasic u p =
  authHeader "Basic" $ toS . B64.encode . toS $ u <> ":" <> p

authHeaderJWT :: BS.ByteString -> Header
authHeaderJWT = authHeader "Bearer"

-- | Tests whether the text can be parsed as a json object containing
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
