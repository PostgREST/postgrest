module SpecHelper where

import           Control.Lens           ((^?))
import           Data.Aeson.Lens
import qualified Data.ByteString.Base64 as B64 (decodeLenient)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map.Strict        as M
import           Data.Scientific        (toRealFloat)
import qualified Data.Set               as S

import Data.Aeson           (Value (..), decode, encode)
import Data.CaseInsensitive (CI (..), mk, original)
import Data.List            (lookup)
import Data.List.NonEmpty   (fromList)
import Network.Wai.Test     (SResponse (simpleBody, simpleHeaders, simpleStatus))
import System.Process       (readProcess)
import Text.Regex.TDFA      ((=~))


import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Text.Heredoc

import PostgREST.Config                  (AppConfig (..),
                                          JSPathExp (..),
                                          LogLevel (..),
                                          OpenAPIMode (..),
                                          parseSecret)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..))
import Protolude                         hiding (get, toS)
import Protolude.Conv                    (toS)

matchContentTypeJson :: MatchHeader
matchContentTypeJson = "Content-Type" <:> "application/json; charset=utf-8"

matchContentTypeSingular :: MatchHeader
matchContentTypeSingular = "Content-Type" <:> "application/vnd.pgrst.object+json; charset=utf-8"

matchCTArrayStrip :: MatchHeader
matchCTArrayStrip = "Content-Type" <:> "application/vnd.pgrst.array+json;nulls=stripped; charset=utf-8"

matchCTSingularStrip :: MatchHeader
matchCTSingularStrip = "Content-Type" <:> "application/vnd.pgrst.object+json;nulls=stripped; charset=utf-8"

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
  Just body <- pure $ decode (simpleBody r)
  Just schema <- liftIO $ decode <$> BL.readFile "test/spec/fixtures/openapi.json"
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


baseCfg :: AppConfig
baseCfg = let secret = Just $ encodeUtf8 "reallyreallyreallyreallyverysafe" in
  AppConfig {
    configAppSettings           = [ ("app.settings.app_host", "localhost") , ("app.settings.external_api_secret", "0123456789abcdef") ]
  , configDbAnonRole            = Just "postgrest_test_anonymous"
  , configDbChannel             = mempty
  , configDbChannelEnabled      = True
  , configDbExtraSearchPath     = []
  , configDbMaxRows             = Nothing
  , configDbPlanEnabled         = False
  , configDbPoolSize            = 10
  , configDbPoolAcquisitionTimeout = 10
  , configDbPoolMaxLifetime     = 1800
  , configDbPoolMaxIdletime     = 600
  , configDbPreRequest          = Just $ QualifiedIdentifier "test" "switch_role"
  , configDbPreparedStatements  = True
  , configDbRootSpec            = Nothing
  , configDbSchemas             = fromList ["test"]
  , configDbConfig              = False
  , configDbPreConfig           = Nothing
  , configDbUri                 = "postgresql://"
  , configDbUseLegacyGucs       = True
  , configFilePath              = Nothing
  , configJWKS                  = parseSecret <$> secret
  , configJwtAudience           = Nothing
  , configJwtRoleClaimKey       = [JSPKey "role"]
  , configJwtSecret             = secret
  , configJwtSecretIsBase64     = False
  , configLogLevel              = LogCrit
  , configOpenApiMode           = OAFollowPriv
  , configOpenApiSecurityActive = False
  , configOpenApiServerProxyUri = Nothing
  , configRawMediaTypes         = []
  , configServerHost            = "localhost"
  , configServerPort            = 3000
  , configServerTraceHeader     = Nothing
  , configServerUnixSocket      = Nothing
  , configServerUnixSocketMode  = 432
  , configDbTxAllowOverride     = True
  , configDbTxRollbackAll       = True
  , configAdminServerPort       = Nothing
  , configRoleSettings          = mempty
  , configRoleIsoLvl            = mempty
  , configInternalSCSleep       = Nothing
  }

testCfg :: AppConfig
testCfg = baseCfg

testCfgDisallowRollback :: AppConfig
testCfgDisallowRollback = baseCfg { configDbTxAllowOverride = False, configDbTxRollbackAll = False }

testCfgForceRollback :: AppConfig
testCfgForceRollback = baseCfg { configDbTxAllowOverride = False, configDbTxRollbackAll = True }

testCfgNoAnon :: AppConfig
testCfgNoAnon = baseCfg { configDbAnonRole = Nothing }

testCfgNoJWT :: AppConfig
testCfgNoJWT = baseCfg { configJwtSecret = Nothing, configJWKS = Nothing }

testUnicodeCfg :: AppConfig
testUnicodeCfg = baseCfg { configDbSchemas = fromList ["تست"] }

testMaxRowsCfg :: AppConfig
testMaxRowsCfg = baseCfg { configDbMaxRows = Just 2 }

testDisabledOpenApiCfg :: AppConfig
testDisabledOpenApiCfg = baseCfg { configOpenApiMode = OADisabled }

testIgnorePrivOpenApiCfg :: AppConfig
testIgnorePrivOpenApiCfg = baseCfg { configOpenApiMode = OAIgnorePriv, configDbSchemas = fromList ["test", "v1"] }

testProxyCfg :: AppConfig
testProxyCfg = baseCfg { configOpenApiServerProxyUri = Just "https://postgrest.com/openapi.json" }

testSecurityOpenApiCfg :: AppConfig
testSecurityOpenApiCfg = baseCfg { configOpenApiSecurityActive = True }

testPlanEnabledCfg :: AppConfig
testPlanEnabledCfg = baseCfg { configDbPlanEnabled = True }

testCfgBinaryJWT :: AppConfig
testCfgBinaryJWT =
  let secret = Just . B64.decodeLenient $ "cmVhbGx5cmVhbGx5cmVhbGx5cmVhbGx5dmVyeXNhZmU=" in
  baseCfg {
    configJwtSecret = secret
  , configJWKS = parseSecret <$> secret
  }

testCfgAudienceJWT :: AppConfig
testCfgAudienceJWT =
  let secret = Just . B64.decodeLenient $ "cmVhbGx5cmVhbGx5cmVhbGx5cmVhbGx5dmVyeXNhZmU=" in
  baseCfg {
    configJwtSecret = secret
  , configJwtAudience = Just "youraudience"
  , configJWKS = parseSecret <$> secret
  }

testCfgAsymJWK :: AppConfig
testCfgAsymJWK =
  let secret = Just $ encodeUtf8 [str|{"alg":"RS256","e":"AQAB","key_ops":["verify"],"kty":"RSA","n":"0etQ2Tg187jb04MWfpuogYGV75IFrQQBxQaGH75eq_FpbkyoLcEpRUEWSbECP2eeFya2yZ9vIO5ScD-lPmovePk4Aa4SzZ8jdjhmAbNykleRPCxMg0481kz6PQhnHRUv3nF5WP479CnObJKqTVdEagVL66oxnX9VhZG9IZA7k0Th5PfKQwrKGyUeTGczpOjaPqbxlunP73j9AfnAt4XCS8epa-n3WGz1j-wfpr_ys57Aq-zBCfqP67UYzNpeI1AoXsJhD9xSDOzvJgFRvc3vm2wjAW4LEMwi48rCplamOpZToIHEPIaPzpveYQwDnB1HFTR1ove9bpKJsHmi-e2uzQ","use":"sig"}|]
  in baseCfg {
    configJwtSecret = secret
  , configJWKS = parseSecret <$> secret
  }

testCfgAsymJWKSet :: AppConfig
testCfgAsymJWKSet =
  let secret = Just $ encodeUtf8 [str|{"keys": [{"alg":"RS256","e":"AQAB","key_ops":["verify"],"kty":"RSA","n":"0etQ2Tg187jb04MWfpuogYGV75IFrQQBxQaGH75eq_FpbkyoLcEpRUEWSbECP2eeFya2yZ9vIO5ScD-lPmovePk4Aa4SzZ8jdjhmAbNykleRPCxMg0481kz6PQhnHRUv3nF5WP479CnObJKqTVdEagVL66oxnX9VhZG9IZA7k0Th5PfKQwrKGyUeTGczpOjaPqbxlunP73j9AfnAt4XCS8epa-n3WGz1j-wfpr_ys57Aq-zBCfqP67UYzNpeI1AoXsJhD9xSDOzvJgFRvc3vm2wjAW4LEMwi48rCplamOpZToIHEPIaPzpveYQwDnB1HFTR1ove9bpKJsHmi-e2uzQ","use":"sig"}]}|]
  in baseCfg {
    configJwtSecret = secret
  , configJWKS = parseSecret <$> secret
  }

testNonexistentSchemaCfg :: AppConfig
testNonexistentSchemaCfg = baseCfg { configDbSchemas = fromList ["nonexistent"] }

testCfgExtraSearchPath :: AppConfig
testCfgExtraSearchPath = baseCfg { configDbExtraSearchPath = ["public", "extensions", "EXTRA \"@/\\#~_-"] }

testCfgRootSpec :: AppConfig
testCfgRootSpec = baseCfg { configDbRootSpec = Just $ QualifiedIdentifier mempty "root"}

testCfgHtmlRawOutput :: AppConfig
testCfgHtmlRawOutput = baseCfg { configRawMediaTypes = [MTOther "text/html"] }

testCfgResponseHeaders :: AppConfig
testCfgResponseHeaders = baseCfg { configDbPreRequest = Just $ QualifiedIdentifier mempty "custom_headers" }

testMultipleSchemaCfg :: AppConfig
testMultipleSchemaCfg = baseCfg { configDbSchemas = fromList ["v1", "v2", "SPECIAL \"@/\\#~_-"] }

testCfgLegacyGucs :: AppConfig
testCfgLegacyGucs = baseCfg { configDbUseLegacyGucs = False }

testPgSafeUpdateEnabledCfg :: AppConfig
testPgSafeUpdateEnabledCfg = baseCfg { configDbPreRequest = Just $ QualifiedIdentifier "test" "load_safeupdate" }

testObservabilityCfg :: AppConfig
testObservabilityCfg = baseCfg { configServerTraceHeader = Just $ mk "X-Request-Id" }

analyzeTable :: Text -> IO ()
analyzeTable tableName =
  void $ readProcess "psql" ["-U", "postgres", "--set", "ON_ERROR_STOP=1", "-a", "-c", toS $ "ANALYZE test.\"" <> tableName <> "\""] []

rangeHdrs :: ByteRange -> [Header]
rangeHdrs r = [rangeUnit, (hRange, renderByteRange r)]

rangeHdrsWithCount :: ByteRange -> [Header]
rangeHdrsWithCount r = ("Prefer", "count=exact") : rangeHdrs r

acceptHdrs :: BS.ByteString -> [Header]
acceptHdrs mime = [(hAccept, mime)]

planHdr :: Header
planHdr = (hAccept, "application/vnd.pgrst.plan+json")

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

-- | Follows these steps to verify if the table data changed in the db:
--  * Verifies the table data in the db before the change
--  * Does the mutation
--  * Verifies that the table data changed in the db
--  * Resets the table with the original data
shouldMutateInto :: MutationCheck -> ResponseMatcher -> WaiExpectation ()
shouldMutateInto (MutationCheck (BaseTable tblName tblOrd dataBefore) mutation) dataAfter = do
  get ("/" <> tblName) `shouldRespondWith` [json|#{dataBefore}|]
  mutation
  get ("/" <> tblName <> "?order=" <> tblOrd) `shouldRespondWith` dataAfter
  request methodPost "/rpc/reset_table"
    [("Prefer", "tx=commit")]
    [json| {"tbl_name": #{decodeUtf8 tblName}, "tbl_data": #{dataBefore}} |]
  `shouldRespondWith` 204

-- | How the base table data will change using the requested mutation
mutatesWith :: BaseTable -> WaiExpectation () -> MutationCheck
mutatesWith = MutationCheck

-- | The original table data before it is modified.
-- The column order is needed for an accurate comparison after the mutation
baseTable :: ByteString -> ByteString -> Value -> BaseTable
baseTable = BaseTable

-- | The mutation (update/delete) that will be applied to the base table
requestMutation :: Method -> ByteString -> [Header] -> BL.ByteString -> WaiExpectation ()
requestMutation method path headers body =
  request method path (("Prefer", "tx=commit") : headers) body `shouldRespondWith` 204

data BaseTable = BaseTable ByteString ByteString Value
data MutationCheck = MutationCheck BaseTable (WaiExpectation ())

planCost :: SResponse -> Float
planCost resp =
  let res = simpleBody resp ^? nth 0 . key "Plan" . key "Total Cost" in
  -- big value in case parsing fails
  fromMaybe 1000000000.0 $ unbox =<< res
  where
    unbox :: Value -> Maybe Float
    unbox (Number n) = Just $ toRealFloat n
    unbox _          = Nothing
