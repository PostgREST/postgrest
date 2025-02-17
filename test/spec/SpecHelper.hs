{-# LANGUAGE FlexibleContexts #-}
module SpecHelper where

import           Control.Lens           ((^?))
import qualified Data.Aeson             as JSON
import           Data.Aeson.Lens
import qualified Data.ByteString.Base64 as B64 (decodeLenient)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map.Strict        as M
import           Data.Scientific        (toRealFloat)
import qualified Data.Set               as S

import Data.Aeson           ((.=))
import Data.CaseInsensitive (CI (..), mk, original)
import Data.List            (lookup)
import Data.List.NonEmpty   (fromList)
import Network.Wai.Test     (SResponse (simpleBody, simpleHeaders, simpleStatus))
import System.IO.Unsafe     (unsafePerformIO)
import System.Process       (readProcess)
import Text.Regex.TDFA      ((=~))


import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Text.Heredoc

import Data.String                       (String)
import PostgREST.Config                  (AppConfig (..),
                                          JSPathExp (..),
                                          LogLevel (..),
                                          LogQuery (..),
                                          OpenAPIMode (..),
                                          parseSecret)
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..))
import Protolude                         hiding (get, toS)
import Protolude.Conv                    (toS)

filterAndMatchCT :: BS.ByteString -> MatchHeader
filterAndMatchCT val = MatchHeader $ \headers _ ->
        case filter (\(n,_) -> n == hContentType) headers of
          [(_,v)] -> if v == val
                     then Nothing
                     else Just $ "missing value:" <> toS val <> "\n"
          _   -> Just "unexpected header: zero or multiple headers present\n"

matchContentTypeJson :: MatchHeader
matchContentTypeJson =
  filterAndMatchCT "application/json; charset=utf-8"

matchContentTypeSingular :: MatchHeader
matchContentTypeSingular =
  filterAndMatchCT "application/vnd.pgrst.object+json; charset=utf-8"

matchCTArrayStrip :: MatchHeader
matchCTArrayStrip =
  filterAndMatchCT "application/vnd.pgrst.array+json;nulls=stripped; charset=utf-8"

matchCTSingularStrip :: MatchHeader
matchCTSingularStrip =
  filterAndMatchCT "application/vnd.pgrst.object+json;nulls=stripped; charset=utf-8"

matchHeaderValuePresent :: HeaderName -> BS.ByteString -> MatchHeader
matchHeaderValuePresent name val = MatchHeader $ \headers _ ->
  case lookup name headers of
    Just hdr -> if val `BS.isInfixOf` hdr then Nothing else Just $ "missing header value: " <> toS val <> "\n"
    Nothing  -> Just $ "missing header: " <> toS (original name) <> "\n"

matchHeaderAbsent :: HeaderName -> MatchHeader
matchHeaderAbsent name = MatchHeader $ \headers _body ->
  case lookup name headers of
    Just _  -> Just $ "unexpected header: " <> toS (original name) <> "\n"
    Nothing -> Nothing

-- | Matches Server-Timing header has a well-formed metric with the given name
matchServerTimingHasTiming :: String -> MatchHeader
matchServerTimingHasTiming metric = MatchHeader $ \headers _body ->
  case lookup "Server-Timing" headers of
    Just hdr -> if hdr =~ (metric <> ";dur=[[:digit:]]+.[[:digit:]]+")
                  then Nothing
                  else Just $ "missing metric: " <> metric <> "\n"
    Nothing  -> Just "missing Server-Timing header\n"

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
  Just body <- pure $ JSON.decode (simpleBody r)
  Just schema <- liftIO $ JSON.decode <$> BL.readFile "test/spec/fixtures/openapi.json"
  let args :: M.Map Text JSON.Value
      args = M.fromList
        [ ( "schema", schema )
        , ( "data", body ) ]
      hdrs = acceptHdrs "application/json"
  request methodPost "/rpc/validate_json_schema" hdrs (JSON.encode args)
      `shouldRespondWith` "true"
      { matchStatus = 200
      , matchHeaders = []
      }


baseCfg :: AppConfig
baseCfg = let secret = encodeUtf8 "reallyreallyreallyreallyverysafe" in
  AppConfig {
    configAppSettings               = [ ("app.settings.app_host", "localhost") , ("app.settings.external_api_secret", "0123456789abcdef") ]
  , configDbAggregates              = False
  , configDbAnonRole                = Just "postgrest_test_anonymous"
  , configDbChannel                 = mempty
  , configDbChannelEnabled          = True
  , configDbExtraSearchPath         = []
  , configDbHoistedTxSettings       = ["default_transaction_isolation","plan_filter.statement_cost_limit","statement_timeout"]
  , configDbMaxRows                 = Nothing
  , configDbPlanEnabled             = False
  , configDbPoolSize                = 10
  , configDbPoolAcquisitionTimeout  = 10
  , configDbPoolMaxLifetime         = 1800
  , configDbPoolMaxIdletime         = 600
  , configDbPoolAutomaticRecovery   = True
  , configDbPreRequest              = Just $ QualifiedIdentifier "test" "switch_role"
  , configDbPreparedStatements      = True
  , configDbRootSpec                = Nothing
  , configDbSchemas                 = fromList ["test"]
  , configDbConfig                  = False
  , configDbPreConfig               = Nothing
  , configDbUri                     = "postgresql://"
  , configFilePath                  = Nothing
  , configJWKS                      = rightToMaybe $ parseSecret secret
  , configJwtAudience               = Nothing
  , configJwtRoleClaimKey           = [JSPKey "role"]
  , configJwtSecret                 = Just secret
  , configJwtSecretIsBase64         = False
  , configJwtCacheMaxLifetime       = 0
  , configLogLevel                  = LogCrit
  , configLogQuery                  = LogQueryDisabled
  , configOpenApiMode               = OAFollowPriv
  , configOpenApiSecurityActive     = False
  , configOpenApiServerProxyUri     = Nothing
  , configServerCorsAllowedOrigins  = Nothing
  , configServerHost                = "localhost"
  , configServerPort                = 3000
  , configServerTraceHeader         = Nothing
  , configServerUnixSocket          = Nothing
  , configServerUnixSocketMode      = 432
  , configDbTxAllowOverride         = True
  , configDbTxRollbackAll           = True
  , configAdminServerHost           = "localhost"
  , configAdminServerPort           = Nothing
  , configRoleSettings              = mempty
  , configRoleIsoLvl                = mempty
  , configInternalSCSleep           = Nothing
  , configServerTimingEnabled       = True
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
  let secret = B64.decodeLenient "cmVhbGx5cmVhbGx5cmVhbGx5cmVhbGx5dmVyeXNhZmU=" in
  baseCfg {
    configJwtSecret = Just secret
  , configJWKS = rightToMaybe $ parseSecret secret
  }

testCfgAudienceJWT :: AppConfig
testCfgAudienceJWT =
  let secret = B64.decodeLenient "cmVhbGx5cmVhbGx5cmVhbGx5cmVhbGx5dmVyeXNhZmU=" in
  baseCfg {
    configJwtSecret = Just secret
  , configJwtAudience = Just "youraudience"
  , configJWKS = rightToMaybe $ parseSecret secret
  }

testCfgAsymJWK :: AppConfig
testCfgAsymJWK =
  let secret = encodeUtf8 [str|{"alg":"RS256","e":"AQAB","key_ops":["verify"],"kty":"RSA","n":"0etQ2Tg187jb04MWfpuogYGV75IFrQQBxQaGH75eq_FpbkyoLcEpRUEWSbECP2eeFya2yZ9vIO5ScD-lPmovePk4Aa4SzZ8jdjhmAbNykleRPCxMg0481kz6PQhnHRUv3nF5WP479CnObJKqTVdEagVL66oxnX9VhZG9IZA7k0Th5PfKQwrKGyUeTGczpOjaPqbxlunP73j9AfnAt4XCS8epa-n3WGz1j-wfpr_ys57Aq-zBCfqP67UYzNpeI1AoXsJhD9xSDOzvJgFRvc3vm2wjAW4LEMwi48rCplamOpZToIHEPIaPzpveYQwDnB1HFTR1ove9bpKJsHmi-e2uzQ","use":"sig"}|]
  in baseCfg {
    configJwtSecret = Just secret
  , configJWKS = rightToMaybe $ parseSecret secret
  }

testCfgAsymJWKSet :: AppConfig
testCfgAsymJWKSet =
  let secret = encodeUtf8 [str|{"keys": [{"alg":"RS256","e":"AQAB","key_ops":["verify"],"kty":"RSA","n":"0etQ2Tg187jb04MWfpuogYGV75IFrQQBxQaGH75eq_FpbkyoLcEpRUEWSbECP2eeFya2yZ9vIO5ScD-lPmovePk4Aa4SzZ8jdjhmAbNykleRPCxMg0481kz6PQhnHRUv3nF5WP479CnObJKqTVdEagVL66oxnX9VhZG9IZA7k0Th5PfKQwrKGyUeTGczpOjaPqbxlunP73j9AfnAt4XCS8epa-n3WGz1j-wfpr_ys57Aq-zBCfqP67UYzNpeI1AoXsJhD9xSDOzvJgFRvc3vm2wjAW4LEMwi48rCplamOpZToIHEPIaPzpveYQwDnB1HFTR1ove9bpKJsHmi-e2uzQ","use":"sig"}]}|]
  in baseCfg {
    configJwtSecret = Just secret
  , configJWKS = rightToMaybe $ parseSecret secret
  }

testCfgExtraSearchPath :: AppConfig
testCfgExtraSearchPath = baseCfg { configDbExtraSearchPath = ["public", "extensions", "EXTRA \"@/\\#~_-"] }

testCfgRootSpec :: AppConfig
testCfgRootSpec = baseCfg { configDbRootSpec = Just $ QualifiedIdentifier mempty "root"}

testCfgResponseHeaders :: AppConfig
testCfgResponseHeaders = baseCfg { configDbPreRequest = Just $ QualifiedIdentifier mempty "custom_headers" }

testMultipleSchemaCfg :: AppConfig
testMultipleSchemaCfg = baseCfg { configDbSchemas = fromList ["v1", "v2", "SPECIAL \"@/\\#~_-"] }

testPgSafeUpdateEnabledCfg :: AppConfig
testPgSafeUpdateEnabledCfg = baseCfg { configDbPreRequest = Just $ QualifiedIdentifier "test" "load_safeupdate" }

testObservabilityCfg :: AppConfig
testObservabilityCfg = baseCfg { configServerTraceHeader = Just $ mk "X-Request-Id" }

testCfgServerTiming :: AppConfig
testCfgServerTiming = baseCfg { configDbPlanEnabled = True }

testCfgAggregatesEnabled :: AppConfig
testCfgAggregatesEnabled = baseCfg { configDbAggregates = True }

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
  obj = JSON.decode s :: Maybe (M.Map Text JSON.Value)
  keys = maybe S.empty M.keysSet obj
  validKeys = S.fromList ["message", "details", "hint", "code"]

planCost :: SResponse -> Float
planCost resp =
  let res = simpleBody resp ^? nth 0 . key "Plan" . key "Total Cost" in
  -- big value in case parsing fails
  fromMaybe 1000000000.0 $ unbox =<< res
  where
    unbox :: JSON.Value -> Maybe Float
    unbox (JSON.Number n) = Just $ toRealFloat n
    unbox _               = Nothing

data TiobePlsRow = TiobePlsRow {
  name' :: Text,
  rank  :: Int
} deriving (Show)

instance JSON.ToJSON TiobePlsRow where
  toJSON (TiobePlsRow name'' rank') = JSON.object ["name" .= name'', "rank" .= rank']

getInsertDataForTiobePlsTable :: Int -> BL.ByteString
getInsertDataForTiobePlsTable rows =
  JSON.encode $ fromList $ [TiobePlsRow {name' = nm, rank = rk} | (nm,rk) <- nameRankList]
   where
     nameRankList = [("Lang " <> show i, i) | i <- [20..(rows+20)] ] :: [(Text, Int)]

readFixtureFile :: FilePath -> BL.ByteString
readFixtureFile file = unsafePerformIO $ BL.readFile $ "test/spec/fixtures/" <> file
