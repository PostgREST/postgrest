module SpecHelper where

import Network.Wai
import Test.Hspec
import Test.Hspec.Wai

import Hasql as H
import Hasql.Backend as B
import Hasql.Postgres as P

import Data.String.Conversions (cs)
import Data.Monoid
import Data.Text hiding (map)
import qualified Data.Vector as V
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (void)

import Network.HTTP.Types.Header (Header, ByteRange, renderByteRange,
                                  hRange, hAuthorization, hAccept)
import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA ((=~))
import qualified Data.ByteString.Char8 as BS
import System.Process (readProcess)
import Web.JWT (secret)

import PostgREST.App (app)
import PostgREST.Config (AppConfig(..))
import PostgREST.Middleware
import PostgREST.Error(pgErrResponse)
import PostgREST.DbStructure
import PostgREST.Types

dbString :: String
dbString = "postgres://postgrest_test_authenticator@localhost:5432/postgrest_test"

cfg :: String -> [SqlFragment] -> Maybe Int -> AppConfig
cfg conStr schema = AppConfig conStr 3000 "postgrest_test_anonymous" schema (secret "safe") 10

cfgDefault :: AppConfig
cfgDefault = cfg dbString ["test"] Nothing

cfgLimitRows :: Int -> AppConfig
cfgLimitRows = cfg dbString ["test"] . Just

cfgTestV2Schema :: AppConfig
cfgTestV2Schema = cfg dbString ["test_v2", "test"] Nothing

testPoolOpts :: PoolSettings
testPoolOpts = fromMaybe (error "bad settings") $ H.poolSettings 1 30

pgSettings :: P.Settings
pgSettings = P.StringSettings $ cs dbString

specDbPool :: IO (H.Pool P.Postgres)
specDbPool = H.acquirePool pgSettings testPoolOpts

specDbStructure :: H.Pool P.Postgres -> Schema -> IO DbStructure
specDbStructure pool schema = do
  dbOrError <- H.session pool $ H.tx specTxSettings
    $ getDbStructure schema
  either (fail . show) return dbOrError

withApp :: AppConfig -> DbStructure -> H.Pool P.Postgres
        -> ActionWith Application -> IO ()
withApp config dbStructure pool perform = do
  perform $ middle $ \req resp -> do
    time <- getPOSIXTime
    body <- strictRequestBody req
    result <- liftIO $ H.session pool $ H.tx specTxSettings $
      ( setSchemaSearchPath (configSchema config)
        . runWithClaims config time)
      (app dbStructure config body) req
    either (resp . pgErrResponse) resp result

  where middle = defaultMiddle

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

testPool :: IO(H.Pool P.Postgres)
testPool = H.acquirePool pgSettings testPoolOpts

clearTable :: Text -> IO ()
clearTable table = do
  pool <- testPool
  void . liftIO $ H.session pool $ H.tx Nothing $
    H.unitEx $ B.Stmt ("truncate table test." <> table <> " cascade") V.empty True

specTxSettings :: Maybe (TxIsolationLevel, Maybe Bool)
specTxSettings = Just (H.ReadCommitted, Just True)
