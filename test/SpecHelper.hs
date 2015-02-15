module SpecHelper where

import Network.Wai
import Test.Hspec
import Test.Hspec.Wai

import Hasql as H
import Hasql.Backend as H
import Hasql.Postgres as H

import Data.String.Conversions (cs)
import Data.Monoid
import Data.Text hiding (map)
import qualified Data.Vector as V
import Control.Monad (void)

import Network.HTTP.Types.Header (Header, ByteRange, renderByteRange,
                                  hRange, hAuthorization)
import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA ((=~))
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Middleware.Cors (cors)
import System.Process (readProcess)

import App (app)
import Config (AppConfig(..), corsPolicy)
import Middleware
import Error(errResponse)
-- import Auth (addUser)

isLeft :: Either a b -> Bool
isLeft (Left _ ) = True
isLeft _ = False

cfg :: AppConfig
cfg = AppConfig "postgrest_test" 5432 "postgrest_test" "" "localhost" 3000 "postgrest_anonymous" False 10

testPoolOpts :: PoolSettings
testPoolOpts = fromMaybe (error "bad settings") $ H.poolSettings 1 30

pgSettings :: H.Settings
pgSettings = H.ParamSettings (cs $ configDbHost cfg)
                             (fromIntegral $ configDbPort cfg)
                             (cs $ configDbUser cfg)
                             (cs $ configDbPass cfg)
                             (cs $ configDbName cfg)

withApp :: ActionWith Application -> IO ()
withApp perform = do
  let anonRole = cs $ configAnonRole cfg
      currRole = cs $ configDbUser cfg
  pool :: H.Pool H.Postgres
    <- H.acquirePool pgSettings testPoolOpts

  perform $ middle $ \req resp -> do
    body <- strictRequestBody req
    result <- liftIO $ H.session pool $ H.tx Nothing
      $ authenticated currRole anonRole (app body) req
    either (resp . errResponse) resp result

  where middle = cors corsPolicy


resetDb :: IO ()
resetDb = do
  pool :: H.Pool H.Postgres
    <- H.acquirePool pgSettings testPoolOpts
  void . liftIO $ H.session pool $
    H.tx Nothing $ do
      H.unitEx [H.stmt| drop schema if exists "1" cascade |]
      H.unitEx [H.stmt| drop schema if exists private cascade |]
      H.unitEx [H.stmt| drop schema if exists postgrest cascade |]

  loadFixture "roles"
  loadFixture "schema"


loadFixture :: FilePath -> IO()
loadFixture name =
  void $ readProcess "psql" ["-U", "postgrest_test", "-d", "postgrest_test", "-a", "-f", "test/fixtures/" ++ name ++ ".sql"] []


rangeHdrs :: ByteRange -> [Header]
rangeHdrs r = [rangeUnit, (hRange, renderByteRange r)]

rangeUnit :: Header
rangeUnit = ("Range-Unit" :: CI BS.ByteString, "items")

matchHeader :: CI BS.ByteString -> String -> [Header] -> Bool
matchHeader name valRegex headers =
  maybe False (=~ valRegex) $ lookup name headers

authHeader :: String -> String -> Header
authHeader u p =
  (hAuthorization, cs $ "Basic " ++ encode (u ++ ":" ++ p))

clearTable :: Text -> IO ()
clearTable table = do
  pool :: H.Pool H.Postgres
    <- H.acquirePool pgSettings testPoolOpts
  void . liftIO $ H.session pool $ H.tx Nothing $
    H.unitEx $ H.Stmt ("delete from \"1\"."<>table) V.empty True

createItems :: Int -> IO ()
createItems n = do
  pool :: H.Pool H.Postgres
    <- H.acquirePool pgSettings testPoolOpts
  void . liftIO $ H.session pool $ H.tx Nothing txn
  where
    txn = sequence_ $ map H.unitEx stmts
    stmts = map [H.stmt|insert into "1".items (id) values (?)|] [1..n]

-- for hspec-wai
pending_ :: WaiSession ()
pending_ = liftIO Test.Hspec.pending

-- for hspec-wai
pendingWith_ :: String -> WaiSession ()
pendingWith_ = liftIO . Test.Hspec.pendingWith
