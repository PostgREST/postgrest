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

import qualified Data.Aeson.Types as J

import PostgREST.App (app)
import PostgREST.Config (AppConfig(..), corsPolicy)
import PostgREST.Middleware
import PostgREST.Error(errResponse)

isLeft :: Either a b -> Bool
isLeft (Left _ ) = True
isLeft _ = False

cfg :: AppConfig
cfg = AppConfig "postgrest_test" 5432 "postgrest_test" "" "localhost" 3000 "postgrest_anonymous" False 10 "1" "safe"

testPoolOpts :: PoolSettings
testPoolOpts = fromMaybe (error "bad settings") $ H.poolSettings 1 30

pgSettings :: P.Settings
pgSettings = P.ParamSettings (cs $ configDbHost cfg)
                             (fromIntegral $ configDbPort cfg)
                             (cs $ configDbUser cfg)
                             (cs $ configDbPass cfg)
                             (cs $ configDbName cfg)

withApp :: ActionWith Application -> IO ()
withApp perform = do
  pool :: H.Pool P.Postgres
    <- H.acquirePool pgSettings testPoolOpts

  perform $ middle $ \req resp -> do
    body <- strictRequestBody req
    result <- liftIO $ H.session pool $ H.tx Nothing
      $ authenticated cfg (app cfg body) req
    either (resp . errResponse) resp result

  where middle = cors corsPolicy


resetDb :: IO ()
resetDb = do
  pool :: H.Pool P.Postgres
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
    H.unitEx $ B.Stmt ("delete from \"1\"."<>table) V.empty True

createItems :: Int -> IO ()
createItems n = do
  pool <- testPool
  void . liftIO $ H.session pool $ H.tx Nothing txn
  where
    txn = mapM_ H.unitEx stmts
    stmts = map [H.stmt|insert into "1".items (id) values (?)|] [1..n]

createNulls :: Int -> IO ()
createNulls n = do
  pool <- testPool
  void . liftIO $ H.session pool $ H.tx Nothing txn
  where
    txn = mapM_ H.unitEx (stmt':stmts)
    stmt' = [H.stmt|insert into "1".no_pk (a,b) values (null,null)|]
    stmts = map [H.stmt|insert into "1".no_pk (a,b) values (?,0)|] [1..n]

createLikableStrings :: IO ()
createLikableStrings = do
  pool <- testPool
  void . liftIO $ H.session pool $ H.tx Nothing $ do
    H.unitEx $ insertSimplePk "xyyx" "u"
    H.unitEx $ insertSimplePk "xYYx" "v"
  where
    insertSimplePk :: Text -> Text -> H.Stmt P.Postgres
    insertSimplePk = [H.stmt|insert into "1".simple_pk (k, extra) values (?,?)|]

createJsonData :: IO ()
createJsonData = do
  pool <- testPool
  void . liftIO $ H.session pool $ H.tx Nothing $
    H.unitEx $
      [H.stmt|
        insert into "1".json (data) values (?)
      |]
      (J.object [("foo", J.object [("bar", J.String "baz")])])
