module SpecHelper where

import Network.Wai
import Test.Hspec

import Data.String.Conversions (cs)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (void)

import Network.HTTP.Types.Header (Header, ByteRange, renderByteRange,
                                  hRange, hAuthorization, hAccept)
import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Text.Regex.TDFA ((=~))
import qualified Data.ByteString.Char8 as BS
import System.Process (readProcess)
import Web.JWT (secret)

import qualified Hasql.Connection  as H
import qualified Hasql.Session     as H

import PostgREST.App (app)
import PostgREST.Config (AppConfig(..))
import PostgREST.Middleware
import PostgREST.Error(pgErrResponse)
import PostgREST.Types
import PostgREST.QueryBuilder (inTransaction, Isolation(..))

dbString :: String
dbString = "postgres://postgrest_test_authenticator@localhost:5432/postgrest_test"

cfg :: String -> Maybe Integer -> AppConfig
cfg conStr = AppConfig conStr "postgrest_test_anonymous" "test" 3000 (secret "safe") 10

cfgDefault :: AppConfig
cfgDefault = cfg dbString Nothing

cfgLimitRows :: Integer -> AppConfig
cfgLimitRows = cfg dbString . Just

withApp :: AppConfig -> DbStructure -> H.Connection
        -> ActionWith Application -> IO ()
withApp config dbStructure c perform = do
  mDbStructure <- newMVar dbStructure
  perform $ defaultMiddle $ \req resp -> do
    time <- getPOSIXTime
    body <- strictRequestBody req
    let handleReq = H.run $ inTransaction ReadCommitted
          (runWithClaims config time (app mDbStructure config body) req)

    handleReq c >>= \case
      Left err -> do
        void $ H.run (H.sql "rollback;") c
        resp $ pgErrResponse err
      Right res -> resp res

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
