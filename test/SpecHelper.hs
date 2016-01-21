module SpecHelper where

import Network.Wai
import Test.Hspec

import Data.String.Conversions (cs)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (void)

import Network.HTTP.Types.Header (Header, ByteRange, renderByteRange,
                                  hRange, hAuthorization, hAccept)
import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Data.Pool
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

dbString :: String
dbString = "postgres://postgrest_test_authenticator@localhost:5432/postgrest_test"

cfg :: String -> Maybe Integer -> AppConfig
cfg conStr = AppConfig conStr 3000 "postgrest_test_anonymous" "test" (secret "safe") 10

cfgDefault :: AppConfig
cfgDefault = cfg dbString Nothing

cfgLimitRows :: Integer -> AppConfig
cfgLimitRows = cfg dbString . Just

withApp :: AppConfig -> DbStructure -> Pool H.Connection
        -> ActionWith Application -> IO ()
withApp config dbStructure pool perform = do
  perform $ defaultMiddle $ \req resp -> do
    time <- getPOSIXTime
    body <- strictRequestBody req
    let handleReq = H.run (runWithClaims config time (app dbStructure config body) req)

    withResource pool $ \c -> do
      resOrError <- handleReq c
      either (resp . pgErrResponse) resp resOrError

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

testPool :: IO (Pool (Either H.ConnectionError H.Connection))
testPool = createPool (H.acquire . cs $ dbString)
  (either (const $ return ()) H.release) 1 1 1
