{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module SpecHelper where

import Network.Wai
import Test.Hspec
import Test.Hspec.Wai

import Hasql as H
import Hasql.Postgres as H

import Data.String.Conversions (cs)
-- import Control.Exception.Base (bracket, finally)
import Control.Monad (void)
import Control.Exception

import Network.HTTP.Types.Header (Header, ByteRange, renderByteRange,
                                  hRange, hAuthorization)
import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA ((=~))
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Middleware.Cors (cors)
import System.Process (readProcess)

import App (app, sqlError, isSqlError)
import Config (AppConfig(..), corsPolicy)
import Middleware
-- import Auth (addUser)

isLeft :: Either a b -> Bool
isLeft (Left _ ) = True
isLeft _ = False

cfg :: AppConfig
cfg = AppConfig "postgrest_test" 5432 "postgrest_test" "" "localhost" 3000 "postgrest_anonymous" False 10

testSettings :: SessionSettings
testSettings = fromMaybe (error "bad settings") $ H.sessionSettings 1 30

pgSettings :: Postgres
pgSettings = H.ParamSettings "localhost" 5432 "postgrest_test" "" "postgrest_test"

withApp :: ActionWith Application -> IO ()
withApp perform =
  let anonRole = cs $ configAnonRole cfg in
  perform $ middle $ \req resp ->
    H.session pgSettings testSettings $ H.sessionUnlifter >>= \unlift ->
      liftIO $ do
        body <- strictRequestBody req
        resp =<< catchJust isSqlError
          (unlift $ H.tx Nothing
                  $ authenticated anonRole (app body) req)
          (return . sqlError)

  where middle = cors corsPolicy


resetDb :: IO ()
resetDb = do
  H.session pgSettings testSettings $
    H.tx Nothing $ do
      H.unit [H.q| drop schema if exists "1" cascade |]
      H.unit [H.q| drop schema if exists private cascade |]
      H.unit [H.q| drop schema if exists postgrest cascade |]

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

-- for hspec-wai
pending_ :: WaiSession ()
pending_ = liftIO Test.Hspec.pending

-- for hspec-wai
pendingWith_ :: String -> WaiSession ()
pendingWith_ = liftIO . Test.Hspec.pendingWith
