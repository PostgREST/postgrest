{-# LANGUAGE QuasiQuotes #-}

module SpecHelper where

import Network.Wai
import Test.Hspec
import Test.Hspec.Wai

import Hasql as H
import Hasql.Postgres as H

import Data.String.Conversions (cs)
-- import Control.Exception.Base (bracket, finally)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad (void)
import Control.Applicative ( (<$>) )
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

import App (app, sqlErrHandler, isSqlError)
import Config (corsPolicy)
-- import Auth (addUser)

isLeft :: Either a b -> Bool
isLeft (Left _ ) = True
isLeft _ = False

-- cfg :: AppConfig
-- cfg = AppConfig "postgres://dbapi_test:@localhost:5432/dbapi_test" 9000 "dbapi_anonymous" False 10

testSettings :: SessionSettings
testSettings = fromMaybe (error "bad settings") $ H.sessionSettings 1 30

pgSettings :: Postgres
pgSettings = H.Postgres "localhost" 5432 "dbapi_test" "" "dbapi_test"

withApp :: ActionWith Application -> IO ()
withApp perform =
  perform $ middle $ \req resp ->
    H.session pgSettings testSettings $ do
      session' <- flip runReaderT <$> ask
      liftIO $ resp =<< catchJust isSqlError (session' $ app req)
        sqlErrHandler

  where middle = cors corsPolicy


resetDb :: IO ()
resetDb = do
  H.session pgSettings testSettings $
    H.tx Nothing $ do
      H.unit [H.q| drop schema if exists "1" cascade |]
      H.unit [H.q| drop schema if exists private cascade |]
      H.unit [H.q| drop schema if exists dbapi cascade |]

  loadFixture "roles"
  loadFixture "schema"


loadFixture :: FilePath -> IO()
loadFixture name =
  void $ readProcess "psql" ["-U", "dbapi_test", "-d", "dbapi_test", "-a", "-f", "test/fixtures/" ++ name ++ ".sql"] []


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
