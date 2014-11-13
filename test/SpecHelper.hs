module SpecHelper where

import Network.Wai
import Test.Hspec
import Test.Hspec.Wai

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Data.String.Conversions (cs)
import Control.Exception.Base (bracket, finally)
import Control.Monad (void)

import Network.HTTP.Types.Header (Header, ByteRange, renderByteRange,
                                  hRange, hAuthorization)
import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Text.Regex.TDFA ((=~))
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Middleware.Cors (cors)

import Middleware(clientErrors, withSavepoint, authenticated, Environment(..))

import App (app)
import Config (corsPolicy, AppConfig(..))
import Auth (addUser)

isLeft :: Either a b -> Bool
isLeft (Left _ ) = True
isLeft _ = False

cfg :: AppConfig
cfg = AppConfig "postgres://dbapi_test:@localhost:5432/dbapi_test" 9000 "dbapi_anonymous" False 10

openConnection :: IO Connection
openConnection = connectPostgreSQL $ cs $ configDbUri cfg

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection close

loadFixture :: String -> Connection -> IO ()
loadFixture name conn = do
  sql <- readFile $ "test/fixtures/" ++ name ++ ".sql"
  void $ execute_ conn $ Query (cs sql)

dbWithSchema :: ActionWith Connection -> IO ()
dbWithSchema action = withDatabaseConnection $ \c -> do
  _ <- execute_ c "begin;"
  action c
  rollback c

withUser :: BS.ByteString -> BS.ByteString -> BS.ByteString ->
            ActionWith Connection -> ActionWith Connection
withUser name pass role action conn = do
  _ <- addUser conn name pass role
  finally (action conn) $ do
    _ <- execute conn "delete from dbapi.auth where id=?" $ Only name
    execute_ conn "commit"

withApp :: ActionWith Application -> ActionWith Connection
withApp action conn = do
  _ <- execute_ conn "begin;"
  action $ cors corsPolicy $ authenticated "dbapi_anonymous" app conn
  rollback conn

appWithFixture :: ActionWith Application -> IO ()
appWithFixture action = withDatabaseConnection $ \c -> do
  _ <- execute_ c "begin;"
  action $ cors corsPolicy . clientErrors  $
    (authenticated "dbapi_anonymous" . Middleware.withSavepoint Test) app c
  rollback c

rangeHdrs :: ByteRange -> [Header]
rangeHdrs r = [rangeUnit, (hRange, renderByteRange r)]

rangeUnit :: Header
rangeUnit = ("Range-Unit" :: CI BS.ByteString, "items")

matchHeader :: CI BS.ByteString -> String -> [Header] -> Bool
matchHeader name valRegex headers =
  maybe False (=~ valRegex) $ lookup name headers

authHeader :: String -> String -> Header
authHeader user pass =
  (hAuthorization, cs $ "Basic " ++ encode (user ++ ":" ++ pass))

-- for hspec-wai
pending_ :: WaiSession ()
pending_ = liftIO Test.Hspec.pending

-- for hspec-wai
pendingWith_ :: String -> WaiSession ()
pendingWith_ = liftIO . Test.Hspec.pendingWith
