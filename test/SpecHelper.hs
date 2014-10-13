{-# LANGUAGE OverloadedStrings #-}
module SpecHelper where

import Network.Wai
import Test.Hspec
import Test.Hspec.Wai

import Database.HDBC
import Database.HDBC.PostgreSQL

import Data.String.Conversions (cs)
import Control.Exception.Base (bracket, finally, tryJust)
import Control.Monad (when)

import Network.HTTP.Types.Header (Header, ByteRange, renderByteRange,
                                  hRange, hAuthorization)
import Codec.Binary.Base64.String (encode)
import Data.CaseInsensitive (CI(..))
import Text.Regex.TDFA ((=~))
import qualified Data.HashMap.Strict as Hash
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Middleware.Cors (cors)

import Dbapi (app, corsPolicy, AppConfig(..))
import PgQuery(addUser)

isLeft :: Either a b -> Bool
isLeft (Left _ ) = True
isLeft _ = False

cfg :: AppConfig
cfg = AppConfig "postgres://dbapi_test:@localhost:5432/dbapi_test" 9000 "dbapi_anonymous" False

openConnection :: IO Connection
openConnection = connectPostgreSQL' $ configDbUri cfg

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection disconnect

loadFixture :: String -> Connection -> IO ()
loadFixture name conn = do
  sql <- readFile $ "test/fixtures/" ++ name ++ ".sql"
  runRaw conn sql

dbWithSchema :: ActionWith Connection -> IO ()
dbWithSchema action = withDatabaseConnection $ \c -> do
  runRaw c "begin;"
  action c
  rollback c

withUser :: BS.ByteString -> BS.ByteString -> BS.ByteString ->
            ActionWith Connection -> ActionWith Connection
withUser name pass role action conn = do
  addUser name pass role conn
  finally (action conn) $ do
    _ <- run conn "delete from dbapi.auth where id=?" [toSql name]
    runRaw conn "commit"

withApp :: ActionWith Application -> ActionWith Connection
withApp action conn = do
  runRaw conn "begin;"
  action $ cors corsPolicy $ app conn "dbapi_anonymous"
  rollback conn

appWithFixture :: ActionWith Application -> IO ()
appWithFixture action = withDatabaseConnection $ \c -> do
  result <- tryJust transactionAborted $ do
    runRaw c "begin;"
    action $ cors corsPolicy $ app c "dbapi_anonymous"
    rollback c

  when (isLeft result) $
    putStrLn "note: commands ignored after aborted transaction"

  where
    transactionAborted :: SqlError -> Maybe ()
    transactionAborted e =
      if seState e == "25P02" then Just () else Nothing

rangeHdrs :: ByteRange -> [Header]
rangeHdrs r = [rangeUnit, (hRange, renderByteRange r)]

rangeUnit :: Header
rangeUnit = ("Range-Unit" :: CI BS.ByteString, "items")

getHeader :: CI BS.ByteString -> [Header] -> Maybe BS.ByteString
getHeader name headers =
  Hash.lookup name $ Hash.fromList headers

matchHeader :: CI BS.ByteString -> String -> [Header] -> Bool
matchHeader name valRegex headers =
  maybe False (=~ valRegex) $ getHeader name headers

authHeader :: String -> String -> Header
authHeader user pass =
  (hAuthorization, cs $ "Basic: " ++ encode (user ++ ":" ++ pass))

-- for hspec-wai
pending_ :: WaiSession ()
pending_ = liftIO pending

-- for hspec-wai
pendingWith_ :: String -> WaiSession ()
pendingWith_ = liftIO . pendingWith
