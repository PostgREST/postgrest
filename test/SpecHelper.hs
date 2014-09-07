{-# LANGUAGE OverloadedStrings #-}
module SpecHelper where

import Network.Wai
import Test.Hspec

import Database.HDBC
import Database.HDBC.PostgreSQL

import Control.Exception.Base (bracket)

import Network.HTTP.Types.Header
import Data.CaseInsensitive (CI(..))
import Text.Regex.TDFA ((=~))
import qualified Data.HashMap.Strict as Hash
import qualified Data.ByteString.Char8 as BS

import Dbapi (app, AppConfig(..))

cfg :: AppConfig
cfg = AppConfig "postgres://postgres:@localhost:5432/dbapi_test" 9000

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

appWithFixture :: ActionWith Application -> IO ()
appWithFixture action = withDatabaseConnection $ \c -> do
  runRaw c "begin;"
  action $ app c
  rollback c

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
