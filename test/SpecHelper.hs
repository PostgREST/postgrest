module SpecHelper where

import Network.Wai
import Test.Hspec

import Database.HDBC
import Database.HDBC.PostgreSQL

import Control.Exception.Base (bracket)

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
