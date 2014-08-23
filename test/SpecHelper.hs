module SpecHelper where

import Network.Wai
import Test.Hspec

import Database.HDBC
import Database.HDBC.PostgreSQL

import Control.Exception.Base (bracket)

import Dbapi (app, AppConfig(..))

import Debug.Trace

cfg :: AppConfig
cfg = AppConfig "postgres://postgres:@localhost:5432/dbapi_test" 9000

openConnection :: IO Connection
openConnection = connectPostgreSQL' $ configDbUri cfg

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection disconnect

loadFixture :: String -> Connection -> IO ()
loadFixture name conn = do
  runRaw conn "begin;"
  sql <- readFile $ "test/fixtures/" ++ name ++ ".sql"
  runRaw conn sql

rollbackFixture :: Connection -> IO ()
rollbackFixture = flip runRaw "rollback;"

dbWithSchema :: ActionWith Connection -> IO ()
dbWithSchema action = withDatabaseConnection $ \c -> do
  trace "Load fixture" loadFixture "schema" c
  trace "act" action (c)
  trace "rollback" rollbackFixture c

appWithFixture :: ActionWith Application -> IO ()
appWithFixture action = withDatabaseConnection $ \c -> do
  loadFixture "schema" c
  action (app cfg)
  rollbackFixture c
