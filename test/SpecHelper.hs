module SpecHelper where

import Network.Wai

import Database.HDBC
import Database.HDBC.PostgreSQL

import Dbapi (AppConfig(..))

cfg :: AppConfig
cfg = AppConfig "postgres://postgres:@localhost:5432/dbapi_test" 9000

loadFixture :: String -> IO Connection
loadFixture name = do
  conn <- connectPostgreSQL $ configDbUri cfg
  sql <- readFile $ "test/fixtures/" ++ name ++ ".sql"
  runRaw conn "drop schema if exists \"1\" cascade"
  runRaw conn sql
  commit conn
  return conn

prepareAppDb :: String -> Application -> IO Application
prepareAppDb = (. return) . (>>) . loadFixture
