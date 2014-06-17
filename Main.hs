{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Applicative

data View = View {
  viewSchema :: String
, viewName :: String
, viewUpdatable :: Bool
, viewInsertable :: Bool
} deriving (Show)

instance FromRow View where
  fromRow = View <$> field <*> field <*>
    fmap toBool field <*> fmap toBool field

toBool :: String -> Bool
toBool = (== "YES")

data Column = Column {
  colSchema :: String
, colTable :: String
, colName :: String
, colPosition :: Int
, colNullable :: Bool
, colType :: String
, colUpdatable :: Bool
, colMaxLen :: Maybe Int
, colPrecision :: Maybe Int
} deriving (Show)

views :: Connection -> String -> IO [View]
views conn s = query conn q $ Only s
  where q = "select table_schema, table_name,\
            \       is_updatable, is_insertable_into \
            \  from information_schema.views\
            \ where table_schema = ?"

main :: IO ()
main = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "dbapi_test"
  }
  mapM_ print =<< views conn "base"
