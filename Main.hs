{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.ByteString.Lazy

import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Text.Read

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Applicative

import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Network.HTTP.Types.Status

import qualified Data.Aeson as JSON
import Data.Aeson ((.=))

data Table = Table {
  viewSchema :: String
, viewName :: String
, viewInsertable :: Bool
} deriving (Show)

instance FromRow Table where
  fromRow = Table <$> field <*> field <*> fmap toBool field

instance JSON.ToJSON Table where
  toJSON v = JSON.object [
    "schema" .= (viewSchema v),
    "name"   .= (viewName v),
    "insertable" .= (viewInsertable v)
    ]

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

instance FromRow Column where
  fromRow = Column <$> field <*> field <*> field <*> field <*>
                       fmap toBool field <*> field <*>
                       fmap toBool field <*>
                       field <*> field

instance JSON.ToJSON Column where
  toJSON c = JSON.object [
      "schema" .= (colSchema c)
    , "name"   .= (colName c)
    , "position" .= (colPosition c)
    , "nullable" .= (colNullable c)
    , "type"   .= (colType c)
    , "updatable" .= (colUpdatable c)
    , "maxLen" .= (colMaxLen c)
    , "precision" .= (colPrecision c) ]

tables :: String -> Connection -> IO [Table]
tables s conn = query conn q $ Only s
  where q = "select table_schema, table_name,\
            \       is_insertable_into \
            \  from information_schema.tables\
            \ where table_schema = ?"

columns :: Text -> Connection -> IO [Column]
columns t conn = query conn q $ Only t
  where q = "select table_schema, table_name, column_name, ordinal_position,\
            \       is_nullable, data_type, is_updatable,\
            \       character_maximum_length, numeric_precision\
            \  from information_schema.columns\
            \ where table_name = ?"

main :: IO ()
main = do
  let port = 3000
  Prelude.putStrLn $ "Listening on port " ++ show port
  run port app

printTables :: Connection -> IO ByteString
printTables conn = JSON.encode <$> tables "base" conn

printColumns :: Text -> Connection -> IO ByteString
printColumns tableName conn = JSON.encode <$> columns tableName conn

app :: Application
app req respond =
  case path of
    []      -> respond =<< responseLBS status200 [] <$> (printTables =<< conn)
    [table] -> respond =<< responseLBS status200 [] <$> (printColumns table =<< conn)
    _       -> respond $ responseLBS status404 [] ""
  where
    path = pathInfo req
    conn = connect defaultConnectInfo {
      connectDatabase = "dbapi_test"
    }
