{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.ByteString.Lazy

import Data.Text.Encoding (encodeUtf8)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Applicative

import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Network.HTTP.Types.Status

import qualified Data.Aeson as JSON

data Table = Table {
  viewSchema :: String
, viewName :: String
, viewInsertable :: Bool
} deriving (Show, Generic)

instance FromRow Table where
  fromRow = Table <$> field <*> field <*> fmap toBool field

instance JSON.ToJSON Table

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

views :: String -> Connection -> IO [Table]
views s conn = query conn q $ Only s
  where q = "select table_schema, table_name,\
            \       is_insertable_into \
            \  from information_schema.tables\
            \ where table_schema = ?"

main :: IO ()
main = do
  let port = 3000
  Prelude.putStrLn $ "Listening on port " ++ show port
  run port app

payload :: Connection -> IO ByteString
payload conn = JSON.encode <$> views "base" conn

app :: Application
app req respond =
  case path of
    [] -> respond =<< responseLBS status200 [] <$> (payload =<< conn)
    -- [table] -> respond $ responseLBS status200 [] $ encodeUtf8 table
    _ -> respond $ responseLBS status404 [] ""
  where
    path = pathInfo req
    conn = connect defaultConnectInfo {
      connectDatabase = "dbapi_test"
    }
