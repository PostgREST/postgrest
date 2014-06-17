{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.ByteString.Lazy

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Applicative

import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Network.HTTP.Types.Status

import qualified Data.Aeson as JSON

data View = View {
  viewSchema :: String
, viewName :: String
, viewUpdatable :: Bool
, viewInsertable :: Bool
} deriving (Show, Generic)

instance FromRow View where
  fromRow = View <$> field <*> field <*>
    fmap toBool field <*> fmap toBool field

instance JSON.ToJSON View

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

views :: String -> Connection -> IO [View]
views s conn = query conn q $ Only s
  where q = "select table_schema, table_name,\
            \       is_updatable, is_insertable_into \
            \  from information_schema.views\
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
  respond =<< responseLBS status200 [] <$> (payload =<< conn)
  where
    conn = connect defaultConnectInfo {
      connectDatabase = "dbapi_test"
    }
