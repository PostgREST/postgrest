{-# LANGUAGE OverloadedStrings #-}

module PgStructure where

import Control.Applicative

import Data.Text
import Data.HashMap.Strict
import Data.ByteString.Lazy

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import qualified Data.Aeson as JSON
import Data.Aeson ((.=))

data Table = Table {
  tableSchema :: String
, tableName :: String
, tableInsertable :: Bool
} deriving (Show)

instance FromRow Table where
  fromRow = Table <$> field <*> field <*> fmap toBool field

instance JSON.ToJSON Table where
  toJSON v = JSON.object [
      "schema"     .= tableSchema v
    , "name"       .= tableName v
    , "insertable" .= tableInsertable v ]

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
      "schema"    .= colSchema c
    , "name"      .= colName c
    , "position"  .= colPosition c
    , "nullable"  .= colNullable c
    , "type"      .= colType c
    , "updatable" .= colUpdatable c
    , "maxLen"    .= colMaxLen c
    , "precision" .= colPrecision c ]

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

namedColumnHash :: [Column] -> HashMap String Column
namedColumnHash = fromList . (Prelude.zip =<< Prelude.map colName)

printTables :: Connection -> IO ByteString
printTables conn = JSON.encode <$> tables "base" conn

printColumns :: Text -> Connection -> IO ByteString
printColumns table conn = JSON.encode . namedColumnHash <$> columns table conn
