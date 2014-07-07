{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module PgStructure where

import Control.Applicative

import Data.HashMap.Strict

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import qualified Data.Aeson as JSON

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

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
  where q = [sql|
              select table_schema, table_name,
                     is_insertable_into
                from information_schema.tables
               where table_schema = ? |]

columns :: T.Text -> Connection -> IO [Column]
columns t conn = query conn q $ Only t
  where q = [sql|
              select table_schema, table_name, column_name, ordinal_position,
                     is_nullable, data_type, is_updatable,
                     character_maximum_length, numeric_precision
                from information_schema.columns
               where table_name = ? |]

namedColumnHash :: [Column] -> HashMap String Column
namedColumnHash = fromList . (Prelude.zip =<< Prelude.map colName)

printTables :: Connection -> IO BL.ByteString
printTables conn = JSON.encode <$> tables "base" conn

printColumns :: T.Text -> Connection -> IO BL.ByteString
printColumns table conn = JSON.encode . namedColumnHash <$> columns table conn

selectAll :: T.Text -> Connection -> IO JSON.Value
selectAll table conn = fromOnly <$> Prelude.head <$> query conn q (Only safeName)
  where
    q = [sql|
          select array_to_json(array_agg(row_to_json(t)))
            from (select * from ?) t; |]
    safeName = QualifiedIdentifier (Just "base") table
