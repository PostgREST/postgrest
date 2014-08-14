-- {{{ Imports

{-# LANGUAGE OverloadedStrings #-}


module PgStructure where

import Data.Functor ( (<$>) )
import Data.Maybe (mapMaybe)

import Control.Applicative ( (<*>) )

import Data.HashMap.Strict hiding (map)

import qualified Data.ByteString.Lazy as BL

import qualified Data.Aeson as JSON

import Database.HDBC hiding (colType, colNullable)
import Database.HDBC.PostgreSQL

import Data.Aeson ((.=))

-- }}}

data Table = Table {
  tableSchema :: String
, tableName :: String
, tableInsertable :: Bool
} deriving (Show)

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

data TableOptions = TableOptions {
  tblOptcolumns :: HashMap String Column
, tblOptpkey :: [String]
}

instance JSON.ToJSON TableOptions where
  toJSON t = JSON.object [
      "columns" .= tblOptcolumns t
    , "pkey"   .= tblOptpkey t ]

tables :: String -> Connection -> IO [Table]
tables s conn = do
  r <- quickQuery conn
        "select table_schema, table_name,\
        \       is_insertable_into\
        \  from information_schema.tables\
        \ where table_schema = ?" [toSql s]
  return $ mapMaybe mkTable r

  where
    mkTable [schema, name, insertable] =
      Just $ Table (fromSql schema)
        (fromSql name)
        (toBool (fromSql insertable))
    mkTable _ = Nothing

columns :: Int -> String -> Connection -> IO [Column]
columns s t conn = do
  r <- quickQuery conn
        "select table_schema, table_name, column_name, ordinal_position,\
        \       is_nullable, data_type, is_updatable,\
        \       character_maximum_length, numeric_precision\
        \  from information_schema.columns\
        \ where table_schema = ?\
        \   and table_name = ?" [toSql (show s), toSql t]
  return $ mapMaybe mkColumn r

  where
    mkColumn [schema, table, name, pos, nullable, colT, updatable, maxlen, precision] =
      Just $ Column (fromSql schema)
        (fromSql table)
        (fromSql name)
        (fromSql pos)
        (toBool (fromSql nullable))
        (fromSql colT)
        (toBool (fromSql updatable))
        (fromSql maxlen)
        (fromSql precision)
    mkColumn _ = Nothing

namedColumnHash :: [Column] -> HashMap String Column
namedColumnHash = fromList . (Prelude.zip =<< Prelude.map colName)

printTables :: Int -> Connection -> IO BL.ByteString
printTables schema conn = JSON.encode <$> tables (show schema) conn

printColumns :: Int -> String -> Connection -> IO BL.ByteString
printColumns schema table conn =
  JSON.encode <$> (TableOptions <$> cols <*> pkey)
  where
    cols :: IO (HashMap String Column)
    cols = namedColumnHash <$> columns schema table conn
    pkey :: IO [String]
    pkey = primaryKeyColumns schema table conn

primaryKeyColumns :: Int -> String -> Connection -> IO [String]
primaryKeyColumns s t conn = do
  r <- quickQuery conn
        "select kc.column_name \
        \  from \
        \    information_schema.table_constraints tc, \
        \    information_schema.key_column_usage kc \
        \where \
        \  tc.constraint_type = 'PRIMARY KEY' \
        \  and kc.table_name = tc.table_name and kc.table_schema = tc.table_schema \
        \  and kc.constraint_name = tc.constraint_name \
        \  and kc.table_schema = ? \
        \  and kc.table_name  = ?" [toSql (show s), toSql t]
  return $ map fromSql (concat r)
