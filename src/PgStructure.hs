{-# LANGUAGE OverloadedStrings #-}

module PgStructure where

import Data.Functor ( (<$>) )
import Data.Maybe (mapMaybe)

import Control.Applicative ( (<*>) )

import qualified Data.ByteString.Lazy as BL
import Data.List.Split (splitOn)

import qualified Data.Aeson as JSON
import qualified Data.Map as Map

import Database.HDBC hiding (colType, colNullable)
import Database.HDBC.PostgreSQL

import Data.Aeson ((.=))

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

data ForeignKey = ForeignKey {
  fkTable::String, fkCol::String
} deriving (Eq, Show)

instance JSON.ToJSON ForeignKey where
  toJSON fk = JSON.object ["table".=fkTable fk, "column".=fkCol fk]

foreignKeys :: String -> String -> Connection -> IO (Map.Map String ForeignKey)
foreignKeys schema table conn = do
  r <- quickQuery conn
    "select kcu.column_name, ccu.table_name AS foreign_table_name,\
    \  ccu.column_name AS foreign_column_name \
    \from information_schema.table_constraints AS tc \
    \  join information_schema.key_column_usage AS kcu \
    \    on tc.constraint_name = kcu.constraint_name \
    \  join information_schema.constraint_column_usage AS ccu \
    \    on ccu.constraint_name = tc.constraint_name \
    \where constraint_type = 'FOREIGN KEY' \
    \  and tc.table_name=? and tc.table_schema = ? \
    \order by kcu.column_name" (map toSql [table, schema])
  return $ foldl addKey Map.empty $ map (map fromSql) r
  where
    addKey m [col, ftab, fcol] = Map.insert col (ForeignKey ftab fcol) m
    addKey m _ = m --should never happen

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
, colDefault :: Maybe String
, colEnum :: Maybe [String]
, colFK :: Maybe ForeignKey
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
    , "precision" .= colPrecision c
    , "references".= colFK c
    , "default"   .= colDefault c
    , "enum"      .= colEnum c ]

data TableOptions = TableOptions {
  tblOptcolumns :: [Column]
, tblOptpkey :: [String]
}

instance JSON.ToJSON TableOptions where
  toJSON t = JSON.object [
      "columns" .= tblOptcolumns t
    , "pkey"    .= tblOptpkey t ]

tables :: String -> Connection -> IO [Table]
tables s conn = do
  r <- quickQuery conn
        "select table_schema, table_name,\
        \       is_insertable_into\
        \  from information_schema.tables\
        \ where table_schema = ?\
        \ order by table_name" [toSql s]
  return $ mapMaybe mkTable r

  where
    mkTable [schema, name, insertable] =
      Just $ Table (fromSql schema)
        (fromSql name)
        (toBool (fromSql insertable))
    mkTable _ = Nothing

columns :: String -> String -> Connection -> IO [Column]
columns s t conn = do
  r <- quickQuery conn
    " select info.table_schema as schema, info.table_name as table_name,               \
    \        info.column_name as name, info.ordinal_position as position,              \
    \        info.is_nullable as nullable, info.data_type as col_type,                 \
    \        info.is_updatable as updatable, info.character_maximum_length as max_len, \
    \        info.numeric_precision as precision, info.column_default as default_value,\
    \        array_to_string(enum_info.vals, ',') as enum                              \
    \   from (                                                                         \
    \     select table_schema, table_name, column_name, ordinal_position,              \
    \            is_nullable, data_type, is_updatable,                                 \
    \            character_maximum_length, numeric_precision,                          \
    \            column_default, udt_name                                              \
    \       from information_schema.columns                                            \
    \      where table_schema = ?                                                      \
    \        and table_name = ?                                                        \
    \   ) as info                                                                      \
    \   left outer join (                                                              \
    \     select n.nspname as s,                                                       \
    \            t.typname as n,                                                       \
    \            array_agg(e.enumlabel) as vals                                        \
    \     from pg_type t                                                               \
    \        join pg_enum e on t.oid = e.enumtypid                                     \
    \        join pg_catalog.pg_namespace n ON n.oid = t.typnamespace                  \
    \     group by s, n                                                                \
    \   ) as enum_info                                                                 \
    \   on (info.udt_name = enum_info.n)                                               \
    \order by position" [toSql s, toSql t]
  fks <- foreignKeys s t conn
  let lookupFK (_:_:name:_) = Map.lookup (fromSql name) fks
      lookupFK _ = Nothing
  let cols = zipWith ($) (map mkColumn r) (map lookupFK r)
  return cols

  where
    mkColumn [schema, table, name, pos, nullable, colT, updatable, maxlen, precision, defVal, enum] = Column (fromSql schema)
        (fromSql table)
        (fromSql name)
        (fromSql pos)
        (toBool (fromSql nullable))
        (fromSql colT)
        (toBool (fromSql updatable))
        (fromSql maxlen)
        (fromSql precision)
        (fromSql defVal)
        (splitOn "," <$> fromSql enum)
    mkColumn _ =  error $ "Incomplete column data received for table " ++
      t ++ " in schema " ++ s ++ "."

printTables :: String -> Connection -> IO BL.ByteString
printTables schema conn = JSON.encode <$> tables schema conn

printColumns :: String -> String -> Connection -> IO BL.ByteString
printColumns schema table conn =
  JSON.encode <$> (TableOptions <$> cols <*> pkey)
  where
    cols :: IO [Column]
    cols = columns schema table conn
    pkey :: IO [String]
    pkey = primaryKeyColumns schema table conn

primaryKeyColumns :: String -> String -> Connection -> IO [String]
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
        \  and kc.table_name  = ?" [toSql s, toSql t]
  return $ map fromSql (concat r)
