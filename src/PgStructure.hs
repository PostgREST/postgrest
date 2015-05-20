{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeSynonymInstances,
             MultiParamTypeClasses, ScopedTypeVariables,
             FlexibleContexts #-}
module PgStructure where

import PgQuery (QualifiedTable(..))
import Data.Text hiding (foldl, map, zipWith, concat)
import Data.Aeson
import Data.Functor.Identity
import Data.String.Conversions (cs)
import Data.Maybe (fromMaybe)
import Control.Applicative

import qualified Data.Map as Map

import qualified Hasql as H
import qualified Hasql.Postgres as P

import Prelude

foreignKeys :: QualifiedTable -> H.Tx P.Postgres s (Map.Map Text ForeignKey)
foreignKeys table = do
  r <- H.listEx $ [H.stmt|
      select kcu.column_name, ccu.table_name AS foreign_table_name,
        ccu.column_name AS foreign_column_name
      from information_schema.table_constraints AS tc
        join information_schema.key_column_usage AS kcu
          on tc.constraint_name = kcu.constraint_name
        join information_schema.constraint_column_usage AS ccu
          on ccu.constraint_name = tc.constraint_name
      where constraint_type = 'FOREIGN KEY'
        and tc.table_name=? and tc.table_schema = ?
        order by kcu.column_name
    |] (qtName table) (qtSchema table)

  return $ foldl addKey Map.empty r
  where
    addKey :: Map.Map Text ForeignKey -> (Text, Text, Text) -> Map.Map Text ForeignKey
    addKey m (col, ftab, fcol) = Map.insert col (ForeignKey ftab fcol) m


tables :: Text -> H.Tx P.Postgres s [Table]
tables schema = do
  rows <- H.listEx $
    [H.stmt|
      select table_schema, table_name,
             is_insertable_into
        from information_schema.tables
       where table_schema = ?
       order by table_name
    |] schema
  return $ map tableFromRow rows


columns :: QualifiedTable -> H.Tx P.Postgres s [Column]
columns table = do
  cols <- H.listEx $ [H.stmt|
      select info.table_schema as schema, info.table_name as table_name,
             info.column_name as name, info.ordinal_position as position,
             info.is_nullable as nullable, info.data_type as col_type,
             info.is_updatable as updatable,
             info.character_maximum_length as max_len,
             info.numeric_precision as precision,
             info.column_default as default_value,
             array_to_string(enum_info.vals, ',') as enum
         from (
           select table_schema, table_name, column_name, ordinal_position,
                  is_nullable, data_type, is_updatable,
                  character_maximum_length, numeric_precision,
                  column_default, udt_name
             from information_schema.columns
            where table_schema = ? and table_name = ?
         ) as info
         left outer join (
           select n.nspname as s,
                  t.typname as n,
                  array_agg(e.enumlabel ORDER BY e.enumsortorder) as vals
           from pg_type t
              join pg_enum e on t.oid = e.enumtypid
              join pg_catalog.pg_namespace n ON n.oid = t.typnamespace
           group by s, n
         ) as enum_info
         on (info.udt_name = enum_info.n)
      order by position |]
    (qtSchema table) (qtName table)

  fks <- foreignKeys table
  return $ map (addFK fks . columnFromRow) cols

  where
    addFK fks col = col { colFK = Map.lookup (cs . colName $ col) fks }


primaryKeyColumns :: QualifiedTable -> H.Tx P.Postgres s [Text]
primaryKeyColumns table = do
  r <- H.listEx $ [H.stmt|
    select kc.column_name
      from
        information_schema.table_constraints tc,
        information_schema.key_column_usage kc
    where
      tc.constraint_type = 'PRIMARY KEY'
      and kc.table_name = tc.table_name and kc.table_schema = tc.table_schema
      and kc.constraint_name = tc.constraint_name
      and kc.table_schema = ?
      and kc.table_name  = ? |] (qtSchema table) (qtName table)
  return $ map runIdentity r


toBool :: Text -> Bool
toBool = (== "YES")

data Table = Table {
  tableSchema :: Text
, tableName :: Text
, tableInsertable :: Bool
} deriving (Show)

data ForeignKey = ForeignKey {
  fkTable::Text, fkCol::Text
} deriving (Eq, Show)

data Column = Column {
  colSchema :: Text
, colTable :: Text
, colName :: Text
, colPosition :: Int
, colNullable :: Bool
, colType :: Text
, colUpdatable :: Bool
, colMaxLen :: Maybe Int
, colPrecision :: Maybe Int
, colDefault :: Maybe Text
, colEnum :: [Text]
, colFK :: Maybe ForeignKey
} deriving (Show)

tableFromRow :: (Text, Text, Text) -> Table
tableFromRow (s, n, i) = Table s n (toBool i)

columnFromRow :: (Text,       Text,      Text,
                  Int,        Text,      Text,
                  Text,       Maybe Int, Maybe Int,
                  Maybe Text, Maybe Text)
              -> Column
columnFromRow (s, t, n, pos, nul, typ, u, l, p, d, e) =
  Column s t n pos (toBool nul) typ (toBool u) l p d (parseEnum e) Nothing

  where
    parseEnum :: Maybe Text -> [Text]
    parseEnum str = fromMaybe [] $ split (==',') <$> str


instance ToJSON Column where
  toJSON c = object [
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

instance ToJSON ForeignKey where
  toJSON fk = object ["table".=fkTable fk, "column".=fkCol fk]

instance ToJSON Table where
  toJSON v = object [
      "schema"     .= tableSchema v
    , "name"       .= tableName v
    , "insertable" .= tableInsertable v ]
