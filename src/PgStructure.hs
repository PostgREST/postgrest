{-# LANGUAGE QuasiQuotes, MultiParamTypeClasses, ScopedTypeVariables #-}
module PgStructure where

import PgQuery (QualifiedTable(..))
import Data.Functor ( (<$>) )
import Data.Text hiding (foldl, map, zipWith, concat)
import Data.Aeson
import Data.Functor.Identity
import qualified Data.Vector as V
import Data.String.Conversions (cs)

import Control.Applicative ( (<*>) )

import qualified Data.List as L
import qualified Data.Map as Map

import qualified Hasql as H
import qualified Hasql.Backend as H
import qualified Hasql.Postgres as H

foreignKeys :: QualifiedTable -> H.Tx H.Postgres s (Map.Map Text ForeignKey)
foreignKeys table = do
  r :: [(Text, Text, Text)] <- H.list $ [H.q|
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
    addKey m (col, ftab, fcol) = Map.insert col (ForeignKey (cs ftab) (cs fcol)) m


tables :: Text -> H.Tx H.Postgres s [Table]
tables schema =
  H.list $ [H.q|
      select table_schema, table_name,
             is_insertable_into
        from information_schema.tables
       where table_schema = ?
       order by table_name
    |] schema


columns :: QualifiedTable -> H.Tx H.Postgres s [Column]
columns table = do
  cols <- H.list $ [H.q|
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
    order by position |] (qtSchema table) (qtName table)

  fks <- foreignKeys table
  return $ map (\col -> col { colFK = Map.lookup (cs . colName $ col) fks }) cols


primaryKeyColumns :: QualifiedTable -> H.Tx H.Postgres s [Text]
primaryKeyColumns table = do
  r :: [Identity Text] <- H.list $ [H.q|
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


vanishNull :: [a] -> Maybe [a]
vanishNull xs = if L.null xs then Nothing else Just xs

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
, colEnum :: Maybe [Text]
, colFK :: Maybe ForeignKey
} deriving (Show)

instance H.RowParser H.Postgres Column where
  parseRow r =
    let schema     = H.parseResult $ r V.! 0
        table      = H.parseResult $ r V.! 1
        name       = H.parseResult $ r V.! 2
        position   = H.parseResult $ r V.! 3
        nullable   = toBool <$> (H.parseResult $ r V.! 4 :: Either Text Text)
        typ        = H.parseResult $ r V.! 5
        updatable  = toBool <$> (H.parseResult $ r V.! 6 :: Either Text Text)
        maxLen     = H.parseResult $ r V.! 7
        precision  = H.parseResult $ r V.! 8
        defValue   = H.parseResult $ r V.! 9
        enum       = H.parseResult $ r V.! 10 in
    if V.length r /= 11
       then Left "Wrong number of fields in Column"
       else Column <$> schema <*> table <*> name <*> position <*> nullable
                   <*> typ <*> updatable <*> maxLen <*> precision
                   <*> defValue <*> enum <*> return Nothing


instance H.RowParser H.Postgres Table where
  parseRow r =
    let schema     = H.parseResult $ r V.! 0
        name       = H.parseResult $ r V.! 2
        insertable = toBool <$> (H.parseResult $ r V.! 3 :: Either Text Text) in
    if V.length r /= 3
       then Left "Wrong number of fields in Table"
       else Table <$> schema <*> name <*> insertable

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
