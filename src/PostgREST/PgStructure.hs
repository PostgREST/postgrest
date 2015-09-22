{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeSynonymInstances,
             MultiParamTypeClasses, ScopedTypeVariables,
             FlexibleContexts #-}
module PostgREST.PgStructure where

import PostgREST.PgQuery (QualifiedIdentifier(..))
import PostgREST.Types
import Data.Text (Text, unpack, split)
import Data.List (find)
import Data.Aeson
import Data.Functor.Identity
import Data.String.Conversions (cs)
import Data.Maybe (fromMaybe, isJust)
import Control.Applicative

import qualified Data.Map as Map

import qualified Hasql as H
import qualified Hasql.Postgres as P

import Prelude

-----------
-- foreignKeys :: QualifiedIdentifier -> H.Tx P.Postgres s (Map.Map Text ForeignKey)
-- foreignKeys table = do
--   r <- H.listEx $ [H.stmt|
--       select kcu.column_name, ccu.table_name AS foreign_table_name,
--         ccu.column_name AS foreign_column_name
--       from information_schema.table_constraints AS tc
--         join information_schema.key_column_usage AS kcu
--           on tc.constraint_name = kcu.constraint_name
--         join information_schema.constraint_column_usage AS ccu
--           on ccu.constraint_name = tc.constraint_name
--       where constraint_type = 'FOREIGN KEY'
--         and tc.table_name=? and tc.table_schema = ?
--         order by kcu.column_name
--     |] (qiName table) (qiSchema table)
--
--   return $ foldl addKey Map.empty r
--   where
--     addKey :: Map.Map Text ForeignKey -> (Text, Text, Text) -> Map.Map Text ForeignKey
--     addKey m (col, ftab, fcol) = Map.insert col (ForeignKey ftab fcol) m
--
--
-- tables :: Text -> H.Tx P.Postgres s [Table]
-- tables schema = do
--   rows <- H.listEx $
--     [H.stmt|
--       select
--         n.nspname as table_schema,
--         relname as table_name,
--         c.relkind = 'r' or (c.relkind IN ('v', 'f')) and (pg_relation_is_updatable(c.oid::regclass, false) & 8) = 8
--         or (exists (
--            select 1
--            from pg_trigger
--            where pg_trigger.tgrelid = c.oid and (pg_trigger.tgtype::integer & 69) = 69)
--         ) as insertable
--       from
--         pg_class c
--         join pg_namespace n on n.oid = c.relnamespace
--       where
--         c.relkind in ('v', 'r', 'm')
--         and n.nspname = ?
--         and (
--           pg_has_role(c.relowner, 'USAGE'::text)
--           or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text)
--           or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)
--         )
--       order by relname
--     |] schema
--   return $ map tableFromRow rows


-- columns :: QualifiedIdentifier -> H.Tx P.Postgres s [Column]
-- columns table = do
--   cols <- H.listEx $ [H.stmt|
--       select info.table_schema as schema, info.table_name as table_name,
--             info.column_name as name, info.ordinal_position as position,
--             info.is_nullable::boolean as nullable, info.data_type as col_type,
--             info.is_updatable::boolean as updatable,
--             info.character_maximum_length as max_len,
--             info.numeric_precision as precision,
--             info.column_default as default_value,
--             array_to_string(enum_info.vals, ',') as enum
--         from (
--           select table_schema, table_name, column_name, ordinal_position,
--                  is_nullable, data_type, is_updatable,
--                  character_maximum_length, numeric_precision,
--                  column_default, udt_name
--             from information_schema.columns
--            where table_schema = ? and table_name = ?
--         ) as info
--         left outer join (
--           select n.nspname as s,
--                  t.typname as n,
--                  array_agg(e.enumlabel ORDER BY e.enumsortorder) as vals
--           from pg_type t
--             join pg_enum e on t.oid = e.enumtypid
--             join pg_catalog.pg_namespace n ON n.oid = t.typnamespace
--           group by s, n
--         ) as enum_info
--         on (info.udt_name = enum_info.n)
--       order by position |]
--     (qiSchema table) (qiName table)
--
--   fks <- foreignKeys table
--   return $ map (addFK fks . columnFromRow) cols
--
--   where
--     addFK fks col = col { colFK = Map.lookup (cs . colName $ col) fks }


primaryKeyColumns :: QualifiedIdentifier -> H.Tx P.Postgres s [Text]
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
      and kc.table_name  = ? |] (qiSchema table) (qiName table)
  return $ map runIdentity r

doesProcExist :: Text -> Text -> H.Tx P.Postgres s Bool
doesProcExist schema proc = do
  row :: Maybe (Identity Int) <- H.maybeEx $ [H.stmt|
      SELECT 1
      FROM   pg_catalog.pg_namespace n
      JOIN   pg_catalog.pg_proc p
      ON     pronamespace = n.oid
      WHERE  nspname = ?
      AND    proname = ?
    |] schema proc
  return $ isJust row


tableFromRow :: (Text, Text, Bool) -> Table
tableFromRow (s, n, i) = Table s n i

columnFromRow :: (Text,       Text,      Text,
                  Int,        Bool,      Text,
                  Bool,       Maybe Int, Maybe Int,
                  Maybe Text, Maybe Text)
              -> Column
columnFromRow (s, t, n, pos, nul, typ, u, l, p, d, e) =
  Column s t n pos nul typ u l p d (parseEnum e) Nothing

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
------------


relationFromRow :: (Text, Text, Text, Text, Text) -> Relation
relationFromRow (s, t, c, ft, fc) = Relation s t c ft fc "child"

pkFromRow :: (Text, Text, Text) -> PrimaryKey
pkFromRow (s, t, n) = PrimaryKey s t n


addFlippedRelation :: Relation -> [Relation] -> [Relation]
addFlippedRelation rel@(Relation s t c ft fc _) rels = Relation s ft fc t c "parent":rel:rels

alltables :: H.Tx P.Postgres s [Table]
alltables = do
    rows <- H.listEx $ [H.stmt|
            SELECT  n.nspname AS table_schema,
                    relname   AS TABLE_NAME,
                    c.relkind = 'r' OR (c.relkind IN ('v','f'))
                    AND (pg_relation_is_updatable(c.oid::regclass, FALSE) & 8) = 8
                    OR (EXISTS ( SELECT 1
                                 FROM pg_trigger
                                 WHERE pg_trigger.tgrelid = c.oid
                                 AND (pg_trigger.tgtype::integer & 69) = 69)
                    ) AS insertable
            FROM pg_class c
            JOIN pg_namespace n ON n.oid = c.relnamespace
            WHERE   c.relkind IN ('v','r','m')
                AND n.nspname NOT IN ('information_schema','pg_catalog')
                AND (  pg_has_role(c.relowner, 'USAGE'::text)
                    OR has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text)
                    OR has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)
                    )
            ORDER BY relname
        |]
    return $ map tableFromRow rows

allrelations :: H.Tx P.Postgres s [Relation]
allrelations = do
  rels <- H.listEx $ [H.stmt|
      WITH table_fk AS (
          SELECT DISTINCT
              tc.table_schema, tc.table_name, kcu.column_name,
              ccu.table_name AS foreign_table_name,
              ccu.column_name AS foreign_column_name
          FROM information_schema.table_constraints AS tc
          JOIN information_schema.key_column_usage AS kcu on tc.constraint_name = kcu.constraint_name
          JOIN information_schema.constraint_column_usage AS ccu on ccu.constraint_name = tc.constraint_name
          WHERE   constraint_type = 'FOREIGN KEY'
              AND tc.table_schema NOT IN ('pg_catalog', 'information_schema')
          ORDER BY tc.table_schema, tc.table_name, kcu.column_name
      )
      SELECT * FROM table_fk
      UNION
      (
          SELECT DISTINCT
              vcu.table_schema, vcu.view_name AS table_name, vcu.column_name,
              table_fk.foreign_table_name,
              table_fk.foreign_column_name
          FROM information_schema.view_column_usage as vcu
          JOIN table_fk ON
              table_fk.table_schema = vcu.view_schema AND
              table_fk.table_name = vcu.table_name AND
              table_fk.column_name = vcu.column_name
          WHERE vcu.view_schema NOT IN ('pg_catalog', 'information_schema')
          ORDER BY vcu.table_schema, vcu.view_name, vcu.column_name
      )

  |]
  return $ foldr (addFlippedRelation.relationFromRow) [] rels

allcolumns :: [Relation] -> H.Tx P.Postgres s [Column]
allcolumns relations = do
  cols <- H.listEx $ [H.stmt|
      SELECT
          info.table_schema AS schema,
          info.table_name AS table_name,
          info.column_name AS name,
          info.ordinal_position AS position,
          info.is_nullable::boolean AS nullable,
          info.data_type AS col_type,
          info.is_updatable::boolean AS updatable,
          info.character_maximum_length AS max_len,
          info.numeric_precision AS precision,
          info.column_default AS default_value,
          array_to_string(enum_info.vals, ',') AS enum
      FROM (
          SELECT
              table_schema,
              table_name,
              column_name,
              ordinal_position,
              is_nullable,
              data_type,
              is_updatable,
              character_maximum_length,
              numeric_precision,
              column_default,
              udt_name
          FROM information_schema.columns
          WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
      ) AS info
      LEFT OUTER JOIN (
          SELECT
              n.nspname AS s,
              t.typname AS n,
              array_agg(e.enumlabel ORDER BY e.enumsortorder) AS vals
          FROM pg_type t
          JOIN pg_enum e ON t.oid = e.enumtypid
          JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
          GROUP BY s,n
      ) AS enum_info ON (info.udt_name = enum_info.n)
      ORDER BY schema, position
  |]
  return $ map (addFK . columnFromRow) cols

  where
    addFK col = col { colFK = relToFk <$> find (lookupFn col) relations }
    lookupFn (Column{colSchema=cs, colTable=ct, colName=cn})  (Relation{relSchema=rs, relTable=rt, relColumn=rc, relType=rty}) =
      cs==rs && ct==rt && cn==rc && rty=="child"
    relToFk (Relation{relFTable=t, relFColumn=c}) = ForeignKey t c

allprimaryKeys :: H.Tx P.Postgres s [PrimaryKey]
allprimaryKeys = do
  pks <- H.listEx $ [H.stmt|
    SELECT kc.table_schema, kc.table_name, kc.column_name
    FROM information_schema.table_constraints tc,
     information_schema.key_column_usage kc
    WHERE tc.constraint_type = 'PRIMARY KEY'
    AND kc.table_name = tc.table_name
    AND kc.table_schema = tc.table_schema
    AND kc.constraint_name = tc.constraint_name
    AND kc.table_schema NOT IN ('pg_catalog', 'information_schema')
  |]
  return $ map pkFromRow pks

alltablesAcl :: H.Tx P.Postgres s [(Text, Text, Text)]
alltablesAcl = do
  acl <- H.listEx $ [H.stmt|
    SELECT
      table_schema,
      table_name,
      grantee as role
    FROM information_schema.role_table_grants
    WHERE
      table_schema NOT IN ('pg_catalog', 'information_schema') AND
      privilege_type = 'SELECT'
  |]
  return acl
