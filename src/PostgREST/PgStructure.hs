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


tableFromRow :: (Text, Text, Bool, Maybe Text) -> Table
tableFromRow (s, n, i, a) = Table s n i (parseAcl a)
  where
    parseAcl :: Maybe Text -> [Text]
    parseAcl str = fromMaybe [] $ split (==',') <$> str

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
      SELECT
        n.nspname AS table_schema,
        c.relname AS table_name,
        c.relkind = 'r' OR (c.relkind IN ('v','f'))
        AND (pg_relation_is_updatable(c.oid::regclass, FALSE) & 8) = 8
        OR (EXISTS
          ( SELECT 1
            FROM pg_trigger
            WHERE pg_trigger.tgrelid = c.oid
            AND (pg_trigger.tgtype::integer & 69) = 69) ) AS insertable,
        array_to_string(array_agg(r.rolname), ',') AS acl
      FROM pg_class c
      CROSS JOIN pg_roles r
      JOIN pg_namespace n ON n.oid = c.relnamespace
      WHERE c.relkind IN ('v','r','m')
        AND n.nspname NOT IN ('pg_catalog', 'information_schema')
        AND (
          pg_has_role(r.rolname, c.relowner, 'USAGE'::text) OR
          has_table_privilege(r.rolname, c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text) OR
          has_any_column_privilege(r.rolname, c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text) )

      GROUP BY table_schema, table_name, insertable
      ORDER BY table_schema, table_name
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
  WITH table_pk AS
  (
  SELECT kc.table_schema, kc.table_name, kc.column_name
    FROM information_schema.table_constraints tc,
     information_schema.key_column_usage kc
    WHERE tc.constraint_type = 'PRIMARY KEY'
    AND kc.table_name = tc.table_name
    AND kc.table_schema = tc.table_schema
    AND kc.constraint_name = tc.constraint_name
    AND kc.table_schema NOT IN ('pg_catalog', 'information_schema')
  )
  SELECT table_schema, table_name, column_name
  FROM table_pk
  UNION
  (
  SELECT vcu.view_schema, vcu.view_name, vcu.column_name
  FROM information_schema.view_column_usage AS vcu
  JOIN table_pk ON table_pk.table_schema = vcu.view_schema
  AND table_pk.table_name = vcu.table_name
  AND table_pk.column_name = vcu.column_name
  WHERE vcu.view_schema NOT IN ('pg_catalog', 'information_schema')
  )
  |]
  return $ map pkFromRow pks

-- alltablesAcl :: H.Tx P.Postgres s [(Text, Text, Text)]
-- alltablesAcl = do
--   acl <- H.listEx $ [H.stmt|
--     SELECT
--       table_schema,
--       table_name,
--       grantee as role
--     FROM information_schema.role_table_grants
--     WHERE
--       table_schema NOT IN ('pg_catalog', 'information_schema') AND
--       privilege_type = 'SELECT'
--   |]
--   return acl
