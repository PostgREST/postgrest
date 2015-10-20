{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module PostgREST.PgStructure where

import           Control.Applicative
import           Data.Functor.Identity
import           Data.List             (find)
import           Data.Maybe            (fromMaybe, isJust, mapMaybe)
import           Data.Monoid
import           Data.Text             (Text, split)
import qualified Hasql                 as H
import qualified Hasql.Postgres        as P
import qualified Hasql.Backend         as B
import           PostgREST.PgQuery     ()
import           PostgREST.Types

import           GHC.Exts              (groupWith)
import           Prelude

doesProc :: forall c s. B.CxValue c Int =>
            (Text -> Text -> B.Stmt c) -> Text -> Text -> H.Tx c s Bool
doesProc stmt schema proc = do
  row :: Maybe (Identity Int) <- H.maybeEx $ stmt schema proc
  return $ isJust row

doesProcExist :: Text -> Text -> H.Tx P.Postgres s Bool
doesProcExist = doesProc [H.stmt|
      SELECT 1
      FROM   pg_catalog.pg_namespace n
      JOIN   pg_catalog.pg_proc p
      ON     pronamespace = n.oid
      WHERE  nspname = ?
      AND    proname = ?
    |]

doesProcReturnJWT :: Text -> Text -> H.Tx P.Postgres s Bool
doesProcReturnJWT = doesProc [H.stmt|
      SELECT 1
      FROM   pg_catalog.pg_namespace n
      JOIN   pg_catalog.pg_proc p
      ON     pronamespace = n.oid
      WHERE  nspname = ?
      AND    proname = ?
      AND    pg_catalog.pg_get_function_result(p.oid) = 'jwt'
    |]

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


relationFromRow :: (Text, Text, Text, Text, Text) -> Relation
relationFromRow (s, t, c, ft, fc) = Relation s t c ft fc Child Nothing Nothing Nothing

pkFromRow :: (Text, Text, Text) -> PrimaryKey
pkFromRow (s, t, n) = PrimaryKey s t n


addParentRelation :: Relation -> [Relation] -> [Relation]
addParentRelation rel@(Relation s t c ft fc _ _ _ _) rels = Relation s ft fc t c Parent Nothing Nothing Nothing:rel:rels

allTables :: H.Tx P.Postgres s [Table]
allTables = do
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

allRelations :: H.Tx P.Postgres s [Relation]
allRelations = do
  rels <- H.listEx $ [H.stmt|
    WITH table_fk AS (
        SELECT
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
        SELECT
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
    UNION
    (
        SELECT
            vcu.view_schema as table_schema,
            table_fk.table_name,
            table_fk.column_name,
            vcu.view_name as foreign_table_name,
            vcu.column_name as foreign_column_name
        FROM information_schema.view_column_usage as vcu
        JOIN table_fk ON
            table_fk.table_schema = vcu.view_schema AND
            table_fk.foreign_table_name = vcu.table_name AND
            table_fk.foreign_column_name = vcu.column_name
        WHERE vcu.view_schema NOT IN ('pg_catalog', 'information_schema')
        ORDER BY vcu.table_schema, vcu.view_name, vcu.column_name
    )
  |]
  let simpleRelations = foldr (addParentRelation.relationFromRow) [] rels
  let links = filter ((==2).length) $ groupWith groupFn $ filter ( (==Child). relType) simpleRelations
  return $ simpleRelations ++ mapMaybe link2Relation links
  where
    groupFn :: Relation -> Text
    groupFn (Relation{relSchema=s, relTable=t}) = s<>"_"<>t
    link2Relation [
      Relation{relSchema=sc, relTable=lt, relColumn=lc1, relFTable=t, relFColumn=c},
      Relation{                           relColumn=lc2, relFTable=ft, relFColumn=fc}
      ] = Just $ Relation sc t c ft fc Many (Just lt) (Just lc1) (Just lc2)
    link2Relation _ = Nothing

allColumns :: [Relation] -> H.Tx P.Postgres s [Column]
allColumns rels = do
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
    addFK col = col { colFK = relToFk <$> find (lookupFn col) rels }
    lookupFn :: Column -> Relation -> Bool
    lookupFn (Column{colSchema=cs, colTable=ct, colName=cn})  (Relation{relSchema=rs, relTable=rt, relColumn=rc, relType=rty}) =
      cs==rs && ct==rt && cn==rc && rty==Child
    lookupFn _ _ = False
    relToFk (Relation{relFTable=t, relFColumn=c}) = ForeignKey t c

allPrimaryKeys :: H.Tx P.Postgres s [PrimaryKey]
allPrimaryKeys = do
  pks <- H.listEx $ [H.stmt|
    WITH table_pk AS (
        SELECT
            kc.table_schema,
            kc.table_name,
            kc.column_name
        FROM
            information_schema.table_constraints tc,
            information_schema.key_column_usage kc
        WHERE
            tc.constraint_type = 'PRIMARY KEY' AND
            kc.table_name = tc.table_name AND
            kc.table_schema = tc.table_schema AND
            kc.constraint_name = tc.constraint_name AND
            kc.table_schema NOT IN ('pg_catalog', 'information_schema')
    )
    SELECT table_schema,
           table_name,
           column_name
    FROM table_pk
    UNION (
        SELECT
            vcu.view_schema,
            vcu.view_name,
            vcu.column_name
         FROM information_schema.view_column_usage AS vcu
         JOIN
            table_pk ON table_pk.table_schema = vcu.view_schema AND
            table_pk.table_name = vcu.table_name AND
            table_pk.column_name = vcu.column_name
         WHERE vcu.view_schema NOT IN ('pg_catalog','information_schema')
    )
    |]
  return $ map pkFromRow pks
