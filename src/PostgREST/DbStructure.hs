{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module PostgREST.DbStructure where

import           Control.Applicative
import           Control.Monad         (join)
import           Data.Functor.Identity
import           Data.List             (elemIndex, find, subsequences)
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

createDbStructure :: H.Tx P.Postgres s DbStructure
createDbStructure = do
  tabs <- allTables
  rels <- allRelations
  cols <- allColumns rels
  keys <- allPrimaryKeys

  return DbStructure {
      tables = tabs
    , columns = cols
    , relations = rels
    , primaryKeys = keys
    }

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
      AND    pg_catalog.pg_get_function_result(p.oid) like '%jwt_claims'
    |]

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


relationFromRow :: (Text, Text, [Text], Text, Text, [Text]) -> Relation
relationFromRow (s, t, cs, fs, ft, fcs) = Relation s t cs fs ft fcs Child Nothing Nothing Nothing Nothing

pkFromRow :: (Text, Text, Text) -> PrimaryKey
pkFromRow (s, t, n) = PrimaryKey s t n


addParentRelation :: Relation -> [Relation] -> [Relation]
addParentRelation rel@(Relation s t c fs ft fc _ _ _ _ _) rels = Relation fs ft fc s t c Parent Nothing Nothing Nothing Nothing:rel:rels

-- allTables :: H.Tx P.Postgres s [Table]
-- allTables = do
--     rows <- H.listEx $ [H.stmt|
--       SELECT
--         n.nspname AS table_schema,
--         c.relname AS table_name,
--         c.relkind = 'r' OR (c.relkind IN ('v','f'))
--         AND (pg_relation_is_updatable(c.oid::regclass, FALSE) & 8) = 8
--         OR (EXISTS
--           ( SELECT 1
--             FROM pg_trigger
--             WHERE pg_trigger.tgrelid = c.oid
--             AND (pg_trigger.tgtype::integer & 69) = 69) ) AS insertable,
--         array_to_string(array_agg(r.rolname), ',') AS acl
--       FROM pg_class c
--       CROSS JOIN pg_roles r
--       JOIN pg_namespace n ON n.oid = c.relnamespace
--       WHERE c.relkind IN ('v','r','m')
--         AND n.nspname NOT IN ('pg_catalog', 'information_schema')
--         AND (
--           pg_has_role(r.rolname, c.relowner, 'USAGE'::text) OR
--           has_table_privilege(r.rolname, c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text) OR
--           has_any_column_privilege(r.rolname, c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text) )
--
--       GROUP BY table_schema, table_name, insertable
--       ORDER BY table_schema, table_name
--     |]
--     return $ map tableFromRow rows

tables :: Text -> H.Tx P.Postgres s [Table]
tables schema = do
  rows <- H.listEx $
    [H.stmt|
      select
        n.nspname as table_schema,
        relname as table_name,
        c.relkind = 'r' or (c.relkind IN ('v', 'f')) and (pg_relation_is_updatable(c.oid::regclass, false) & 8) = 8
        or (exists (
           select 1
           from pg_trigger
           where pg_trigger.tgrelid = c.oid and (pg_trigger.tgtype::integer & 69) = 69)
        ) as insertable
      from
        pg_class c
        join pg_namespace n on n.oid = c.relnamespace
      where
        c.relkind in ('v', 'r', 'm')
        and n.nspname = ?
        and (
          pg_has_role(c.relowner, 'USAGE'::text)
          or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text)
          or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)
        )
      order by relname
    |] schema
  return $ map tableFromRow rows

allRelations :: H.Tx P.Postgres s [Relation]
allRelations = do
  rels <- H.listEx $ [H.stmt|
    WITH table_fk AS (
        SELECT ns1.nspname AS table_schema,
               tab.relname AS table_name,
               column_info.cols AS columns,
               ns2.nspname AS foreign_table_schema,
               other.relname AS foreign_table_name,
               column_info.refs AS foreign_columns
        FROM pg_constraint,
           LATERAL (SELECT array_agg(cols.attname) AS cols,
                           array_agg(cols.attnum)  AS nums,
                           array_agg(refs.attname) AS refs
                      FROM ( SELECT unnest(conkey) AS col, unnest(confkey) AS ref) k,
                           LATERAL (SELECT * FROM pg_attribute
                                     WHERE attrelid = conrelid AND attnum = col)
                                AS cols,
                           LATERAL (SELECT * FROM pg_attribute
                                     WHERE attrelid = confrelid AND attnum = ref)
                                AS refs)
                AS column_info,
           LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = connamespace) AS ns1,
           LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = conrelid) AS tab,
           LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = confrelid) AS other,
           LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = other.relnamespace) AS ns2
        WHERE confrelid != 0
        ORDER BY (conrelid, column_info.nums)
    )

    SELECT * FROM table_fk
    UNION
    (
        SELECT
            vcu.table_schema,
            vcu.view_name AS table_name,
            array_agg(vcu.column_name::text) AS columns,
            table_fk.foreign_table_schema,
            table_fk.foreign_table_name,
            table_fk.foreign_columns
        FROM information_schema.view_column_usage AS vcu
        JOIN table_fk ON
            table_fk.table_schema = vcu.view_schema AND
            table_fk.table_name = vcu.table_name AND
            vcu.column_name = ANY (table_fk.columns)
        WHERE vcu.view_schema NOT IN ('pg_catalog', 'information_schema')
        AND columns = table_fk.columns
        GROUP BY vcu.table_schema, vcu.view_name, table_fk.foreign_table_schema, table_fk.foreign_table_name, table_fk.foreign_columns
    )
    UNION
    (
        SELECT
            table_fk.table_schema,
            table_fk.table_name,
            table_fk.columns,
            vcu.view_schema AS foreign_table_schema,
            vcu.view_name AS foreign_table_name,
            array_agg(vcu.column_name::text) AS foreign_columns
        FROM information_schema.view_column_usage AS vcu
        JOIN table_fk ON
            table_fk.table_schema = vcu.view_schema AND
            table_fk.foreign_table_name = vcu.table_name AND
            vcu.column_name = ANY (table_fk.foreign_columns)
        WHERE vcu.view_schema NOT IN ('pg_catalog', 'information_schema')
            AND foreign_columns = table_fk.foreign_columns
        GROUP BY table_fk.table_schema, table_fk.table_name, vcu.view_schema, vcu.view_name, table_fk.columns
    )
  |]
  let simpleRelations = foldr (addParentRelation.relationFromRow) [] rels
      links = join $ map (combinations 2) $ filter (not . null) $ groupWith groupFn $ filter ( (==Child). relType) simpleRelations
  return $ simpleRelations ++ mapMaybe link2Relation links
  where
    groupFn :: Relation -> Text
    groupFn (Relation{relSchema=s, relTable=t}) = s<>"_"<>t
    combinations k ns = filter ((k==).length) (subsequences ns)
    link2Relation [
      Relation{relSchema=ls, relTable=lt, relColumns=lc1, relFSchema=s,  relFTable=t,  relFColumns=c},
      Relation{                           relColumns=lc2, relFSchema=fs, relFTable=ft, relFColumns=fc}
      ]
      | lc1 /= lc2 && length lc1 == 1 && length lc2 == 1 = Just $ Relation s t c fs ft fc Many (Just ls) (Just lt) (Just lc1) (Just lc2)
      | otherwise = Nothing
    link2Relation _ = Nothing


allColumns :: [Relation] -> H.Tx P.Postgres s [Column]
allColumns rels = do
  cols <- H.listEx $ [H.stmt|
      SELECT DISTINCT
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
    addFK col = col { colFK = fk col }
    fk col = join $ relToFk (colName col) <$> find (lookupFn col) rels
    lookupFn :: Column -> Relation -> Bool
    lookupFn (Column{colSchema=cs, colTable=ct, colName=cn})  (Relation{relSchema=rs, relTable=rt, relColumns=rc, relType=rty}) =
      cs==rs && ct==rt && cn `elem` rc && rty==Child
    lookupFn _ _ = False
    relToFk cName (Relation{relSchema=s, relFTable=t, relColumns=cs, relFColumns=fcs}) = ForeignKey s t <$> c
      where
        pos = elemIndex cName cs
        c = (fcs !!) <$> pos

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
