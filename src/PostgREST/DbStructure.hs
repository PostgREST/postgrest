{-|
Module      : PostgREST.DbStructure
Description : PostgREST schema cache

This module contains queries that target PostgreSQL system catalogs, these are used to build the schema cache(DbStructure).

The schema cache is necessary for resource embedding, foreign keys are used for inferring the relationships between tables.

These queries are executed once at startup or when PostgREST is reloaded.
-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module PostgREST.DbStructure
  ( DbStructure(..)
  , queryDbStructure
  , accessibleTables
  , accessibleProcs
  , schemaDescription
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as S
import qualified Hasql.Decoders      as HD
import qualified Hasql.Encoders      as HE
import qualified Hasql.Statement     as SQL
import qualified Hasql.Transaction   as SQL

import Contravariant.Extras          (contrazip2)
import Text.InterpolatedString.Perl6 (q)

import PostgREST.Config.Database          (pgVersionStatement)
import PostgREST.Config.PgVersion         (PgVersion, pgVersion100,
                                           pgVersion110)
import PostgREST.DbStructure.Identifiers  (FieldName,
                                           QualifiedIdentifier (..),
                                           Schema)
import PostgREST.DbStructure.Proc         (PgType (..),
                                           ProcDescription (..),
                                           ProcParam (..),
                                           ProcVolatility (..),
                                           ProcsMap, RetType (..))
import PostgREST.DbStructure.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..),
                                           RelationshipsMap)
import PostgREST.DbStructure.Table        (Column (..), Table (..),
                                           TablesMap)

import Protolude


data DbStructure = DbStructure
  { dbTables        :: TablesMap
  , dbRelationships :: RelationshipsMap
  , dbProcs         :: ProcsMap
  }
  deriving (Generic, JSON.ToJSON)

-- | A view foreign key or primary key dependency detected on its source table
data ViewKeyDependency = ViewKeyDependency {
  keyDepTable :: QualifiedIdentifier
, keyDepView  :: QualifiedIdentifier
, keyDepCons  :: Text
, keyDepType  :: KeyDep
, keyDepCols  :: [(FieldName, FieldName)] -- ^ First element is the table column, second is the view column
} deriving (Eq)
data KeyDep
  = PKDep    -- ^ PK dependency
  | FKDep    -- ^ FK dependency
  | FKDepRef -- ^ FK reference dependency
  deriving (Eq)

-- | A SQL query that can be executed independently
type SqlQuery = ByteString

queryDbStructure :: [Schema] -> [Schema] -> Bool -> SQL.Transaction DbStructure
queryDbStructure schemas extraSearchPath prepared = do
  SQL.sql "set local schema ''" -- This voids the search path. The following queries need this for getting the fully qualified name(schema.name) of every db object
  pgVer   <- SQL.statement mempty pgVersionStatement
  tabs    <- SQL.statement schemas $ allTables pgVer prepared
  keyDeps <- SQL.statement (schemas, extraSearchPath) $ allViewsKeyDependencies prepared
  m2oRels <- SQL.statement mempty $ allM2ORels pgVer prepared
  procs   <- SQL.statement schemas $ allProcs pgVer prepared

  let tabsWViewsPks = addViewPrimaryKeys tabs keyDeps
      rels          = relsToMap $ addO2MRels $ addM2MRels tabsWViewsPks $ addViewM2ORels keyDeps m2oRels

  return $ removeInternal schemas $ DbStructure {
      dbTables = tabsWViewsPks
    , dbRelationships = rels
    , dbProcs = procs
    }
  where
    relsToMap = map sort . HM.fromListWith (++) . map ((\(x, fSch, y) -> ((x, fSch), [y])) . addKey)
    addKey rel = (relTable rel, qiSchema $ relForeignTable rel, rel)

-- | Remove db objects that belong to an internal schema(not exposed through the API) from the DbStructure.
removeInternal :: [Schema] -> DbStructure -> DbStructure
removeInternal schemas dbStruct =
  DbStructure {
      dbTables        = HM.filterWithKey (\(QualifiedIdentifier sch _) _ -> sch `elem` schemas) $ dbTables dbStruct
    , dbRelationships = filter (\r -> qiSchema (relForeignTable r) `elem` schemas && not (hasInternalJunction r)) <$>
                        HM.filterWithKey (\(QualifiedIdentifier sch _, _) _ -> sch `elem` schemas ) (dbRelationships dbStruct)
    , dbProcs         = dbProcs dbStruct -- procs are only obtained from the exposed schemas, no need to filter them.
    }
  where
    hasInternalJunction rel = case relCardinality rel of
      M2M Junction{junTable} -> qiSchema junTable `notElem` schemas
      _                      -> False

decodeTables :: HD.Result TablesMap
decodeTables =
 HM.fromList . map (\tbl@Table{tableSchema, tableName} -> (QualifiedIdentifier tableSchema tableName, tbl)) <$> HD.rowList tblRow
 where
  tblRow = Table
    <$> column HD.text
    <*> column HD.text
    <*> nullableColumn HD.text
    <*> column HD.bool
    <*> column HD.bool
    <*> column HD.bool
    <*> column HD.bool
    <*> arrayColumn HD.text
    <*> compositeArrayColumn
        (Column
        <$> compositeField HD.text
        <*> nullableCompositeField HD.text
        <*> compositeField HD.bool
        <*> compositeField HD.text
        <*> nullableCompositeField HD.int4
        <*> nullableCompositeField HD.text
        <*> compositeFieldArray HD.text)

decodeRels :: HD.Result [Relationship]
decodeRels =
 HD.rowList relRow
 where
  relRow =
    Relationship <$>
    (QualifiedIdentifier <$> column HD.text <*> column HD.text) <*>
    (QualifiedIdentifier <$> column HD.text <*> column HD.text) <*>
    column HD.bool <*>
    (M2O <$> column HD.text <*> compositeArrayColumn ((,) <$> compositeField HD.text <*> compositeField HD.text)) <*>
    pure False <*>
    pure False

decodeViewKeyDeps :: HD.Result [ViewKeyDependency]
decodeViewKeyDeps =
  map viewKeyDepFromRow <$> HD.rowList row
 where
  row = (,,,,,,)
    <$> column HD.text <*> column HD.text
    <*> column HD.text <*> column HD.text
    <*> column HD.text <*> column HD.text
    <*> compositeArrayColumn
        ((,)
        <$> compositeField HD.text
        <*> compositeField HD.text)

viewKeyDepFromRow :: (Text,Text,Text,Text,Text,Text,[(Text, Text)]) -> ViewKeyDependency
viewKeyDepFromRow (s1,t1,s2,v2,cons,consType,sCols) = ViewKeyDependency (QualifiedIdentifier s1 t1) (QualifiedIdentifier s2 v2) cons keyDep sCols
  where
    keyDep | consType == "p" = PKDep
           | consType == "f" = FKDep
           | otherwise       = FKDepRef -- f_ref, we build this type in the query

decodeProcs :: HD.Result ProcsMap
decodeProcs =
  -- Duplicate rows for a function means they're overloaded, order these by least args according to ProcDescription Ord instance
  map sort . HM.fromListWith (++) . map ((\(x,y) -> (x, [y])) . addKey) <$> HD.rowList procRow
  where
    procRow = ProcDescription
              <$> column HD.text
              <*> column HD.text
              <*> nullableColumn HD.text
              <*> compositeArrayColumn
                  (ProcParam
                  <$> compositeField HD.text
                  <*> compositeField HD.text
                  <*> compositeField HD.bool
                  <*> compositeField HD.bool)
              <*> (parseRetType
                  <$> column HD.text
                  <*> column HD.text
                  <*> column HD.bool
                  <*> column HD.bool
                  <*> column HD.bool)
              <*> (parseVolatility <$> column HD.char)
              <*> column HD.bool

    addKey :: ProcDescription -> (QualifiedIdentifier, ProcDescription)
    addKey pd = (QualifiedIdentifier (pdSchema pd) (pdName pd), pd)

    parseRetType :: Text -> Text -> Bool -> Bool -> Bool -> Maybe RetType
    parseRetType schema name isSetOf isComposite isVoid
      | isVoid    = Nothing
      | isSetOf   = Just (SetOf pgType)
      | otherwise = Just (Single pgType)
      where
        qi = QualifiedIdentifier schema name
        pgType
          | isComposite = Composite qi
          | otherwise   = Scalar

    parseVolatility :: Char -> ProcVolatility
    parseVolatility v | v == 'i' = Immutable
                      | v == 's' = Stable
                      | otherwise = Volatile -- only 'v' can happen here

allProcs :: PgVersion -> Bool -> SQL.Statement [Schema] ProcsMap
allProcs pgVer = SQL.Statement sql (arrayParam HE.text) decodeProcs
  where
    sql = procsSqlQuery pgVer <> " AND pn.nspname = ANY($1)"

accessibleProcs :: PgVersion -> Bool -> SQL.Statement Schema ProcsMap
accessibleProcs pgVer = SQL.Statement sql (param HE.text) decodeProcs
  where
    sql = procsSqlQuery pgVer <> " AND pn.nspname = $1 AND has_function_privilege(p.oid, 'execute')"

procsSqlQuery :: PgVersion -> SqlQuery
procsSqlQuery pgVer = [q|
 -- Recursively get the base types of domains
  WITH
  base_types AS (
    WITH RECURSIVE
    recurse AS (
      SELECT
        oid,
        typbasetype,
        COALESCE(NULLIF(typbasetype, 0), oid) AS base
      FROM pg_type
      UNION
      SELECT
        t.oid,
        b.typbasetype,
        COALESCE(NULLIF(b.typbasetype, 0), b.oid) AS base
      FROM recurse t
      JOIN pg_type b ON t.typbasetype = b.oid
    )
    SELECT
      oid,
      base
    FROM recurse
    WHERE typbasetype = 0
  ),
  arguments AS (
    SELECT
      oid,
      array_agg((
        COALESCE(name, ''), -- name
        type::regtype::text, -- type
        idx <= (pronargs - pronargdefaults), -- is_required
        COALESCE(mode = 'v', FALSE) -- is_variadic
      ) ORDER BY idx) AS args,
      CASE COUNT(*) - COUNT(name) -- number of unnamed arguments
        WHEN 0 THEN true
        WHEN 1 THEN (array_agg(type))[1] IN ('bytea'::regtype, 'json'::regtype, 'jsonb'::regtype, 'text'::regtype, 'xml'::regtype)
        ELSE false
      END AS callable
    FROM pg_proc,
         unnest(proargnames, proargtypes, proargmodes)
           WITH ORDINALITY AS _ (name, type, mode, idx)
    WHERE type IS NOT NULL -- only input arguments
    GROUP BY oid
  )
  SELECT
    pn.nspname AS proc_schema,
    p.proname AS proc_name,
    d.description AS proc_description,
    COALESCE(a.args, '{}') AS args,
    tn.nspname AS schema,
    COALESCE(comp.relname, t.typname) AS name,
    p.proretset AS rettype_is_setof,
    (t.typtype = 'c'
     -- if any TABLE, INOUT or OUT arguments present, treat as composite
     or COALESCE(proargmodes::text[] && '{t,b,o}', false)
    ) AS rettype_is_composite,
    ('void'::regtype = t.oid) AS rettype_is_void,
    p.provolatile,
    p.provariadic > 0 as hasvariadic
  FROM pg_proc p
  LEFT JOIN arguments a ON a.oid = p.oid
  JOIN pg_namespace pn ON pn.oid = p.pronamespace
  JOIN base_types bt ON bt.oid = p.prorettype
  JOIN pg_type t ON t.oid = bt.base
  JOIN pg_namespace tn ON tn.oid = t.typnamespace
  LEFT JOIN pg_class comp ON comp.oid = t.typrelid
  LEFT JOIN pg_description as d ON d.objoid = p.oid
  WHERE t.oid <> 'trigger'::regtype AND COALESCE(a.callable, true)
|] <> (if pgVer >= pgVersion110 then "AND prokind = 'f'" else "AND NOT (proisagg OR proiswindow)")

schemaDescription :: Bool -> SQL.Statement Schema (Maybe Text)
schemaDescription =
    SQL.Statement sql (param HE.text) (join <$> HD.rowMaybe (nullableColumn HD.text))
  where
    sql = [q|
      select
        description
      from
        pg_namespace n
        left join pg_description d on d.objoid = n.oid
      where
        n.nspname = $1 |]

accessibleTables :: PgVersion -> Bool -> SQL.Statement [Schema] TablesMap
accessibleTables pgVer =
  SQL.Statement sql (arrayParam HE.text) decodeTables
 where
  sql = tablesSqlQuery False pgVer

{-
Adds M2O relationships for views to tables, tables to views, and views to views. The example below is taken from the test fixtures, but the views names/colnames were modified.

--allM2ORels sample query result--
private      | personnages          | private    | actors           | personnages_role_id_fkey   | {"(role_id,id)"}

--allViewsKeyDependencies sample query result--
private      | personnages          | test       | personnages_view | personnages_role_id_fkey   | f       | {"(role_id,roleId)"}
private      | actors               | test       | actors_view      | personnages_role_id_fkey   | f_ref   | {"(id,actorId)"}

--this function result--
test         | personnages_view     | private    | actors           | personnages_role_id_fkey   | f        | {"(roleId,id)"}       | viewTableM2O
private      | personnages          | test       | actors_view      | personnages_role_id_fkey   | f_ref    | {"(role_id,actorId)"} | tableViewM2O
test         | personnages_view     | test       | actors_view      | personnages_role_id_fkey   | f,r_ref  | {"(roleId,actorId)"}  | viewViewM2O
-}
addViewM2ORels :: [ViewKeyDependency] -> [Relationship] -> [Relationship]
addViewM2ORels keyDeps rels =
  rels ++ concat (viewRels <$> rels)
  where
    viewRels Relationship{relTable,relForeignTable,relCardinality=M2O cons relColumns} =
      let
        viewTableM2Os = filter (\ViewKeyDependency{keyDepTable, keyDepCons, keyDepType} -> keyDepTable == relTable        && keyDepCons == cons && keyDepType == FKDep)    keyDeps
        tableViewM2Os = filter (\ViewKeyDependency{keyDepTable, keyDepCons, keyDepType} -> keyDepTable == relForeignTable && keyDepCons == cons && keyDepType == FKDepRef) keyDeps
      in
        [ Relationship
            (keyDepView vwTbl)
            relForeignTable
            False
            (M2O cons $ zipWith (\(_, vCol) (_, fCol)-> (vCol, fCol)) (keyDepCols vwTbl) relColumns)
            True
            False
        | vwTbl <- viewTableM2Os ]
        ++
        [ Relationship
            relTable
            (keyDepView tblVw)
            False
            (M2O cons $ zipWith (\(tCol, _) (_, vCol) -> (tCol, vCol)) relColumns (keyDepCols tblVw))
            False
            True
        | tblVw <- tableViewM2Os ]
        ++
        [
          let
            vw1 = keyDepView vwTbl
            vw2 = keyDepView tblVw
          in
          Relationship
            vw1
            vw2
            (vw1 == vw2)
            (M2O cons $ zipWith (\(_, vcol1) (_, vcol2) -> (vcol1, vcol2)) (keyDepCols vwTbl) (keyDepCols tblVw))
            True
            True
        | vwTbl <- viewTableM2Os
        , tblVw <- tableViewM2Os ]
    viewRels _ = []


addO2MRels :: [Relationship] -> [Relationship]
addO2MRels rels = rels ++ [ Relationship ft t isSelf (O2M cons (swap <$> cols)) fTableIsView tableIsView
                          | Relationship t ft isSelf (M2O cons cols) tableIsView fTableIsView <- rels ]

-- | Adds a m2m relationship if a table has FKs to two other tables and the FK columns are part of the PK columns
addM2MRels :: TablesMap -> [Relationship] -> [Relationship]
addM2MRels tbls rels = rels ++ catMaybes
  [ let
      jtCols = S.fromList $ (fst <$> cols) ++ (fst <$> fcols)
      pkCols = S.fromList $ maybe mempty tablePKCols $ HM.lookup jt1 tbls
    in if S.isSubsetOf jtCols pkCols
      then Just $ Relationship t ft (t == ft) (M2M $ Junction jt1 cons1 cons2 (swap <$> cols) (swap <$> fcols)) tblIsView fTblisView
      else Nothing
  | Relationship jt1 t  _ (M2O cons1 cols)  _ tblIsView <- rels
  , Relationship jt2 ft _ (M2O cons2 fcols) _ fTblisView <- rels
  , jt1 == jt2
  , cons1 /= cons2]

addViewPrimaryKeys :: TablesMap -> [ViewKeyDependency] -> TablesMap
addViewPrimaryKeys tabs keyDeps =
  (\tbl@Table{tableSchema, tableName, tableIsView}-> if tableIsView
    then tbl{tablePKCols=findViewPKCols tableSchema tableName}
    else tbl) <$> tabs
  where
    findViewPKCols sch vw =
      maybe [] (\(ViewKeyDependency _ _ _ _ pkCols) -> snd <$> pkCols) $
      find (\(ViewKeyDependency _ viewQi _ dep _) -> dep == PKDep && viewQi == QualifiedIdentifier sch vw) keyDeps

allTables :: PgVersion -> Bool -> SQL.Statement [Schema] TablesMap
allTables pgVer =
  SQL.Statement sql (arrayParam HE.text) decodeTables
  where
    sql = tablesSqlQuery True pgVer

-- | Gets tables with their PK cols
tablesSqlQuery :: Bool -> PgVersion -> SqlQuery
tablesSqlQuery getAll pgVer =
  -- the tbl_constraints/key_col_usage CTEs are based on the standard "information_schema.table_constraints"/"information_schema.key_column_usage" views,
  -- we cannot use those directly as they include the following privilege filter:
  -- (pg_has_role(ss.relowner, 'USAGE'::text) OR has_column_privilege(ss.roid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES'::text));
  [q|
  WITH
  columns AS (
      SELECT
          nc.nspname::name AS table_schema,
          c.relname::name AS table_name,
          a.attname::name AS column_name,
          d.description AS description,
          pg_get_expr(ad.adbin, ad.adrelid)::text AS column_default,
          not (a.attnotnull OR t.typtype = 'd' AND t.typnotnull) AS is_nullable,
              CASE
                  WHEN t.typtype = 'd' THEN
                  CASE
                      WHEN bt.typelem <> 0::oid AND bt.typlen = (-1) THEN 'ARRAY'::text
                      WHEN nbt.nspname = 'pg_catalog'::name THEN format_type(t.typbasetype, NULL::integer)
                      ELSE format_type(a.atttypid, a.atttypmod)
                  END
                  ELSE
                  CASE
                      WHEN t.typelem <> 0::oid AND t.typlen = (-1) THEN 'ARRAY'::text
                      WHEN nt.nspname = 'pg_catalog'::name THEN format_type(a.atttypid, NULL::integer)
                      ELSE format_type(a.atttypid, a.atttypmod)
                  END
              END::text AS data_type,
          information_schema._pg_char_max_length(
              information_schema._pg_truetypid(a.*, t.*),
              information_schema._pg_truetypmod(a.*, t.*)
          )::integer AS character_maximum_length,
          COALESCE(bt.typname, t.typname)::name AS udt_name,
          a.attnum::integer AS position
      FROM pg_attribute a
          LEFT JOIN pg_description AS d
              ON d.objoid = a.attrelid and d.objsubid = a.attnum
          LEFT JOIN pg_attrdef ad
              ON a.attrelid = ad.adrelid AND a.attnum = ad.adnum
          JOIN (pg_class c JOIN pg_namespace nc ON c.relnamespace = nc.oid)
              ON a.attrelid = c.oid
          JOIN (pg_type t JOIN pg_namespace nt ON t.typnamespace = nt.oid)
              ON a.atttypid = t.oid
          LEFT JOIN (pg_type bt JOIN pg_namespace nbt ON bt.typnamespace = nbt.oid)
              ON t.typtype = 'd' AND t.typbasetype = bt.oid
          LEFT JOIN (pg_collation co JOIN pg_namespace nco ON co.collnamespace = nco.oid)
              ON a.attcollation = co.oid AND (nco.nspname <> 'pg_catalog'::name OR co.collname <> 'default'::name)
      WHERE
          NOT pg_is_other_temp_schema(nc.oid)
          AND a.attnum > 0
          AND NOT a.attisdropped
          AND c.relkind in ('r', 'v', 'f', 'm', 'p')
          AND nc.nspname = ANY($1)
  ),
  columns_agg AS (
    SELECT DISTINCT
        info.table_schema AS table_schema,
        info.table_name AS table_name,
        array_agg(row(
          info.column_name,
          info.description,
          info.is_nullable::boolean,
          info.data_type,
          info.character_maximum_length,
          info.column_default,
          coalesce(enum_info.vals, '{}')) order by info.position) as columns
    FROM columns info
    LEFT OUTER JOIN (
        SELECT
            n.nspname AS s,
            t.typname AS n,
            array_agg(e.enumlabel ORDER BY e.enumsortorder) AS vals
        FROM pg_type t
        JOIN pg_enum e ON t.oid = e.enumtypid
        JOIN pg_namespace n ON n.oid = t.typnamespace
        GROUP BY s,n
    ) AS enum_info ON info.udt_name = enum_info.n
    WHERE info.table_schema NOT IN ('pg_catalog', 'information_schema')
    GROUP BY info.table_schema, info.table_name
  ),
  tbl_constraints AS (
      SELECT
          c.conname::name AS constraint_name,
          nr.nspname::name AS table_schema,
          r.relname::name AS table_name
      FROM pg_namespace nc
      JOIN pg_constraint c ON nc.oid = c.connamespace
      JOIN pg_class r ON c.conrelid = r.oid
      JOIN pg_namespace nr ON nr.oid = r.relnamespace
      WHERE
        r.relkind IN ('r', 'p')
        AND NOT pg_is_other_temp_schema(nr.oid)
        AND c.contype = 'p'
  ),
  key_col_usage AS (
      SELECT
          ss.conname::name AS constraint_name,
          ss.nr_nspname::name AS table_schema,
          ss.relname::name AS table_name,
          a.attname::name AS column_name,
          (ss.x).n::integer AS ordinal_position,
          CASE
              WHEN ss.contype = 'f' THEN information_schema._pg_index_position(ss.conindid, ss.confkey[(ss.x).n])
              ELSE NULL::integer
          END::integer AS position_in_unique_constraint
      FROM pg_attribute a
      JOIN (
        SELECT r.oid AS roid,
          r.relname,
          r.relowner,
          nc.nspname AS nc_nspname,
          nr.nspname AS nr_nspname,
          c.oid AS coid,
          c.conname,
          c.contype,
          c.conindid,
          c.confkey,
          information_schema._pg_expandarray(c.conkey) AS x
        FROM pg_namespace nr
        JOIN pg_class r
          ON nr.oid = r.relnamespace
        JOIN pg_constraint c
          ON r.oid = c.conrelid
        JOIN pg_namespace nc
          ON c.connamespace = nc.oid
        WHERE
          c.contype in ('p', 'u')
          AND r.relkind IN ('r', 'p')
          AND NOT pg_is_other_temp_schema(nr.oid)
      ) ss ON a.attrelid = ss.roid AND a.attnum = (ss.x).x
      WHERE
        NOT a.attisdropped
  ),
  tbl_pk_cols AS (
    SELECT
        key_col_usage.table_schema,
        key_col_usage.table_name,
        array_agg(key_col_usage.column_name) as pk_cols
    FROM
        tbl_constraints
    JOIN
        key_col_usage
    ON
        key_col_usage.table_name = tbl_constraints.table_name AND
        key_col_usage.table_schema = tbl_constraints.table_schema AND
        key_col_usage.constraint_name = tbl_constraints.constraint_name
    WHERE
        key_col_usage.table_schema NOT IN ('pg_catalog', 'information_schema')
    GROUP BY key_col_usage.table_schema, key_col_usage.table_name
  )
  SELECT
    n.nspname AS table_schema,
    c.relname AS table_name,
    d.description AS table_description,
    c.relkind IN ('v','m') as is_view,
    (
      c.relkind IN ('r','p')
      OR (
        c.relkind in ('v','f')
        -- The function `pg_relation_is_updateable` returns a bitmask where 8
        -- corresponds to `1 << CMD_INSERT` in the PostgreSQL source code, i.e.
        -- it's possible to insert into the relation.
        AND (pg_relation_is_updatable(c.oid::regclass, TRUE) & 8) = 8
      )
    ) AS insertable,
    (
      c.relkind IN ('r','p')
      OR (
        c.relkind in ('v','f')
        -- CMD_UPDATE
        AND (pg_relation_is_updatable(c.oid::regclass, TRUE) & 4) = 4
      )
    ) AS updatable,
    (
      c.relkind IN ('r','p')
      OR (
        c.relkind in ('v','f')
        -- CMD_DELETE
        AND (pg_relation_is_updatable(c.oid::regclass, TRUE) & 16) = 16
      )
    ) AS deletable,
    coalesce(tpks.pk_cols, '{}') as pk_cols,
    coalesce(cols_agg.columns, '{}') as columns
  FROM pg_class c
  JOIN pg_namespace n ON n.oid = c.relnamespace
  LEFT JOIN pg_description d on d.objoid = c.oid and d.objsubid = 0
  LEFT JOIN tbl_pk_cols tpks ON n.nspname = tpks.table_schema AND c.relname = tpks.table_name
  LEFT JOIN columns_agg cols_agg ON n.nspname = cols_agg.table_schema AND c.relname = cols_agg.table_name
  WHERE c.relkind IN ('v','r','m','f','p')
  AND n.nspname NOT IN ('pg_catalog', 'information_schema') |] <>
  relIsPartition <>
  fltTables <>
  "ORDER BY table_schema, table_name"
  where
    fltTables = if getAll then mempty else [q|
      AND n.nspname = ANY($1)
      AND (
        pg_has_role(c.relowner, 'USAGE')
        or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
        or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
      )|]
    relIsPartition = if pgVer >= pgVersion100 then " AND not c.relispartition " else mempty

allM2ORels :: PgVersion -> Bool -> SQL.Statement () [Relationship]
allM2ORels pgVer =
  SQL.Statement sql HE.noParams decodeRels
 where
  sql = [q|
    SELECT ns1.nspname AS table_schema,
           tab.relname AS table_name,
           ns2.nspname AS foreign_table_schema,
           other.relname AS foreign_table_name,
           (ns1.nspname, tab.relname) = (ns2.nspname, other.relname) AS is_self,
           conname     AS constraint_name,
           column_info.cols AS columns
    FROM pg_constraint,
    LATERAL (
      SELECT array_agg(row(cols.attname, refs.attname) order by cols.attnum) AS cols
      FROM ( SELECT unnest(conkey) AS col, unnest(confkey) AS ref) k,
      LATERAL (SELECT * FROM pg_attribute WHERE attrelid = conrelid AND attnum = col) AS cols,
      LATERAL (SELECT * FROM pg_attribute WHERE attrelid = confrelid AND attnum = ref) AS refs) AS column_info,
    LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = connamespace) AS ns1,
    LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = conrelid) AS tab,
    LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = confrelid) AS other,
    LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = other.relnamespace) AS ns2
    WHERE contype = 'f'|] <>
    (if pgVer >= pgVersion110
      then " and conparentid = 0 "
      else mempty) <>
    "ORDER BY conrelid, conname"

-- | Returns all the views' primary keys and foreign keys dependencies
allViewsKeyDependencies :: Bool -> SQL.Statement ([Schema], [Schema]) [ViewKeyDependency]
allViewsKeyDependencies =
  SQL.Statement sql (contrazip2 (arrayParam HE.text) (arrayParam HE.text)) decodeViewKeyDeps
  -- query explanation at:
  --  * rationale: https://gist.github.com/wolfgangwalther/5425d64e7b0d20aad71f6f68474d9f19
  --  * json transformation: https://gist.github.com/wolfgangwalther/3a8939da680c24ad767e93ad2c183089
  where
    sql = [q|
      with recursive
      pks_fks as (
        -- pk + fk referencing col
        select
          contype,
          conname,
          conrelid as resorigtbl,
          unnest(conkey) as resorigcol
        from pg_constraint
        where contype IN ('p', 'f')
        union
        -- fk referenced col
        select
          concat(contype, '_ref') as contype,
          conname,
          confrelid,
          unnest(confkey)
        from pg_constraint
        where contype='f'
      ),
      views as (
        select
          c.oid       as view_id,
          n.nspname   as view_schema,
          c.relname   as view_name,
          r.ev_action as view_definition
        from pg_class c
        join pg_namespace n on n.oid = c.relnamespace
        join pg_rewrite r on r.ev_class = c.oid
        where c.relkind in ('v', 'm') and n.nspname = ANY($1 || $2)
      ),
      transform_json as (
        select
          view_id, view_schema, view_name,
          -- the following formatting is without indentation on purpose
          -- to allow simple diffs, with less whitespace noise
          replace(
            replace(
            replace(
            replace(
            replace(
            replace(
            replace(
            regexp_replace(
            replace(
            replace(
            replace(
            replace(
            replace(
            replace(
            replace(
            replace(
            replace(
            replace(
            replace(
              view_definition::text,
            -- This conversion to json is heavily optimized for performance.
            -- The general idea is to use as few regexp_replace() calls as possible.
            -- Simple replace() is a lot faster, so we jump through some hoops
            -- to be able to use regexp_replace() only once.
            -- This has been tested against a huge schema with 250+ different views.
            -- The unit tests do NOT reflect all possible inputs. Be careful when changing this!
            -- -----------------------------------------------
            -- pattern           | replacement         | flags
            -- -----------------------------------------------
            -- `<>` in pg_node_tree is the same as `null` in JSON, but due to very poor performance of json_typeof
            -- we need to make this an empty array here to prevent json_array_elements from throwing an error
            -- when the targetList is null.
            -- We'll need to put it first, to make the node protection below work for node lists that start with
            -- null: `(<> ...`, too. This is the case for coldefexprs, when the first column does not have a default value.
               '<>'              , '()'
            -- `,` is not part of the pg_node_tree format, but used in the regex.
            -- This removes all `,` that might be part of column names.
            ), ','               , ''
            -- The same applies for `{` and `}`, although those are used a lot in pg_node_tree.
            -- We remove the escaped ones, which might be part of column names again.
            ), E'\\{'            , ''
            ), E'\\}'            , ''
            -- The fields we need are formatted as json manually to protect them from the regex.
            ), ' :targetList '   , ',"targetList":'
            ), ' :resno '        , ',"resno":'
            ), ' :resorigtbl '   , ',"resorigtbl":'
            ), ' :resorigcol '   , ',"resorigcol":'
            -- Make the regex also match the node type, e.g. `{QUERY ...`, to remove it in one pass.
            ), '{'               , '{ :'
            -- Protect node lists, which start with `({` or `((` from the greedy regex.
            -- The extra `{` is removed again later.
            ), '(('              , '{(('
            ), '({'              , '{({'
            -- This regex removes all unused fields to avoid the need to format all of them correctly.
            -- This leads to a smaller json result as well.
            -- Removal stops at `,` for used fields (see above) and `}` for the end of the current node.
            -- Nesting can't be parsed correctly with a regex, so we stop at `{` as well and
            -- add an empty key for the followig node.
            ), ' :[^}{,]+'       , ',"":'              , 'g'
            -- For performance, the regex also added those empty keys when hitting a `,` or `}`.
            -- Those are removed next.
            ), ',"":}'           , '}'
            ), ',"":,'           , ','
            -- This reverses the "node list protection" from above.
            ), '{('              , '('
            -- Every key above has been added with a `,` so far. The first key in an object doesn't need it.
            ), '{,'              , '{'
            -- pg_node_tree has `()` around lists, but JSON uses `[]`
            ), '('               , '['
            ), ')'               , ']'
            -- pg_node_tree has ` ` between list items, but JSON uses `,`
            ), ' '             , ','
          )::json as view_definition
        from views
      ),
      target_entries as(
        select
          view_id, view_schema, view_name,
          json_array_elements(view_definition->0->'targetList') as entry
        from transform_json
      ),
      results as(
        select
          view_id, view_schema, view_name,
          (entry->>'resno')::int as view_column,
          (entry->>'resorigtbl')::oid as resorigtbl,
          (entry->>'resorigcol')::int as resorigcol
        from target_entries
      ),
      recursion as(
        select r.*
        from results r
        where view_schema = ANY ($1)
        union all
        select
          view.view_id,
          view.view_schema,
          view.view_name,
          view.view_column,
          tab.resorigtbl,
          tab.resorigcol
        from recursion view
        join results tab on view.resorigtbl=tab.view_id and view.resorigcol=tab.view_column
      )
      select
        sch.nspname as table_schema,
        tbl.relname as table_name,
        rec.view_schema,
        rec.view_name,
        pks_fks.conname as constraint_name,
        pks_fks.contype as constraint_type,
        array_agg(row(col.attname, vcol.attname) order by col.attnum) as column_dependencies
      from recursion rec
      join pg_class tbl on tbl.oid = rec.resorigtbl
      join pg_attribute col on col.attrelid = tbl.oid and col.attnum = rec.resorigcol
      join pg_attribute vcol on vcol.attrelid = rec.view_id and vcol.attnum = rec.view_column
      join pg_namespace sch on sch.oid = tbl.relnamespace
      join pks_fks using (resorigtbl, resorigcol)
      group by sch.nspname, tbl.relname,  rec.view_schema, rec.view_name, pks_fks.conname, pks_fks.contype
      |]

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable

arrayParam :: HE.Value a -> HE.Params [a]
arrayParam = param . HE.foldableArray . HE.nonNullable

compositeArrayColumn :: HD.Composite a -> HD.Row [a]
compositeArrayColumn = arrayColumn . HD.composite

compositeField :: HD.Value a -> HD.Composite a
compositeField = HD.field . HD.nonNullable

nullableCompositeField :: HD.Value a -> HD.Composite (Maybe a)
nullableCompositeField = HD.field . HD.nullable

compositeFieldArray :: HD.Value a -> HD.Composite [a]
compositeFieldArray = HD.field . HD.nonNullable . HD.listArray . HD.nonNullable

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

nullableColumn :: HD.Value a -> HD.Row (Maybe a)
nullableColumn = HD.column . HD.nullable

arrayColumn :: HD.Value a -> HD.Row [a]
arrayColumn = column . HD.listArray . HD.nonNullable
