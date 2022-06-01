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
  , tableCols
  , tablePKCols
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import qualified Hasql.Decoders      as HD
import qualified Hasql.Encoders      as HE
import qualified Hasql.Statement     as SQL
import qualified Hasql.Transaction   as SQL

import Contravariant.Extras          (contrazip2)
import Data.Set                      as S (fromList)
import Data.Text                     (split)
import Text.InterpolatedString.Perl6 (q)

import PostgREST.Config.PgVersion         (PgVersion, pgVersion100)
import PostgREST.DbStructure.Identifiers  (QualifiedIdentifier (..),
                                           Schema, TableName)
import PostgREST.DbStructure.Proc         (PgType (..),
                                           ProcDescription (..),
                                           ProcParam (..),
                                           ProcVolatility (..),
                                           ProcsMap, RetType (..))
import PostgREST.DbStructure.Relationship (Cardinality (..),
                                           Junction (..),
                                           PrimaryKey (..),
                                           Relationship (..))
import PostgREST.DbStructure.Table        (Column (..), Table (..))

import Protolude
import Protolude.Unsafe (unsafeHead)


data DbStructure = DbStructure
  { dbTables        :: [Table]
  , dbColumns       :: [Column]
  , dbRelationships :: [Relationship]
  , dbPrimaryKeys   :: [PrimaryKey]
  , dbProcs         :: ProcsMap
  }
  deriving (Generic, JSON.ToJSON)

-- TODO Table could hold references to all its Columns
tableCols :: DbStructure -> Schema -> TableName -> [Column]
tableCols dbs tSchema tName = filter (\Column{colTable=Table{tableSchema=s, tableName=t}} -> s==tSchema && t==tName) $ dbColumns dbs

-- TODO Table could hold references to all its PrimaryKeys
tablePKCols :: DbStructure -> Schema -> TableName -> [Text]
tablePKCols dbs tSchema tName =  pkName <$> filter (\pk -> tSchema == (tableSchema . pkTable) pk && tName == (tableName . pkTable) pk) (dbPrimaryKeys dbs)

-- | The source table column a view column refers to
type SourceColumn = (Column, ViewColumn)
type ViewColumn = Column

-- | A SQL query that can be executed independently
type SqlQuery = ByteString

queryDbStructure :: [Schema] -> [Schema] -> PgVersion -> Bool -> SQL.Transaction DbStructure
queryDbStructure schemas extraSearchPath pgVer prepared = do
  SQL.sql "set local schema ''" -- This voids the search path. The following queries need this for getting the fully qualified name(schema.name) of every db object
  tabs    <- SQL.statement mempty $ allTables pgVer prepared
  cols    <- SQL.statement schemas $ allColumns tabs prepared
  srcCols <- SQL.statement (schemas, extraSearchPath) $ pfkSourceColumns cols prepared
  m2oRels <- SQL.statement mempty $ allM2ORels tabs cols prepared
  keys    <- SQL.statement mempty $ allPrimaryKeys tabs prepared
  procs   <- SQL.statement schemas $ allProcs prepared

  let rels = addO2MRels . addM2MRels $ addViewM2ORels srcCols m2oRels
      keys' = addViewPrimaryKeys srcCols keys

  return $ removeInternal schemas $ DbStructure {
      dbTables = tabs
    , dbColumns = cols
    , dbRelationships = rels
    , dbPrimaryKeys = keys'
    , dbProcs = procs
    }

-- | Remove db objects that belong to an internal schema(not exposed through the API) from the DbStructure.
removeInternal :: [Schema] -> DbStructure -> DbStructure
removeInternal schemas dbStruct =
  DbStructure {
      dbTables        = filter (\x -> tableSchema x `elem` schemas) $ dbTables dbStruct
    , dbColumns       = filter (\x -> tableSchema (colTable x) `elem` schemas) (dbColumns dbStruct)
    , dbRelationships = filter (\x -> tableSchema (relTable x) `elem` schemas &&
                                      tableSchema (relForeignTable x) `elem` schemas &&
                                      not (hasInternalJunction x)) $ dbRelationships dbStruct
    , dbPrimaryKeys   = filter (\x -> tableSchema (pkTable x) `elem` schemas) $ dbPrimaryKeys dbStruct
    , dbProcs         = dbProcs dbStruct -- procs are only obtained from the exposed schemas, no need to filter them.
    }
  where
    hasInternalJunction rel = case relCardinality rel of
      M2M Junction{junTable} -> tableSchema junTable `notElem` schemas
      _                      -> False

decodeTables :: HD.Result [Table]
decodeTables =
  HD.rowList tblRow
 where
  tblRow = Table <$> column HD.text
                 <*> column HD.text
                 <*> nullableColumn HD.text
                 <*> column HD.bool
                 <*> column HD.bool
                 <*> column HD.bool

decodeColumns :: [Table] -> HD.Result [Column]
decodeColumns tables =
  mapMaybe (columnFromRow tables) <$> HD.rowList colRow
 where
  colRow =
    (,,,,,,,,)
      <$> column HD.text
      <*> column HD.text
      <*> column HD.text
      <*> nullableColumn HD.text
      <*> column HD.bool
      <*> column HD.text
      <*> nullableColumn HD.int4
      <*> nullableColumn HD.text
      <*> nullableColumn HD.text

decodeRels :: [Table] -> [Column] -> HD.Result [Relationship]
decodeRels tables cols =
  mapMaybe (relFromRow tables cols) <$> HD.rowList relRow
 where
  relRow = (,,,,,,)
    <$> column HD.text
    <*> column HD.text
    <*> column HD.text
    <*> arrayColumn HD.text
    <*> column HD.text
    <*> column HD.text
    <*> arrayColumn HD.text

decodePks :: [Table] -> HD.Result [PrimaryKey]
decodePks tables =
  mapMaybe (pkFromRow tables) <$> HD.rowList pkRow
 where
  pkRow = (,,) <$> column HD.text <*> column HD.text <*> column HD.text

decodeSourceColumns :: [Column] -> HD.Result [SourceColumn]
decodeSourceColumns cols =
  mapMaybe (sourceColumnFromRow cols) <$> HD.rowList srcColRow
 where
  srcColRow = (,,,,,)
    <$> column HD.text <*> column HD.text
    <*> column HD.text <*> column HD.text
    <*> column HD.text <*> column HD.text

sourceColumnFromRow :: [Column] -> (Text,Text,Text,Text,Text,Text) -> Maybe SourceColumn
sourceColumnFromRow allCols (s1,t1,c1,s2,t2,c2) = (,) <$> col1 <*> col2
  where
    col1 = findCol s1 t1 c1
    col2 = findCol s2 t2 c2
    findCol s t c = find (\col -> (tableSchema . colTable) col == s && (tableName . colTable) col == t && colName col == c) allCols

decodeProcs :: HD.Result ProcsMap
decodeProcs =
  -- Duplicate rows for a function means they're overloaded, order these by least args according to ProcDescription Ord instance
  map sort . M.fromListWith (++) . map ((\(x,y) -> (x, [y])) . addKey) <$> HD.rowList procRow
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
                  <*> column HD.bool)
              <*> (parseVolatility <$> column HD.char)
              <*> column HD.bool

    addKey :: ProcDescription -> (QualifiedIdentifier, ProcDescription)
    addKey pd = (QualifiedIdentifier (pdSchema pd) (pdName pd), pd)

    parseRetType :: Text -> Text -> Bool -> Bool -> RetType
    parseRetType schema name isSetOf isComposite
      | isSetOf   = SetOf pgType
      | otherwise = Single pgType
      where
        qi = QualifiedIdentifier schema name
        pgType
          | isComposite = Composite qi
          | otherwise   = Scalar

    parseVolatility :: Char -> ProcVolatility
    parseVolatility v | v == 'i' = Immutable
                      | v == 's' = Stable
                      | otherwise = Volatile -- only 'v' can happen here

allProcs :: Bool -> SQL.Statement [Schema] ProcsMap
allProcs = SQL.Statement sql (arrayParam HE.text) decodeProcs
  where
    sql = procsSqlQuery <> " WHERE pn.nspname = ANY($1)"

accessibleProcs :: Bool -> SQL.Statement Schema ProcsMap
accessibleProcs = SQL.Statement sql (param HE.text) decodeProcs
  where
    sql = procsSqlQuery <> " WHERE pn.nspname = $1 AND has_function_privilege(p.oid, 'execute')"

procsSqlQuery :: SqlQuery
procsSqlQuery = [q|
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
      ) ORDER BY idx) AS args
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
    p.provolatile,
    p.provariadic > 0 as hasvariadic
  FROM pg_proc p
  LEFT JOIN arguments a ON a.oid = p.oid
  JOIN pg_namespace pn ON pn.oid = p.pronamespace
  JOIN base_types bt ON bt.oid = p.prorettype
  JOIN pg_type t ON t.oid = bt.base
  JOIN pg_namespace tn ON tn.oid = t.typnamespace
  LEFT JOIN pg_class comp ON comp.oid = t.typrelid
  LEFT JOIN pg_catalog.pg_description as d ON d.objoid = p.oid
|]

schemaDescription :: Bool -> SQL.Statement Schema (Maybe Text)
schemaDescription =
    SQL.Statement sql (param HE.text) (join <$> HD.rowMaybe (nullableColumn HD.text))
  where
    sql = [q|
      select
        description
      from
        pg_catalog.pg_namespace n
        left join pg_catalog.pg_description d on d.objoid = n.oid
      where
        n.nspname = $1 |]

accessibleTables :: PgVersion -> Bool -> SQL.Statement Schema [Table]
accessibleTables pgVer =
  SQL.Statement sql (param HE.text) decodeTables
 where
  sql = [q|
    select
      n.nspname as table_schema,
      relname as table_name,
      d.description as table_description,
      (
        c.relkind IN ('r','p')
        OR (
          c.relkind IN ('v','f')
          -- CMD_INSERT - see allTables query below for explanation
          AND (pg_relation_is_updatable(c.oid::regclass, TRUE) & 8) = 8
        )
      ) AS insertable,
      (
        c.relkind IN ('r','p')
        OR (
          c.relkind IN ('v','f')
          -- CMD_UPDATE
          AND (pg_relation_is_updatable(c.oid::regclass, TRUE) & 4) = 4
        )
      ) as updatable,
      (
        c.relkind IN ('r','p')
        OR (
          c.relkind IN ('v','f')
          -- CMD_DELETE
          AND (pg_relation_is_updatable(c.oid::regclass, TRUE) & 16) = 16
        )
      ) as deletable
    from
      pg_class c
      join pg_namespace n on n.oid = c.relnamespace
      left join pg_catalog.pg_description as d on d.objoid = c.oid and d.objsubid = 0
    where
      c.relkind in ('v','r','m','f','p')
      and n.nspname = $1 |]
      <> relIsNotPartition pgVer <> [q|
      and (
        pg_has_role(c.relowner, 'USAGE')
        or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
        or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
      )
    order by relname |]

{-
Adds Views M2O Relationships based on SourceColumns found, the logic is as follows:

Having a Relationship{relTable=t1, relColumns=[c1], relFTable=t2, relFColumns=[c2], relCardinality=M2O} represented by:

t1.c1------t2.c2

When only having a t1_view.c1 source column, we need to add a View-Table M2O Relationship

         t1.c1----t2.c2         t1.c1----------t2.c2
                         ->            ________/
                                      /
      t1_view.c1             t1_view.c1


When only having a t2_view.c2 source column, we need to add a Table-View M2O Relationship

         t1.c1----t2.c2               t1.c1----------t2.c2
                               ->          \________
                                                    \
                    t2_view.c2                      t2_view.c1

When having t1_view.c1 and a t2_view.c2 source columns, we need to add a View-View M2O Relationship in addition to the prior

         t1.c1----t2.c2               t1.c1----------t2.c2
                               ->          \________/
                                           /        \
    t1_view.c1     t2_view.c2     t1_view.c1-------t2_view.c1

The logic for composite pks is similar just need to make sure all the Relationship columns have source columns.
-}
addViewM2ORels :: [SourceColumn] -> [Relationship] -> [Relationship]
addViewM2ORels allSrcCols = concatMap (\rel@Relationship{..} -> rel :
  let
    srcColsGroupedByView :: [Column] -> [[SourceColumn]]
    srcColsGroupedByView relCols = L.groupBy (\(_, viewCol1) (_, viewCol2) -> colTable viewCol1 == colTable viewCol2) $
                   filter (\(c, _) -> c `elem` relCols) allSrcCols
    relSrcCols = srcColsGroupedByView relColumns
    relFSrcCols = srcColsGroupedByView relForeignColumns
    getView :: [SourceColumn] -> Table
    getView = colTable . snd . unsafeHead
    srcCols `allSrcColsOf` cols = S.fromList (fst <$> srcCols) == S.fromList cols
    -- Relationship is dependent on the order of relColumns and relFColumns to get the join conditions right in the generated query.
    -- So we need to change the order of the SourceColumns to match the relColumns
    -- TODO: This could be avoided if the Relationship type is improved with a structure that maintains the association of relColumns and relFColumns
    srcCols `sortAccordingTo` cols = sortOn (\(k, _) -> L.lookup k $ zip cols [0::Int ..]) srcCols

    viewTableM2O =
      [ Relationship
          (getView srcCols) (snd <$> srcCols `sortAccordingTo` relColumns)
          relForeignTable relForeignColumns relCardinality
      | srcCols <- relSrcCols, srcCols `allSrcColsOf` relColumns ]

    tableViewM2O =
      [ Relationship
          relTable relColumns
          (getView fSrcCols) (snd <$> fSrcCols `sortAccordingTo` relForeignColumns)
          relCardinality
      | fSrcCols <- relFSrcCols, fSrcCols `allSrcColsOf` relForeignColumns ]

    viewViewM2O =
      [ Relationship
          (getView srcCols) (snd <$> srcCols `sortAccordingTo` relColumns)
          (getView fSrcCols) (snd <$> fSrcCols `sortAccordingTo` relForeignColumns)
          relCardinality
      | srcCols  <- relSrcCols, srcCols `allSrcColsOf` relColumns
      , fSrcCols <- relFSrcCols, fSrcCols `allSrcColsOf` relForeignColumns ]

  in viewTableM2O ++ tableViewM2O ++ viewViewM2O)

addO2MRels :: [Relationship] -> [Relationship]
addO2MRels rels = rels ++ [ Relationship ft fc t c (O2M cons)
                          | Relationship t c ft fc (M2O cons) <- rels ]

addM2MRels :: [Relationship] -> [Relationship]
addM2MRels rels = rels ++ [ Relationship t c ft fc (M2M $ Junction jt1 cons1 jc1 cons2 jc2)
                          | Relationship jt1 jc1 t c (M2O cons1) <- rels
                          , Relationship jt2 jc2 ft fc (M2O cons2) <- rels
                          , jt1 == jt2
                          , cons1 /= cons2]

addViewPrimaryKeys :: [SourceColumn] -> [PrimaryKey] -> [PrimaryKey]
addViewPrimaryKeys srcCols = concatMap (\pk ->
  let viewPks = (\(_, viewCol) -> PrimaryKey{pkTable=colTable viewCol, pkName=colName viewCol}) <$>
                filter (\(col, _) -> colTable col == pkTable pk && colName col == pkName pk) srcCols in
  pk : viewPks)

allTables :: PgVersion -> Bool -> SQL.Statement () [Table]
allTables pgVer =
  SQL.Statement sql HE.noParams decodeTables
 where
  sql = [q|
    SELECT
      n.nspname AS table_schema,
      c.relname AS table_name,
      d.description AS table_description,
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
      ) AS deletable
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace
    LEFT JOIN pg_catalog.pg_description as d on d.objoid = c.oid and d.objsubid = 0
    WHERE c.relkind IN ('v','r','m','f','p')
      AND n.nspname NOT IN ('pg_catalog', 'information_schema') |]
      <> relIsNotPartition pgVer <> [q|
    ORDER BY table_schema, table_name |]

relIsNotPartition :: PgVersion -> SqlQuery
relIsNotPartition pgVer = if pgVer >= pgVersion100 then " AND not c.relispartition " else mempty

allColumns :: [Table] -> Bool -> SQL.Statement [Schema] [Column]
allColumns tabs =
 SQL.Statement sql (arrayParam HE.text) (decodeColumns tabs)
 where
  sql = [q|
    SELECT DISTINCT
        info.table_schema AS schema,
        info.table_name AS table_name,
        info.column_name AS name,
        info.description AS description,
        info.is_nullable::boolean AS nullable,
        info.data_type AS col_type,
        info.character_maximum_length AS max_len,
        info.column_default AS default_value,
        array_to_string(enum_info.vals, ',') AS enum,
        info.position
    FROM (
        -- CTE based on pg_catalog to get PRIMARY/FOREIGN key and UNIQUE columns outside api schema
        WITH key_columns AS (
             SELECT
               r.oid AS r_oid,
               c.oid AS c_oid,
               n.nspname,
               c.relname,
               r.conname,
               r.contype,
               unnest(r.conkey) AS conkey
             FROM
               pg_catalog.pg_constraint r,
               pg_catalog.pg_class c,
               pg_catalog.pg_namespace n
             WHERE
               r.contype IN ('f', 'p', 'u')
               AND c.relkind IN ('r', 'v', 'f', 'm', 'p')
               AND r.conrelid = c.oid
               AND c.relnamespace = n.oid
               AND n.nspname <> ANY (ARRAY['pg_catalog', 'information_schema'] || $1)
        ),
        /*
        -- CTE based on information_schema.columns
        -- changed:
        -- remove the owner filter
        -- limit columns to the ones in the api schema or PK/FK columns
        */
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
                LEFT JOIN key_columns kc
                    ON kc.conkey = a.attnum AND kc.c_oid = a.attrelid
                LEFT JOIN pg_catalog.pg_description AS d
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
                -- Filter only columns that are FK/PK or in the api schema:
                AND (nc.nspname = ANY ($1) OR kc.r_oid IS NOT NULL)
        )
        SELECT
            table_schema,
            table_name,
            column_name,
            description,
            is_nullable,
            data_type,
            character_maximum_length,
            column_default,
            udt_name,
            position
        FROM columns
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
    ORDER BY schema, position |]

columnFromRow :: [Table] ->
                 (Text,        Text,        Text,
                  Maybe Text,  Bool,        Text,
                  Maybe Int32, Maybe Text,  Maybe Text)
                 -> Maybe Column
columnFromRow tabs (s, t, n, desc, nul, typ, l, d, e) = buildColumn <$> table
  where
    buildColumn tbl = Column tbl n desc nul typ l d (parseEnum e)
    table = find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs
    parseEnum :: Maybe Text -> [Text]
    parseEnum = maybe [] (split (==','))

allM2ORels :: [Table] -> [Column] -> Bool -> SQL.Statement () [Relationship]
allM2ORels tabs cols =
  SQL.Statement sql HE.noParams (decodeRels tabs cols)
 where
  sql = [q|
    SELECT ns1.nspname AS table_schema,
           tab.relname AS table_name,
           conname     AS constraint_name,
           column_info.cols AS columns,
           ns2.nspname AS foreign_table_schema,
           other.relname AS foreign_table_name,
           column_info.refs AS foreign_columns
    FROM pg_constraint,
    LATERAL (
      SELECT array_agg(cols.attname) AS cols,
                    array_agg(cols.attnum)  AS nums,
                    array_agg(refs.attname) AS refs
      FROM ( SELECT unnest(conkey) AS col, unnest(confkey) AS ref) k,
      LATERAL (SELECT * FROM pg_attribute WHERE attrelid = conrelid AND attnum = col) AS cols,
      LATERAL (SELECT * FROM pg_attribute WHERE attrelid = confrelid AND attnum = ref) AS refs) AS column_info,
    LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = connamespace) AS ns1,
    LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = conrelid) AS tab,
    LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = confrelid) AS other,
    LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = other.relnamespace) AS ns2
    WHERE confrelid != 0
    ORDER BY (conrelid, column_info.nums) |]

relFromRow :: [Table] -> [Column] -> (Text, Text, Text, [Text], Text, Text, [Text]) -> Maybe Relationship
relFromRow allTabs allCols (rs, rt, cn, rcs, frs, frt, frcs) =
  Relationship <$> table <*> cols <*> tableF <*> colsF <*> pure (M2O cn)
  where
    findTable s t = find (\tbl -> tableSchema tbl == s && tableName tbl == t) allTabs
    findCol s t c = find (\col -> tableSchema (colTable col) == s && tableName (colTable col) == t && colName col == c) allCols
    table  = findTable rs rt
    tableF = findTable frs frt
    cols  = mapM (findCol rs rt) rcs
    colsF = mapM (findCol frs frt) frcs

allPrimaryKeys :: [Table] -> Bool -> SQL.Statement () [PrimaryKey]
allPrimaryKeys tabs =
  SQL.Statement sql HE.noParams (decodePks tabs)
 where
  sql = [q|
    -- CTE to replace information_schema.table_constraints to remove owner limit
    WITH tc AS (
        SELECT
            c.conname::name AS constraint_name,
            nr.nspname::name AS table_schema,
            r.relname::name AS table_name
        FROM pg_namespace nc,
            pg_namespace nr,
            pg_constraint c,
            pg_class r
        WHERE
            nc.oid = c.connamespace
            AND nr.oid = r.relnamespace
            AND c.conrelid = r.oid
            AND r.relkind IN ('r', 'p')
            AND NOT pg_is_other_temp_schema(nr.oid)
            AND c.contype = 'p'
    ),
    -- CTE to replace information_schema.key_column_usage to remove owner limit
    kc AS (
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
        FROM pg_attribute a,
            ( SELECT r.oid AS roid,
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
               FROM pg_namespace nr,
                pg_class r,
                pg_namespace nc,
                pg_constraint c
              WHERE
                nr.oid = r.relnamespace
                AND r.oid = c.conrelid
                AND nc.oid = c.connamespace
                AND c.contype in ('p', 'u', 'f')
                AND r.relkind IN ('r', 'p')
                AND NOT pg_is_other_temp_schema(nr.oid)
            ) ss
        WHERE
          ss.roid = a.attrelid
          AND a.attnum = (ss.x).x
          AND NOT a.attisdropped
    )
    SELECT
        kc.table_schema,
        kc.table_name,
        kc.column_name
    FROM
        tc, kc
    WHERE
        kc.table_name = tc.table_name AND
        kc.table_schema = tc.table_schema AND
        kc.constraint_name = tc.constraint_name AND
        kc.table_schema NOT IN ('pg_catalog', 'information_schema') |]

pkFromRow :: [Table] -> (Schema, Text, Text) -> Maybe PrimaryKey
pkFromRow tabs (s, t, n) = PrimaryKey <$> table <*> pure n
  where table = find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs

-- returns all the primary and foreign key columns which are referenced in views
pfkSourceColumns :: [Column] -> Bool -> SQL.Statement ([Schema], [Schema]) [SourceColumn]
pfkSourceColumns cols =
  SQL.Statement sql (contrazip2 (arrayParam HE.text) (arrayParam HE.text)) (decodeSourceColumns cols)
  -- query explanation at:
  --  * rationale: https://gist.github.com/wolfgangwalther/5425d64e7b0d20aad71f6f68474d9f19
  --  * json transformation: https://gist.github.com/wolfgangwalther/3a8939da680c24ad767e93ad2c183089
  where
    sql = [q|
      with recursive
      pks_fks as (
        -- pk + fk referencing col
        select
          conrelid as resorigtbl,
          unnest(conkey) as resorigcol
        from pg_constraint
        where contype IN ('p', 'f')
        union
        -- fk referenced col
        select
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
        col.attname as table_column_name,
        rec.view_schema,
        rec.view_name,
        vcol.attname as view_column_name
      from recursion rec
      join pg_class tbl on tbl.oid = rec.resorigtbl
      join pg_attribute col on col.attrelid = tbl.oid and col.attnum = rec.resorigcol
      join pg_attribute vcol on vcol.attrelid = rec.view_id and vcol.attnum = rec.view_column
      join pg_namespace sch on sch.oid = tbl.relnamespace
      join pks_fks using (resorigtbl, resorigcol)
      order by view_schema, view_name, view_column_name; |]

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable

arrayParam :: HE.Value a -> HE.Params [a]
arrayParam = param . HE.foldableArray . HE.nonNullable

compositeArrayColumn :: HD.Composite a -> HD.Row [a]
compositeArrayColumn = arrayColumn . HD.composite

compositeField :: HD.Value a -> HD.Composite a
compositeField = HD.field . HD.nonNullable

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

nullableColumn :: HD.Value a -> HD.Row (Maybe a)
nullableColumn = HD.column . HD.nullable

arrayColumn :: HD.Value a -> HD.Row [a]
arrayColumn = column . HD.listArray . HD.nonNullable
