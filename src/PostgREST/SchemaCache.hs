{-|
Module      : PostgREST.SchemaCache
Description : PostgREST schema cache

This module(used to be named DbStructure) contains queries that target PostgreSQL system catalogs, these are used to build the schema cache(SchemaCache).

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

module PostgREST.SchemaCache
  ( SchemaCache(..)
  , querySchemaCache
  , accessibleTables
  , accessibleFuncs
  , schemaDescription
  , showSummary
  ) where

import Control.Monad.Extra (whenJust)

import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.Types           as JSON
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashMap.Strict.InsOrd as HMI
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Statement            as SQL
import qualified Hasql.Transaction          as SQL

import Contravariant.Extras          (contrazip2)
import Text.InterpolatedString.Perl6 (q)

import PostgREST.Config                      (AppConfig (..))
import PostgREST.Config.Database             (TimezoneNames,
                                              pgVersionStatement,
                                              toIsolationLevel)
import PostgREST.Config.PgVersion            (PgVersion, pgVersion100,
                                              pgVersion110,
                                              pgVersion120)
import PostgREST.SchemaCache.Identifiers     (AccessSet, FieldName,
                                              QualifiedIdentifier (..),
                                              RelIdentifier (..),
                                              Schema, isAnyElement)
import PostgREST.SchemaCache.Relationship    (Cardinality (..),
                                              Junction (..),
                                              Relationship (..),
                                              RelationshipsMap)
import PostgREST.SchemaCache.Representations (DataRepresentation (..),
                                              RepresentationsMap)
import PostgREST.SchemaCache.Routine         (FuncVolatility (..),
                                              MediaHandler (..),
                                              MediaHandlerMap,
                                              PgType (..),
                                              RetType (..),
                                              Routine (..),
                                              RoutineMap,
                                              RoutineParam (..))
import PostgREST.SchemaCache.Table           (Column (..), ColumnMap,
                                              Table (..), TablesMap)

import qualified PostgREST.MediaType as MediaType

import Protolude

data SchemaCache = SchemaCache
  { dbTables          :: TablesMap
  , dbRelationships   :: RelationshipsMap
  , dbRoutines        :: RoutineMap
  , dbRepresentations :: RepresentationsMap
  , dbMediaHandlers   :: MediaHandlerMap
  , dbTimezones       :: TimezoneNames
  }

instance JSON.ToJSON SchemaCache where
  toJSON (SchemaCache tabs rels routs reps _ _) = JSON.object [
      "dbTables"          .= JSON.toJSON tabs
    , "dbRelationships"   .= JSON.toJSON rels
    , "dbRoutines"        .= JSON.toJSON routs
    , "dbRepresentations" .= JSON.toJSON reps
    , "dbMediaHandlers"   .= JSON.emptyArray
    , "dbTimezones"       .= JSON.emptyArray
    ]

showSummary :: SchemaCache -> Text
showSummary (SchemaCache tbls rels routs reps mediaHdlrs _) =
  T.intercalate ", "
  [ show (HM.size tbls)       <> " Relations"
  , show (HM.size rels)       <> " Relationships"
  , show (HM.size routs)      <> " Functions"
  , show (HM.size reps)       <> " Domain Representations"
  , show (HM.size mediaHdlrs) <> " Media Type Handlers"
  ]

-- | A view foreign key or primary key dependency detected on its source table
-- Each column of the key could be referenced multiple times in the view, e.g.
--
-- create view projects_view as
-- select
--   id as id_1,
--   id as id_2,
--   id as id_3,
--   name
-- from projects
--
-- In this case, the keyDepCols mapping maps projects.id to all three of the columns:
--
-- [('id', ['id_1', 'id_2', 'id_3'])]
--
-- Depending on key type, we can then choose how to handle this case. Primary keys
-- can arbitrarily choose one of the columns, but for foreign keys we need to create
-- relationships for each possible mutations.
--
-- Previously, we stored a (FieldName, FieldName) tuple only, but then we had no
-- way to make a difference between a multi-column-key and a single-column-key with multiple
-- references in the view. Or even worse in the multi-column-key-multi-reference case...
data ViewKeyDependency = ViewKeyDependency {
  keyDepTable :: QualifiedIdentifier
, keyDepView  :: QualifiedIdentifier
, keyDepCons  :: Text
, keyDepType  :: KeyDep
, keyDepCols  :: [(FieldName, [FieldName])] -- ^ First element is the table column, second is a list of view columns
} deriving (Eq)
data KeyDep
  = PKDep    -- ^ PK dependency
  | FKDep    -- ^ FK dependency
  | FKDepRef -- ^ FK reference dependency
  deriving (Eq)

-- | A SQL query that can be executed independently
type SqlQuery = ByteString


querySchemaCache :: AppConfig -> SQL.Transaction SchemaCache
querySchemaCache AppConfig{..} = do
  SQL.sql "set local schema ''" -- This voids the search path. The following queries need this for getting the fully qualified name(schema.name) of every db object
  pgVer   <- SQL.statement mempty $ pgVersionStatement prepared
  tabs    <- SQL.statement schemas $ allTables pgVer prepared
  keyDeps <- SQL.statement (schemas, configDbExtraSearchPath) $ allViewsKeyDependencies prepared
  m2oRels <- SQL.statement mempty $ allM2OandO2ORels pgVer prepared
  funcs   <- SQL.statement schemas $ allFunctions pgVer prepared
  cRels   <- SQL.statement mempty $ allComputedRels prepared
  reps    <- SQL.statement schemas $ dataRepresentations prepared
  mHdlers <- SQL.statement schemas $ mediaHandlers pgVer prepared
  tzones  <- SQL.statement mempty $ timezones prepared
  _       <-
    let sleepCall = SQL.Statement "select pg_sleep($1)" (param HE.int4) HD.noResult prepared in
    whenJust configInternalSCSleep (`SQL.statement` sleepCall) -- only used for testing

  let tabsWViewsPks = addViewPrimaryKeys tabs keyDeps
      rels          = addInverseRels $ addM2MRels tabsWViewsPks $ addViewM2OAndO2ORels keyDeps m2oRels

  return $ removeInternal schemas $ SchemaCache {
      dbTables = tabsWViewsPks
    , dbRelationships = getOverrideRelationshipsMap rels cRels
    , dbRoutines = funcs
    , dbRepresentations = reps
    , dbMediaHandlers = HM.union mHdlers initialMediaHandlers -- the custom handlers will override the initial ones
    , dbTimezones = tzones
    }
  where
    schemas = toList configDbSchemas
    prepared = configDbPreparedStatements

-- | overrides detected relationships with the computed relationships and gets the RelationshipsMap
getOverrideRelationshipsMap :: [Relationship] -> [Relationship] -> RelationshipsMap
getOverrideRelationshipsMap rels cRels =
  sort <$> deformedRelMap patchedRels
  where
    -- there can only be a single (table_type, func_name) pair in a function definition `test.function(table_type)`, so we use HM.fromList to disallow duplicates
    computedRels  = HM.fromList $ relMapKey <$> cRels
    -- here we override the detected relationships with the user computed relationships, HM.union makes sure computedRels prevail
    patchedRels   = HM.union computedRels (relsMap rels)
    relsMap = HM.fromListWith (++) . fmap relMapKey
    relMapKey rel = case rel of
      Relationship{relTable,relForeignTable} -> ((relTable, relForeignTable), [rel])
      -- we use (relTable, relFunction) as key to override detected relationships with the function name
      ComputedRelationship{relTable,relFunction} -> ((relTable, relFunction), [rel])
    -- Since a relationship is between a table and foreign table, the logical way to index/search is by their table/ftable QualifiedIdentifier
    -- However, because we allow searching a relationship by the columns of the foreign key(using the "column as target" disambiguation) we lose the
    -- ability to index by the foreign table name, so we deform the key. TODO remove once support for "column as target" is gone.
    deformedRelMap = HM.fromListWith (++) . fmap addDeformedRelKey . HM.toList
    addDeformedRelKey ((relT, relFT), rls) = ((relT, qiSchema relFT), rls)

-- | Remove db objects that belong to an internal schema(not exposed through the API) from the SchemaCache.
removeInternal :: [Schema] -> SchemaCache -> SchemaCache
removeInternal schemas dbStruct =
  SchemaCache {
      dbTables          = HM.filterWithKey (\(QualifiedIdentifier sch _) _ -> sch `elem` schemas) $ dbTables dbStruct
    , dbRelationships   = filter (\r -> qiSchema (relForeignTable r) `elem` schemas && not (hasInternalJunction r)) <$>
                          HM.filterWithKey (\(QualifiedIdentifier sch _, _) _ -> sch `elem` schemas ) (dbRelationships dbStruct)
    , dbRoutines        = dbRoutines dbStruct -- procs are only obtained from the exposed schemas, no need to filter them.
    , dbRepresentations = dbRepresentations dbStruct -- no need to filter, not directly exposed through the API
    , dbMediaHandlers   = dbMediaHandlers dbStruct
    , dbTimezones       = dbTimezones dbStruct
    }
  where
    hasInternalJunction ComputedRelationship{} = False
    hasInternalJunction Relationship{relCardinality=card} = case card of
      M2M Junction{junTable} -> qiSchema junTable `notElem` schemas
      _                      -> False

decodeAccessibleIdentifiers :: HD.Result AccessSet
decodeAccessibleIdentifiers =
 S.fromList <$> HD.rowList row
 where
  row = QualifiedIdentifier
    <$> column HD.text
    <*> column HD.text

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
    <*> parseCols (compositeArrayColumn
      (Column
        <$> compositeField HD.text
        <*> nullableCompositeField HD.text
        <*> compositeField HD.bool
        <*> compositeField HD.text
        <*> compositeField HD.text
        <*> nullableCompositeField HD.int4
        <*> nullableCompositeField HD.text
        <*> compositeFieldArray HD.text))


parseCols :: HD.Row [Column] -> HD.Row ColumnMap
parseCols = fmap (HMI.fromList . map (\col@Column{colName} -> (colName, col)))

decodeRels :: HD.Result [Relationship]
decodeRels =
 HD.rowList relRow
 where
  relRow = (\(qi1, qi2, isSelf, constr, cols, isOneToOne) -> Relationship qi1 qi2 isSelf ((if isOneToOne then O2O else M2O) constr cols) False False) <$> row
  row =
    (,,,,,) <$>
    (QualifiedIdentifier <$> column HD.text <*> column HD.text) <*>
    (QualifiedIdentifier <$> column HD.text <*> column HD.text) <*>
    column HD.bool <*>
    column HD.text <*>
    compositeArrayColumn ((,) <$> compositeField HD.text <*> compositeField HD.text) <*>
    column HD.bool

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
        <*> compositeFieldArray HD.text)

viewKeyDepFromRow :: (Text,Text,Text,Text,Text,Text,[(Text, [Text])]) -> ViewKeyDependency
viewKeyDepFromRow (s1,t1,s2,v2,cons,consType,sCols) = ViewKeyDependency (QualifiedIdentifier s1 t1) (QualifiedIdentifier s2 v2) cons keyDep sCols
  where
    keyDep | consType == "p" = PKDep
           | consType == "f" = FKDep
           | otherwise       = FKDepRef -- f_ref, we build this type in the query

decodeFuncs :: HD.Result RoutineMap
decodeFuncs =
  -- Duplicate rows for a function means they're overloaded, order these by least args according to Routine Ord instance
  map sort . HM.fromListWith (++) . map ((\(x,y) -> (x, [y])) . addKey) <$> HD.rowList funcRow
  where
    funcRow = Function
              <$> column HD.text
              <*> column HD.text
              <*> nullableColumn HD.text
              <*> compositeArrayColumn
                  (RoutineParam
                  <$> compositeField HD.text
                  <*> compositeField HD.text
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
              <*> nullableColumn (toIsolationLevel <$> HD.text)
              <*> compositeArrayColumn ((,) <$> compositeField HD.text <*> compositeField HD.text) -- function setting

    addKey :: Routine -> (QualifiedIdentifier, Routine)
    addKey pd = (QualifiedIdentifier (pdSchema pd) (pdName pd), pd)

    parseRetType :: Text -> Text -> Bool -> Bool -> Bool -> RetType
    parseRetType schema name isSetOf isComposite isCompositeAlias
      | isSetOf   = SetOf pgType
      | otherwise = Single pgType
      where
        qi = QualifiedIdentifier schema name
        pgType
          | isComposite = Composite qi isCompositeAlias
          | otherwise   = Scalar qi

    parseVolatility :: Char -> FuncVolatility
    parseVolatility v | v == 'i' = Immutable
                      | v == 's' = Stable
                      | otherwise = Volatile -- only 'v' can happen here

decodeRepresentations :: HD.Result RepresentationsMap
decodeRepresentations =
  HM.fromList . map (\rep@DataRepresentation{drSourceType, drTargetType} -> ((drSourceType, drTargetType), rep)) <$> HD.rowList row
  where
    row = DataRepresentation
      <$> column HD.text
      <*> column HD.text
      <*> column HD.text

-- Selects all potential data representation transformations. To qualify the cast must be
-- 1. to or from a domain
-- 2. implicit
-- For the time being it must also be to/from JSON or text, although one can imagine a future where we support special
-- cases like CSV specific representations.
dataRepresentations :: Bool -> SQL.Statement [Schema] RepresentationsMap
dataRepresentations = SQL.Statement sql (arrayParam HE.text) decodeRepresentations
  where
    sql = [q|
    SELECT
      c.castsource::regtype::text,
      c.casttarget::regtype::text,
      c.castfunc::regproc::text
    FROM
      pg_catalog.pg_cast c
    JOIN pg_catalog.pg_type src_t
      ON c.castsource::oid = src_t.oid
    JOIN pg_catalog.pg_type dst_t
      ON c.casttarget::oid = dst_t.oid
    WHERE
      c.castcontext = 'i'
      AND c.castmethod = 'f'
      AND has_function_privilege(c.castfunc, 'execute')
      AND ((src_t.typtype = 'd' AND c.casttarget IN ('json'::regtype::oid , 'text'::regtype::oid))
       OR (dst_t.typtype = 'd' AND c.castsource IN ('json'::regtype::oid , 'text'::regtype::oid)))
    |]

allFunctions :: PgVersion -> Bool -> SQL.Statement [Schema] RoutineMap
allFunctions pgVer = SQL.Statement sql (arrayParam HE.text) decodeFuncs
  where
    sql = funcsSqlQuery pgVer <> " AND pn.nspname = ANY($1)"

accessibleFuncs :: PgVersion -> Bool -> SQL.Statement Schema RoutineMap
accessibleFuncs pgVer = SQL.Statement sql (param HE.text) decodeFuncs
  where
    sql = funcsSqlQuery pgVer <> " AND pn.nspname = $1 AND has_function_privilege(p.oid, 'execute')"

funcsSqlQuery :: PgVersion -> SqlQuery
funcsSqlQuery pgVer = [q|
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
        CASE type
          WHEN 'bit'::regtype THEN 'bit varying'
          WHEN 'bit[]'::regtype THEN 'bit varying[]'
          WHEN 'character'::regtype THEN 'character varying'
          WHEN 'character[]'::regtype THEN 'character varying[]'
          ELSE type::regtype::text
        END, -- convert types that ignore the lenth and accept any value till maximum size
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
    bt.oid <> bt.base as rettype_is_composite_alias,
    p.provolatile,
    p.provariadic > 0 as hasvariadic,
    lower((regexp_split_to_array((regexp_split_to_array(iso_config, '='))[2], ','))[1]) AS transaction_isolation_level,
    coalesce(func_settings.kvs, '{}') as kvs
  FROM pg_proc p
  LEFT JOIN arguments a ON a.oid = p.oid
  JOIN pg_namespace pn ON pn.oid = p.pronamespace
  JOIN base_types bt ON bt.oid = p.prorettype
  JOIN pg_type t ON t.oid = bt.base
  JOIN pg_namespace tn ON tn.oid = t.typnamespace
  LEFT JOIN pg_class comp ON comp.oid = t.typrelid
  LEFT JOIN pg_description as d ON d.objoid = p.oid
  LEFT JOIN LATERAL unnest(proconfig) iso_config ON iso_config like 'default_transaction_isolation%'
  LEFT JOIN LATERAL (
    SELECT
      array_agg(row(
        substr(setting, 1, strpos(setting, '=') - 1),
        lower(substr(setting, strpos(setting, '=') + 1))
      )) as kvs
    FROM unnest(proconfig) setting
    WHERE setting not LIKE 'default_transaction_isolation%'
  ) func_settings ON TRUE
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

accessibleTables :: PgVersion -> Bool -> SQL.Statement [Schema] AccessSet
accessibleTables pgVer =
  SQL.Statement sql (arrayParam HE.text) decodeAccessibleIdentifiers
 where
  sql = [q|
    SELECT
      n.nspname AS table_schema,
      c.relname AS table_name
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace
    WHERE c.relkind IN ('v','r','m','f','p')
    AND n.nspname NOT IN ('pg_catalog', 'information_schema')
    AND n.nspname = ANY($1)
    AND (
      pg_has_role(c.relowner, 'USAGE')
      or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
      or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
    ) |] <>
    relIsPartition <>
    "ORDER BY table_schema, table_name"
  relIsPartition = if pgVer >= pgVersion100 then " AND not c.relispartition " else mempty

{-
Adds M2O and O2O relationships for views to tables, tables to views, and views to views. The example below is taken from the test fixtures, but the views names/colnames were modified.

--allM2OandO2ORels sample query result--
private      | personnages          | private    | actors           | personnages_role_id_fkey   | {"(role_id,id)"}

--allViewsKeyDependencies sample query result--
private      | personnages          | test       | personnages_view | personnages_role_id_fkey   | f       | {"(role_id,roleId)"}
private      | actors               | test       | actors_view      | personnages_role_id_fkey   | f_ref   | {"(id,actorId)"}

--this function result--
test         | personnages_view     | private    | actors           | personnages_role_id_fkey   | f        | {"(roleId,id)"}       | viewTableM2O
private      | personnages          | test       | actors_view      | personnages_role_id_fkey   | f_ref    | {"(role_id,actorId)"} | tableViewM2O
test         | personnages_view     | test       | actors_view      | personnages_role_id_fkey   | f,r_ref  | {"(roleId,actorId)"}  | viewViewM2O
-}
addViewM2OAndO2ORels :: [ViewKeyDependency] -> [Relationship] -> [Relationship]
addViewM2OAndO2ORels keyDeps rels =
  rels ++ concatMap viewRels rels
  where
    isM2O card = case card of {M2O _ _ -> True; _ -> False;}
    isO2O card = case card of {O2O _ _ -> True; _ -> False;}
    viewRels Relationship{relTable,relForeignTable,relCardinality=card} =
      if isM2O card || isO2O card then
      let
        cons = relCons card
        relCols = relColumns card
        viewTableRels = filter (\ViewKeyDependency{keyDepTable, keyDepCons, keyDepType} -> keyDepTable == relTable        && keyDepCons == cons && keyDepType == FKDep)    keyDeps
        tableViewRels = filter (\ViewKeyDependency{keyDepTable, keyDepCons, keyDepType} -> keyDepTable == relForeignTable && keyDepCons == cons && keyDepType == FKDepRef) keyDeps
      in
        [ Relationship
            (keyDepView vwTbl)
            relForeignTable
            False
            ((if isM2O card then M2O else O2O) cons $ zipWith (\(_, vCol) (_, fCol)-> (vCol, fCol)) keyDepColsVwTbl relCols)
            True
            False
        | vwTbl <- viewTableRels
        , keyDepColsVwTbl <- expandKeyDepCols $ keyDepCols vwTbl ]
        ++
        [ Relationship
            relTable
            (keyDepView tblVw)
            False
            ((if isM2O card then M2O else O2O) cons $ zipWith (\(tCol, _) (_, vCol) -> (tCol, vCol)) relCols keyDepColsTblVw)
            False
            True
        | tblVw <- tableViewRels
        , keyDepColsTblVw <- expandKeyDepCols $ keyDepCols tblVw ]
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
            ((if isM2O card then M2O else O2O) cons $ zipWith (\(_, vcol1) (_, vcol2) -> (vcol1, vcol2)) keyDepColsVwTbl keyDepColsTblVw)
            True
            True
        | vwTbl <- viewTableRels
        , keyDepColsVwTbl <- expandKeyDepCols $ keyDepCols vwTbl
        , tblVw <- tableViewRels
        , keyDepColsTblVw <- expandKeyDepCols $ keyDepCols tblVw ]
      else []
    viewRels _ = []
    expandKeyDepCols kdc = zip (fst <$> kdc) <$> traverse snd kdc

addInverseRels :: [Relationship] -> [Relationship]
addInverseRels rels =
  rels ++
  [ Relationship ft t isSelf (O2M cons (swap <$> cols)) fTableIsView tableIsView | Relationship t ft isSelf (M2O cons cols) tableIsView fTableIsView <- rels ] ++
  [ Relationship ft t isSelf (O2O cons (swap <$> cols)) fTableIsView tableIsView | Relationship t ft isSelf (O2O cons cols) tableIsView fTableIsView <- rels ]

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
      concatMap (\(ViewKeyDependency _ _ _ _ pkCols) -> takeFirstPK pkCols) $
      filter (\(ViewKeyDependency _ viewQi _ dep _) -> dep == PKDep && viewQi == QualifiedIdentifier sch vw) keyDeps
    -- In the case of multiple reference to the same PK (see comment for ViewKeyDependency) we take the first reference available.
    -- We assume this to be safe to do, because:
    -- * We don't have any logic that requires the client to name a PK column (compared to the column hints in embedding for FKs),
    --   so we don't need to know about the other references.
    -- * We need to choose a single reference for each column, otherwise we'd output too many columns in location headers etc.
    takeFirstPK = mapMaybe (head . snd)

allTables :: PgVersion -> Bool -> SQL.Statement [Schema] TablesMap
allTables pgVer =
  SQL.Statement sql (arrayParam HE.text) decodeTables
  where
    sql = tablesSqlQuery pgVer

-- | Gets tables with their PK cols
tablesSqlQuery :: PgVersion -> SqlQuery
tablesSqlQuery pgVer =
  -- the tbl_constraints/key_col_usage CTEs are based on the standard "information_schema.table_constraints"/"information_schema.key_column_usage" views,
  -- we cannot use those directly as they include the following privilege filter:
  -- (pg_has_role(ss.relowner, 'USAGE'::text) OR has_column_privilege(ss.roid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES'::text));
  -- on the "columns" CTE, left joining on pg_depend and pg_class is used to obtain the sequence name as a column default in case there are GENERATED .. AS IDENTITY,
  -- generated columns are only available from pg >= 10 but the query is agnostic to versions. dep.deptype = 'i' is done because there are other 'a' dependencies on PKs
  [q|
  WITH
  columns AS (
      SELECT
          nc.nspname::name AS table_schema,
          c.relname::name AS table_name,
          a.attname::name AS column_name,
          d.description AS description,
  |] <> columnDefault <> [q| AS column_default,
          not (a.attnotnull OR t.typtype = 'd' AND t.typnotnull) AS is_nullable,
          CASE
              WHEN t.typtype = 'd' THEN
              CASE
                  WHEN nbt.nspname = 'pg_catalog'::name THEN format_type(t.typbasetype, NULL::integer)
                  ELSE format_type(a.atttypid, a.atttypmod)
              END
              ELSE
              CASE
                  WHEN nt.nspname = 'pg_catalog'::name THEN format_type(a.atttypid, NULL::integer)
                  ELSE format_type(a.atttypid, a.atttypmod)
              END
          END::text AS data_type,
          format_type(a.atttypid, a.atttypmod)::text AS nominal_data_type,
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
          LEFT JOIN pg_depend dep
              ON dep.refobjid = a.attrelid and dep.refobjsubid = a.attnum and dep.deptype = 'i'
          LEFT JOIN pg_class seqclass
              ON seqclass.oid = dep.objid
          LEFT JOIN pg_namespace seqsch
              ON seqsch.oid = seqclass.relnamespace
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
          info.nominal_data_type,
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
  "ORDER BY table_schema, table_name"
  where
    relIsPartition = if pgVer >= pgVersion100 then " AND not c.relispartition " else mempty
    columnDefault -- typbasetype and typdefaultbin handles `CREATE DOMAIN .. DEFAULT val`,  attidentity/attgenerated handles generated columns, pg_get_expr gets the default of a column
      | pgVer >= pgVersion120 = [q|
          CASE
            WHEN t.typbasetype  != 0  THEN pg_get_expr(t.typdefaultbin, 0)
            WHEN a.attidentity  = 'd' THEN format('nextval(%s)', quote_literal(seqsch.nspname || '.' || seqclass.relname))
            WHEN a.attgenerated = 's' THEN null
            ELSE pg_get_expr(ad.adbin, ad.adrelid)::text
          END|]
      | pgVer >= pgVersion100 = [q|
          CASE
            WHEN t.typbasetype  != 0  THEN pg_get_expr(t.typdefaultbin, 0)
            WHEN a.attidentity = 'd' THEN format('nextval(%s)', quote_literal(seqsch.nspname || '.' || seqclass.relname))
            ELSE pg_get_expr(ad.adbin, ad.adrelid)::text
          END|]
      | otherwise  = [q|
          CASE
            WHEN t.typbasetype  != 0  THEN pg_get_expr(t.typdefaultbin, 0)
            ELSE pg_get_expr(ad.adbin, ad.adrelid)::text
          END|]

-- | Gets many-to-one relationships and one-to-one(O2O) relationships, which are a refinement of the many-to-one's
allM2OandO2ORels :: PgVersion -> Bool -> SQL.Statement () [Relationship]
allM2OandO2ORels pgVer =
  SQL.Statement sql HE.noParams decodeRels
 where
  -- We use jsonb_agg for comparing the uniques/pks instead of array_agg to avoid the ERROR:  cannot accumulate arrays of different dimensionality
  sql = [q|
    WITH
    pks_uniques_cols AS (
      SELECT
        connamespace,
        conrelid,
        jsonb_agg(column_info.cols) as cols
      FROM pg_constraint
      JOIN lateral (
        SELECT array_agg(cols.attname order by cols.attnum) as cols
        FROM ( select unnest(conkey) as col) _
        JOIN pg_attribute cols on cols.attrelid = conrelid and cols.attnum = col
      ) column_info ON TRUE
      WHERE
        contype IN ('p', 'u') and
        connamespace::regnamespace::text <> 'pg_catalog'
      GROUP BY connamespace, conrelid
    )
    SELECT
      ns1.nspname AS table_schema,
      tab.relname AS table_name,
      ns2.nspname AS foreign_table_schema,
      other.relname AS foreign_table_name,
      (ns1.nspname, tab.relname) = (ns2.nspname, other.relname) AS is_self,
      traint.conname  AS constraint_name,
      column_info.cols_and_fcols,
      (column_info.cols IN (SELECT * FROM jsonb_array_elements(pks_uqs.cols))) AS one_to_one
    FROM pg_constraint traint
    JOIN LATERAL (
      SELECT
        array_agg(row(cols.attname, refs.attname) order by ord) AS cols_and_fcols,
        jsonb_agg(cols.attname order by ord) AS cols
      FROM unnest(traint.conkey, traint.confkey) WITH ORDINALITY AS _(col, ref, ord)
      JOIN pg_attribute cols ON cols.attrelid = traint.conrelid AND cols.attnum = col
      JOIN pg_attribute refs ON refs.attrelid = traint.confrelid AND refs.attnum = ref
    ) AS column_info ON TRUE
    JOIN pg_namespace ns1 ON ns1.oid = traint.connamespace
    JOIN pg_class tab ON tab.oid = traint.conrelid
    JOIN pg_class other ON other.oid = traint.confrelid
    JOIN pg_namespace ns2 ON ns2.oid = other.relnamespace
    LEFT JOIN pks_uniques_cols pks_uqs ON pks_uqs.connamespace = traint.connamespace AND pks_uqs.conrelid = traint.conrelid
    WHERE traint.contype = 'f'
  |] <>
    (if pgVer >= pgVersion110
      then " and traint.conparentid = 0 "
      else mempty) <>
    "ORDER BY traint.conrelid, traint.conname"

allComputedRels :: Bool -> SQL.Statement () [Relationship]
allComputedRels =
  SQL.Statement sql HE.noParams (HD.rowList cRelRow)
 where
  sql = [q|
    with
    all_relations as (
      select reltype
      from pg_class
      where relkind in ('v','r','m','f','p')
    ),
    computed_rels as (
      select
        (parse_ident(p.pronamespace::regnamespace::text))[1] as schema,
        p.proname::text                  as name,
        arg_schema.nspname::text         as rel_table_schema,
        arg_name.typname::text           as rel_table_name,
        ret_schema.nspname::text         as rel_ftable_schema,
        ret_name.typname::text           as rel_ftable_name,
        not p.proretset or p.prorows = 1 as single_row
      from pg_proc p
        join pg_type      arg_name   on arg_name.oid = p.proargtypes[0]
        join pg_namespace arg_schema on arg_schema.oid = arg_name.typnamespace
        join pg_type      ret_name   on ret_name.oid = p.prorettype
        join pg_namespace ret_schema on ret_schema.oid = ret_name.typnamespace
      where
        p.pronargs = 1
        and p.proargtypes[0] in (select reltype from all_relations)
        and p.prorettype in (select reltype from all_relations)
    )
    select
      *,
      row(rel_table_schema, rel_table_name) = row(rel_ftable_schema, rel_ftable_name) as is_self
    from computed_rels;
  |]

  cRelRow =
    ComputedRelationship <$>
    (QualifiedIdentifier <$> column HD.text <*> column HD.text) <*>
    (QualifiedIdentifier <$> column HD.text <*> column HD.text) <*>
    (QualifiedIdentifier <$> column HD.text <*> column HD.text) <*>
    pure (QualifiedIdentifier mempty mempty) <*>
    column HD.bool <*>
    column HD.bool

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
          contype::text as contype,
          conname,
          array_length(conkey, 1) as ncol,
          conrelid as resorigtbl,
          col as resorigcol,
          ord
        from pg_constraint
        left join lateral unnest(conkey) with ordinality as _(col, ord) on true
        where contype IN ('p', 'f')
        union
        -- fk referenced col
        select
          concat(contype, '_ref') as contype,
          conname,
          array_length(confkey, 1) as ncol,
          confrelid,
          col,
          ord
        from pg_constraint
        left join lateral unnest(confkey) with ordinality as _(col, ord) on true
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
      -- CYCLE detection according to PG docs: https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-CYCLE
      -- Can be replaced with CYCLE clause once PG v13 is EOL.
      recursion(view_id, view_schema, view_name, view_column, resorigtbl, resorigcol, is_cycle, path) as(
        select
          r.*,
          false,
          ARRAY[resorigtbl]
        from results r
        where view_schema = ANY ($1)
        union all
        select
          view.view_id,
          view.view_schema,
          view.view_name,
          view.view_column,
          tab.resorigtbl,
          tab.resorigcol,
          tab.resorigtbl = ANY(path),
          path || tab.resorigtbl
        from recursion view
        join results tab on view.resorigtbl=tab.view_id and view.resorigcol=tab.view_column
        where not is_cycle
      ),
      repeated_references as(
        select
          view_id,
          view_schema,
          view_name,
          resorigtbl,
          resorigcol,
          array_agg(attname) as view_columns
        from recursion
        join pg_attribute vcol on vcol.attrelid = view_id and vcol.attnum = view_column
        group by
          view_id,
          view_schema,
          view_name,
          resorigtbl,
          resorigcol
      )
      select
        sch.nspname as table_schema,
        tbl.relname as table_name,
        rep.view_schema,
        rep.view_name,
        pks_fks.conname as constraint_name,
        pks_fks.contype as constraint_type,
        array_agg(row(col.attname, view_columns) order by pks_fks.ord) as column_dependencies
      from repeated_references rep
      join pks_fks using (resorigtbl, resorigcol)
      join pg_class tbl on tbl.oid = rep.resorigtbl
      join pg_attribute col on col.attrelid = tbl.oid and col.attnum = rep.resorigcol
      join pg_namespace sch on sch.oid = tbl.relnamespace
      group by sch.nspname, tbl.relname,  rep.view_schema, rep.view_name, pks_fks.conname, pks_fks.contype, pks_fks.ncol
      -- make sure we only return key for which all columns are referenced in the view - no partial PKs or FKs
      having ncol = array_length(array_agg(row(col.attname, view_columns) order by pks_fks.ord), 1)
      |]

initialMediaHandlers :: MediaHandlerMap
initialMediaHandlers =
  HM.insert (RelAnyElement, MediaType.MTAny            ) (BuiltinOvAggJson,    MediaType.MTApplicationJSON) $
  HM.insert (RelAnyElement, MediaType.MTApplicationJSON) (BuiltinOvAggJson,    MediaType.MTApplicationJSON) $
  HM.insert (RelAnyElement, MediaType.MTTextCSV        ) (BuiltinOvAggCsv,     MediaType.MTTextCSV) $
  HM.insert (RelAnyElement, MediaType.MTGeoJSON        ) (BuiltinOvAggGeoJson, MediaType.MTGeoJSON)
  HM.empty

mediaHandlers :: PgVersion -> Bool -> SQL.Statement [Schema] MediaHandlerMap
mediaHandlers pgVer =
  SQL.Statement sql (arrayParam HE.text) decodeMediaHandlers
  where
    sql = [q|
      with
      all_relations as (
        select reltype
        from pg_class
        where relkind in ('v','r','m','f','p')
        union
        select oid
        from pg_type
        where typname = 'anyelement'
      ),
      media_types as (
          SELECT
            t.oid,
            lower(t.typname) as typname,
            b.oid as base_oid,
            b.typname AS basetypname,
            t.typnamespace,
            case t.typname
              when '*/*' then 'application/octet-stream'
              else t.typname
            end as resolved_media_type
          FROM pg_type t
          JOIN pg_type b ON t.typbasetype = b.oid
          WHERE
            t.typbasetype <> 0 and
            (t.typname ~* '^[A-Za-z0-9.-]+/[A-Za-z0-9.\+-]+$' or t.typname = '*/*')
      )
      select
        proc_schema.nspname           as handler_schema,
        proc.proname                  as handler_name,
        arg_schema.nspname::text      as target_schema,
        arg_name.typname::text        as target_name,
        media_types.typname           as media_type,
        media_types.resolved_media_type
      from media_types
        join pg_proc      proc         on proc.prorettype = media_types.oid
        join pg_namespace proc_schema  on proc_schema.oid = proc.pronamespace
        join pg_aggregate agg          on agg.aggfnoid = proc.oid
        join pg_type      arg_name     on arg_name.oid = proc.proargtypes[0]
        join pg_namespace arg_schema   on arg_schema.oid = arg_name.typnamespace
      where
        proc_schema.nspname = ANY($1) and
        proc.pronargs = 1 and
        arg_name.oid in (select reltype from all_relations)
      union
      select
          typ_sch.nspname as handler_schema,
          mtype.typname   as handler_name,
          pro_sch.nspname as target_schema,
          proname         as target_name,
          mtype.typname   as media_type,
          mtype.resolved_media_type
      from pg_proc proc
        join pg_namespace pro_sch on pro_sch.oid = proc.pronamespace
        join media_types mtype on proc.prorettype = mtype.oid
        join pg_namespace typ_sch     on typ_sch.oid = mtype.typnamespace
      where
        pro_sch.nspname = ANY($1) and NOT proretset
      |] <> (if pgVer >= pgVersion110 then " AND prokind = 'f'" else " AND NOT (proisagg OR proiswindow)")

decodeMediaHandlers :: HD.Result MediaHandlerMap
decodeMediaHandlers =
  HM.fromList . fmap (\(x, y, z, w) -> ((if isAnyElement y then RelAnyElement else RelId y, z), (CustomFunc x, w)) ) <$> HD.rowList caggRow
  where
    caggRow = (,,,)
              <$> (QualifiedIdentifier <$> column HD.text <*> column HD.text)
              <*> (QualifiedIdentifier <$> column HD.text <*> column HD.text)
              <*> (MediaType.decodeMediaType . encodeUtf8 <$> column HD.text)
              <*> (MediaType.decodeMediaType . encodeUtf8 <$> column HD.text)

timezones :: Bool -> SQL.Statement () TimezoneNames
timezones = SQL.Statement sql HE.noParams decodeTimezones
  where
    sql = "SELECT name FROM pg_timezone_names"
    decodeTimezones :: HD.Result TimezoneNames
    decodeTimezones = S.fromList . map encodeUtf8 <$> HD.rowList (column HD.text)

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
