{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PostgREST.SchemaCache
-- Description : PostgREST schema cache
--
-- This module(used to be named DbStructure) contains queries that target PostgreSQL system catalogs, these are used to build the schema cache(SchemaCache).
--
-- The schema cache is necessary for resource embedding, foreign keys are used for inferring the relationships between tables.
--
-- These queries are executed once at startup or when PostgREST is reloaded.
module PostgREST.SchemaCache
  ( SchemaCache (..)
  , TablesFuzzyIndex
  , querySchemaCache
  , showSummary
  , QueryTimings (..)
  , queryTimingsWLabels
  )
where

import Control.Arrow ((&&&))
import Data.Aeson ((.=))
import Data.Functor.Contravariant ((>$<))
import NeatInterpolation (trimming)
import Protolude

import Data.Aeson qualified as JSON
import Data.ByteString.Char8 qualified as BS
import Data.FuzzySet qualified as Fuzzy
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as S
import Data.Text qualified as T

import PostgREST.Catalog.Decoders (column)
import PostgREST.Catalog.Identifiers
  ( QualifiedIdentifier (..)
  , RelIdentifier (..)
  , Schema
  , escapeIdent
  , isAnyElement
  )
import PostgREST.Catalog.Query
  ( allFunctions
  , allM2OandO2ORels
  , allTables
  , allViewsKeyDependencies
  )
import PostgREST.Catalog.Relationship
  ( Cardinality (..)
  , Junction (..)
  , KeyDep (..)
  , Relationship (..)
  , RelationshipsMap
  , ViewKeyDependency (..)
  )
import PostgREST.Catalog.Representations
  ( DataRepresentation (..)
  , RepresentationsMap
  )
import PostgREST.Catalog.Routine
  ( MediaHandler (..)
  , MediaHandlerMap
  , RoutineMap
  )
import PostgREST.Catalog.Table
  ( Table (..)
  , TablesMap
  )
import PostgREST.Config (AppConfig (..), LogLevel (..))
import PostgREST.Config.PgVersion (PgVersion)

import Hasql.Decoders qualified as HD
import Hasql.Encoders qualified as HE
import Hasql.Statement qualified as SQL
import Hasql.Transaction qualified as SQL
import PostgREST.MediaType qualified as MediaType

type TablesFuzzyIndex = HM.HashMap Schema Fuzzy.FuzzySet

data SchemaCache = SchemaCache
  { dbTables :: TablesMap
  , dbRelationships :: RelationshipsMap
  , dbRoutines :: RoutineMap
  , dbRepresentations :: RepresentationsMap
  , dbMediaHandlers :: MediaHandlerMap
  , -- Memoized fuzzy index of table names per schema to support approximate matching
    -- Since index construction can be expensive, we build it once and store in the SchemaCache
    -- Haskell lazy evaluation ensures it's only built on first use and memoized afterwards
    dbTablesFuzzyIndex :: TablesFuzzyIndex
  }
  deriving (Show)

instance JSON.ToJSON SchemaCache where
  toJSON (SchemaCache tabs rels routs reps hdlers _) =
    JSON.object
      [ "dbTables" .= JSON.toJSON tabs
      , "dbRelationships" .= JSON.toJSON rels
      , "dbRoutines" .= JSON.toJSON routs
      , "dbRepresentations" .= JSON.toJSON reps
      , "dbMediaHandlers" .= JSON.toJSON hdlers
      ]

showSummary :: SchemaCache -> Text
showSummary (SchemaCache tbls rels routs reps mediaHdlrs _) =
  T.intercalate
    ", "
    [ show (HM.size tbls) <> " Relations"
    , show (HM.size rels) <> " Relationships"
    , show (HM.size routs) <> " RPCs"
    , show (HM.size reps) <> " Domain Representations"
    , show (HM.size mediaHdlrs) <> " Media Type Handlers"
    ]

maxDbTablesForFuzzySearch :: Int
maxDbTablesForFuzzySearch = 500

querySchemaCache :: PgVersion -> AppConfig -> SQL.Transaction (SchemaCache, Maybe QueryTimings)
querySchemaCache pgVer conf@AppConfig{..} = do
  SQL.sql "set local schema ''" -- This voids the search path. The following queries need this for getting the fully qualified name(schema.name) of every db object
  for_ configInternalSCQuerySleepFst (`SQL.statement` sleepCall) -- only used for testing
  tabs <- sqlTimedStmt gucTbls conf (allTables pgVer configDbPreparedStatements)
  keyDeps <- sqlTimedStmt gucKDeps conf allViewsKeyDependencies
  m2oRels <- sqlTimedStmt gucRels mempty allM2OandO2ORels
  funcs <- sqlTimedStmt gucFuncs conf (allFunctions pgVer configDbPreparedStatements)
  cRels <- sqlTimedStmt gucCRels mempty allComputedRels
  reps <- sqlTimedStmt gucDReps conf dataRepresentations
  mHdlers <- sqlTimedStmt gucMHdrs conf mediaHandlers

  for_ configInternalSCQuerySleepSnd (`SQL.statement` sleepCall) -- only used for testing
  qsTime <-
    if isLogDebug then
      Just <$> SQL.statement mempty extractTimings
    else
      pure Nothing

  let
    tabsWViewsPks = addViewPrimaryKeys tabs keyDeps
    rels = addInverseRels $ addM2MRels tabsWViewsPks $ addViewM2OAndO2ORels keyDeps m2oRels

  return
    ( removeInternal schemas $
        SchemaCache
          { dbTables = tabsWViewsPks
          , dbRelationships = getOverrideRelationshipsMap rels cRels
          , dbRoutines = funcs
          , dbRepresentations = reps
          , dbMediaHandlers = HM.union mHdlers initialMediaHandlers -- the custom handlers will override the initial ones
          , dbTablesFuzzyIndex =
              -- Only build fuzzy index for schemas with a reasonable number of tables
              -- Fuzzy.FuzzySet is memory heavy we just don't use it for large schemas
              Fuzzy.fromList <$> HM.filter ((< maxDbTablesForFuzzySearch) . length) (HM.fromListWith (<>) ((qiSchema &&& pure . qiName) <$> HM.keys tabsWViewsPks))
          }
    , qsTime
    )
  where
    schemas = toList configDbSchemas
    isLogDebug = configLogLevel == LogDebug
    sqlTimedStmt = sqlTimedStatement isLogDebug
    sleepCall = SQL.Statement "select pg_sleep($1 / 1000.0)" (param HE.int4) HD.noResult True

-- | overrides detected relationships with the computed relationships and gets the RelationshipsMap
getOverrideRelationshipsMap :: [Relationship] -> [Relationship] -> RelationshipsMap
getOverrideRelationshipsMap rels cRels =
  sort <$> deformedRelMap patchedRels
  where
    -- there can only be a single (table_type, func_name) pair in a function definition `test.function(table_type)`, so we use HM.fromList to disallow duplicates
    computedRels = HM.fromList $ relMapKey <$> cRels
    -- here we override the detected relationships with the user computed relationships, HM.union makes sure computedRels prevail
    patchedRels = HM.union computedRels (relsMap rels)
    relsMap = HM.fromListWith (++) . fmap relMapKey
    relMapKey rel = case rel of
      Relationship{relTable, relForeignTable} -> ((relTable, relForeignTable), [rel])
      -- we use (relTable, relFunction) as key to override detected relationships with the function name
      ComputedRelationship{relTable, relFunction} -> ((relTable, relFunction), [rel])
    -- Since a relationship is between a table and foreign table, the logical way to index/search is by their table/ftable QualifiedIdentifier
    -- However, because we allow searching a relationship by the columns of the foreign key(using the "column as target" disambiguation) we lose the
    -- ability to index by the foreign table name, so we deform the key. TODO remove once support for "column as target" is gone.
    deformedRelMap = HM.fromListWith (++) . fmap addDeformedRelKey . HM.toList
    addDeformedRelKey ((relT, relFT), rls) = ((relT, qiSchema relFT), rls)

-- | Remove db objects that belong to an internal schema(not exposed through the API) from the SchemaCache.
removeInternal :: [Schema] -> SchemaCache -> SchemaCache
removeInternal schemas dbStruct =
  SchemaCache
    { dbTables = HM.filterWithKey (\(QualifiedIdentifier sch _) _ -> sch `elem` schemas) $ dbTables dbStruct
    , dbRelationships =
        filter (\r -> qiSchema (relForeignTable r) `elem` schemas && not (hasInternalJunction r))
          <$> HM.filterWithKey (\(QualifiedIdentifier sch _, _) _ -> sch `elem` schemas) (dbRelationships dbStruct)
    , dbRoutines = dbRoutines dbStruct -- procs are only obtained from the exposed schemas, no need to filter them.
    , dbRepresentations = dbRepresentations dbStruct -- no need to filter, not directly exposed through the API
    , dbMediaHandlers = dbMediaHandlers dbStruct
    , dbTablesFuzzyIndex = dbTablesFuzzyIndex dbStruct
    }
  where
    hasInternalJunction ComputedRelationship{} = False
    hasInternalJunction Relationship{relCardinality = card} = case card of
      M2M Junction{junTable} -> qiSchema junTable `notElem` schemas
      _ -> False

decodeRepresentations :: HD.Result RepresentationsMap
decodeRepresentations =
  HM.fromList . map (\rep@DataRepresentation{drSourceType, drTargetType} -> ((drSourceType, drTargetType), rep)) <$> HD.rowList row
  where
    row =
      DataRepresentation
        <$> column HD.text
        <*> column HD.text
        <*> column HD.text

-- Selects all potential data representation transformations. To qualify the cast must be
-- 1. to or from a domain
-- 2. implicit
-- For the time being it must also be to/from JSON or text, although one can imagine a future where we support special
-- cases like CSV specific representations.
dataRepresentations :: SQL.Statement AppConfig RepresentationsMap
dataRepresentations = SQL.Statement sql mempty decodeRepresentations True
  where
    sql =
      encodeUtf8
        [trimming|
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
    isM2O card = case card of M2O _ _ -> True; _ -> False
    isO2O card = case card of O2O _ _ False -> True; _ -> False
    viewRels Relationship{relTable, relForeignTable, relCardinality = card}
      | isM2O card || isO2O card =
          let
            cons = relCons card
            relCols = relColumns card
            buildCard cns cls = if isM2O card then M2O cns cls else O2O cns cls False
            viewTableRels = fold $ HM.lookup (relTable, (cons, FKDep)) indexedKeyDeps
            tableViewRels = fold $ HM.lookup (relForeignTable, (cons, FKDepRef)) indexedKeyDeps
          in
            [ Relationship
                (keyDepView vwTbl)
                relForeignTable
                False
                (buildCard cons $ zipWith (\(_, vCol) (_, fCol) -> (vCol, fCol)) keyDepColsVwTbl relCols)
                True
                False
            | vwTbl <- viewTableRels
            , keyDepColsVwTbl <- expandKeyDepCols $ keyDepCols vwTbl
            ]
              ++ [ Relationship
                     relTable
                     (keyDepView tblVw)
                     False
                     (buildCard cons $ zipWith (\(tCol, _) (_, vCol) -> (tCol, vCol)) relCols keyDepColsTblVw)
                     False
                     True
                 | tblVw <- tableViewRels
                 , keyDepColsTblVw <- expandKeyDepCols $ keyDepCols tblVw
                 ]
              ++ [ let
                     vw1 = keyDepView vwTbl
                     vw2 = keyDepView tblVw
                   in
                     Relationship
                       vw1
                       vw2
                       (vw1 == vw2)
                       (buildCard cons $ zipWith (\(_, vcol1) (_, vcol2) -> (vcol1, vcol2)) keyDepColsVwTbl keyDepColsTblVw)
                       True
                       True
                 | vwTbl <- viewTableRels
                 , keyDepColsVwTbl <- expandKeyDepCols $ keyDepCols vwTbl
                 , tblVw <- tableViewRels
                 , keyDepColsTblVw <- expandKeyDepCols $ keyDepCols tblVw
                 ]
    viewRels _ = []
    expandKeyDepCols kdc = zip (fst <$> kdc) <$> traverse snd kdc
    indexedKeyDeps = HM.fromListWith (<>) $ fmap ((keyDepTable &&& keyDepCons &&& keyDepType) &&& pure) keyDeps

addInverseRels :: [Relationship] -> [Relationship]
addInverseRels rels =
  rels
    ++ [Relationship ft t isSelf (O2M cons (swap <$> cols)) fTableIsView tableIsView | Relationship t ft isSelf (M2O cons cols) tableIsView fTableIsView <- rels]
    ++ [Relationship ft t isSelf (O2O cons (swap <$> cols) (not isParent)) fTableIsView tableIsView | Relationship t ft isSelf (O2O cons cols isParent) tableIsView fTableIsView <- rels]

-- | Adds a m2m relationship if a table has FKs to two other tables and the FK columns are part of the PK columns
addM2MRels :: TablesMap -> [Relationship] -> [Relationship]
addM2MRels tbls rels =
  rels
    ++ catMaybes
      [ let
          jtCols = S.fromList $ (fst <$> cols) ++ (fst <$> fcols)
          pkCols = S.fromList $ maybe mempty tablePKCols $ HM.lookup jt1 tbls
        in
          if S.isSubsetOf jtCols pkCols then
            Just $ Relationship t ft (t == ft) (M2M $ Junction jt1 cons1 cons2 (swap <$> cols) (swap <$> fcols)) tblIsView fTblisView
          else
            Nothing
      | Relationship jt1 t _ (M2O cons1 cols) _ tblIsView <- rels
      , Relationship _ ft _ (M2O cons2 fcols) _ fTblisView <- fold $ HM.lookup jt1 indexedRels
      , cons1 /= cons2
      ]
  where
    indexedRels = HM.fromListWith (<>) $ fmap (relTable &&& pure) rels

addViewPrimaryKeys :: TablesMap -> [ViewKeyDependency] -> TablesMap
addViewPrimaryKeys tabs keyDeps =
  ( \tbl@Table{tableSchema, tableName, tableIsView} ->
      if tableIsView then
        tbl{tablePKCols = findViewPKCols tableSchema tableName}
      else
        tbl
  )
    <$> tabs
  where
    findViewPKCols sch vw =
      concatMap (\(ViewKeyDependency _ _ _ _ pkCols) -> takeFirstPK pkCols) $
        fold $
          HM.lookup (PKDep, QualifiedIdentifier sch vw) indexedDeps
    -- In the case of multiple reference to the same PK (see comment for ViewKeyDependency) we take the first reference available.
    -- We assume this to be safe to do, because:
    -- \* We don't have any logic that requires the client to name a PK column (compared to the column hints in embedding for FKs),
    --   so we don't need to know about the other references.
    -- \* We need to choose a single reference for each column, otherwise we'd output too many columns in location headers etc.
    takeFirstPK = mapMaybe (head . snd)
    indexedDeps = HM.fromListWith (++) $ fmap ((keyDepType &&& keyDepView) &&& pure) keyDeps

allComputedRels :: SQL.Statement () [Relationship]
allComputedRels =
  SQL.Statement sql HE.noParams (HD.rowList cRelRow) True
  where
    sql =
      encodeUtf8
        [trimming|
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
      ComputedRelationship
        <$> (QualifiedIdentifier <$> column HD.text <*> column HD.text)
        <*> (QualifiedIdentifier <$> column HD.text <*> column HD.text)
        <*> (QualifiedIdentifier <$> column HD.text <*> column HD.text)
        <*> pure (QualifiedIdentifier mempty mempty)
        <*> column HD.bool
        <*> column HD.bool

-- | Returns all the views' primary keys and foreign keys dependencies
initialMediaHandlers :: MediaHandlerMap
initialMediaHandlers =
  HM.insert (RelAnyElement, MediaType.MTAny) (BuiltinOvAggJson, MediaType.MTApplicationJSON) $
    HM.insert (RelAnyElement, MediaType.MTApplicationJSON) (BuiltinOvAggJson, MediaType.MTApplicationJSON) $
      HM.insert (RelAnyElement, MediaType.MTTextCSV) (BuiltinOvAggCsv, MediaType.MTTextCSV) $
        HM.insert
          (RelAnyElement, MediaType.MTGeoJSON)
          (BuiltinOvAggGeoJson, MediaType.MTGeoJSON)
          HM.empty

mediaHandlers :: SQL.Statement AppConfig MediaHandlerMap
mediaHandlers =
  SQL.Statement sql params decodeMediaHandlers True
  where
    params = map escapeIdent . toList . configDbSchemas >$< arrayParam HE.text
    sql =
      encodeUtf8
        [trimming|
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
            t.typnamespace,
            case t.typname
              when '*/*' then 'application/octet-stream'
              else t.typname
            end as resolved_media_type
          FROM pg_type t
          JOIN pg_type b ON t.typbasetype = b.oid
          WHERE
            t.typbasetype <> 0 and
            (t.typname ~* '^[A-Za-z0-9.-]+/[A-Za-z0-9.\+-]+$$' or t.typname = '*/*')
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
        proc.pronamespace = ANY($$1::regnamespace[]) and
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
        proc.pronamespace = ANY($$1::regnamespace[]) and NOT proretset
        and prokind = 'f'|]

decodeMediaHandlers :: HD.Result MediaHandlerMap
decodeMediaHandlers =
  HM.fromList
    . fmap
      ( \(x, y, z, w) ->
          let rel = if isAnyElement y then RelAnyElement else RelId y
          in  ((rel, z), (CustomFunc x rel, w))
      )
    <$> HD.rowList caggRow
  where
    caggRow =
      (,,,)
        <$> (QualifiedIdentifier <$> column HD.text <*> column HD.text)
        <*> (QualifiedIdentifier <$> column HD.text <*> column HD.text)
        <*> (MediaType.decodeMediaType . encodeUtf8 <$> column HD.text)
        <*> (MediaType.decodeMediaType . encodeUtf8 <$> column HD.text)

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable

arrayParam :: HE.Value a -> HE.Params [a]
arrayParam = param . HE.foldableArray . HE.nonNullable

{-
 - Times a sql statement inside a transaction, for this:
 -
 - 1. We start a timer: select set_config('pgrst.tmp_x', clock_timestamp()::text, false);
 - 2. Run the statement: select ....
 - 3. End the timer:  select set_config('pgrst.tmp_x', (clock_timestamp() - current_setting('pgrst.tmp_x', false)::timestamptz)::text, false);
 -
 - We can do this for several statements inside the transaction. The timings are later captured at the end of the transaction with extractTimings.
 -}
sqlTimedStatement :: Bool -> ByteString -> a -> SQL.Statement a b -> SQL.Transaction b
sqlTimedStatement isLogDebug guc params stmt =
  if isLogDebug then
    SQL.sql sFrag >> SQL.statement params stmt <* SQL.sql eFrag
  else
    SQL.statement params stmt
  where
    sFrag = "select set_config('pgrst." <> guc <> "', clock_timestamp()::text, true)"
    eFrag = "select set_config('pgrst." <> guc <> "', (clock_timestamp() - current_setting('pgrst." <> guc <> "', false)::timestamptz)::text, true)"

-- Extract all the generated timings (see sqlTimedStatement) converting the value to milliseconds.
extractTimings :: SQL.Statement () QueryTimings
extractTimings = SQL.Statement sql HE.noParams decodeThem True
  where
    qFrag setting = "extract('milliseconds' from current_setting('pgrst." <> setting <> "', false)::interval)::text"
    sql =
      "SELECT "
        <> BS.intercalate
          ","
          [ qFrag gucTbls
          , qFrag gucKDeps
          , qFrag gucRels
          , qFrag gucFuncs
          , qFrag gucCRels
          , qFrag gucDReps
          , qFrag gucMHdrs
          ]
    decodeThem :: HD.Result QueryTimings
    decodeThem =
      HD.singleRow $
        QueryTimings
          <$> column HD.text
          <*> column HD.text
          <*> column HD.text
          <*> column HD.text
          <*> column HD.text
          <*> column HD.text
          <*> column HD.text

data QueryTimings = QueryTimings
  { qtTables :: Text
  , qtKeyDeps :: Text
  , qtRels :: Text
  , qtFuncs :: Text
  , qtCRels :: Text
  , qtDReps :: Text
  , qtMHdrs :: Text
  }
  deriving (Show)

queryTimingsWLabels :: QueryTimings -> [(ByteString, Text)]
queryTimingsWLabels qt =
  [ (gucTbls, qtTables qt)
  , (gucKDeps, qtKeyDeps qt)
  , (gucRels, qtRels qt)
  , (gucFuncs, qtFuncs qt)
  , (gucCRels, qtCRels qt)
  , (gucDReps, qtDReps qt)
  , (gucMHdrs, qtMHdrs qt)
  ]

gucTbls, gucKDeps, gucRels, gucFuncs, gucCRels, gucDReps, gucMHdrs :: ByteString
gucTbls = "tables"
gucKDeps = "keydeps"
gucRels = "rels"
gucFuncs = "funcs"
gucCRels = "comprels"
gucDReps = "dreps"
gucMHdrs = "mhandlers"
