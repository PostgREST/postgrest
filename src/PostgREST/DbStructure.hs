{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module PostgREST.DbStructure (
  createDbStructure
, doesProcExist
, doesProcReturnJWT
) where

import           Control.Applicative
import           Control.Monad          (join)
import           Data.Functor.Identity
import           Data.List              (elemIndex, find, subsequences, sort, transpose)
import           Data.Maybe             (fromMaybe, fromJust, isJust, mapMaybe, listToMaybe)
import           Data.Monoid
import           Data.Text              (Text, split)
import qualified Hasql                  as H
import qualified Hasql.Postgres         as P
import qualified Hasql.Backend          as B
import           PostgREST.PgQuery      ()
import           PostgREST.Types

import           GHC.Exts               (groupWith)
import           Prelude

createDbStructure :: Schema -> H.Tx P.Postgres s DbStructure
createDbStructure schema = do
  tabs <- allTables
  cols <- allColumns tabs
  syns <- allSynonyms cols
  rels <- allRelations tabs cols
  keys <- allPrimaryKeys tabs

  let rels' = (manyToManyRelations . raiseRelations schema syns . parentRelations . synonymousRelations syns) rels
      cols' = addForeignKeys rels' cols
      keys' = synonymousPrimaryKeys syns keys

  return DbStructure {
      tables = tabs
    , columns = cols'
    , relations = rels'
    , primaryKeys = keys'
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

synonymousColumns :: [(Column,Column)] -> [Column] -> [[Column]]
synonymousColumns allSyns cols = synCols'
  where
    syns = sort $ filter ((== colTable (head cols)) . colTable . fst) allSyns
    synCols  = transpose $ map (\c -> map snd $ filter ((== c) . fst) syns) cols
    synCols' = (filter sameTable . filter matchLength) synCols
    matchLength cs = length cols == length cs
    sameTable (c:cs) = all (\cc -> colTable c == colTable cc) (c:cs)
    sameTable [] = False

addForeignKeys :: [Relation] -> [Column] -> [Column]
addForeignKeys rels = map addFk
  where
    addFk col = col { colFK = fk col }
    fk col = join $ relToFk col <$> find (lookupFn col) rels
    lookupFn :: Column -> Relation -> Bool
    lookupFn c (Relation{relColumns=cs, relType=rty}) = c `elem` cs && rty==Child
    -- lookupFn _ _ = False
    relToFk col (Relation{relColumns=cols, relFColumns=colsF}) = ForeignKey <$> colF
      where
        pos = elemIndex col cols
        colF = (colsF !!) <$> pos

synonymousRelations :: [(Column,Column)] -> [Relation] -> [Relation]
synonymousRelations _ [] = []
synonymousRelations syns (rel:rels) = rel : synRelsP ++ synRelsF ++ synonymousRelations syns rels
  where
    synRelsP = synRels (relColumns rel) (\t cs -> rel{relTable=t,relColumns=cs})
    synRelsF = synRels (relFColumns rel) (\t cs -> rel{relFTable=t,relFColumns=cs})
    synRels cols mapFn = map (\cs -> mapFn (colTable $ head cs) cs) $ synonymousColumns syns cols

parentRelations :: [Relation] -> [Relation]
parentRelations [] = []
parentRelations (rel@(Relation t c ft fc _ _ _ _):rels) = Relation ft fc t c Parent Nothing Nothing Nothing : rel : parentRelations rels

manyToManyRelations :: [Relation] -> [Relation]
manyToManyRelations rels = rels ++ mapMaybe link2Relation links
  where
    links = join $ map (combinations 2) $ filter (not . null) $ groupWith groupFn $ filter ( (==Child). relType) rels
    groupFn :: Relation -> Text
    groupFn (Relation{relTable=Table{tableSchema=s, tableName=t}}) = s<>"_"<>t
    combinations k ns = filter ((k==).length) (subsequences ns)
    link2Relation [
      Relation{relTable=lt, relColumns=lc1, relFTable=t,  relFColumns=c},
      Relation{             relColumns=lc2, relFTable=ft, relFColumns=fc}
      ]
      | lc1 /= lc2 && length lc1 == 1 && length lc2 == 1 = Just $ Relation t c ft fc Many (Just lt) (Just lc1) (Just lc2)
      | otherwise = Nothing
    link2Relation _ = Nothing

raiseRelations :: Schema -> [(Column,Column)] -> [Relation] -> [Relation]
raiseRelations schema syns = map raiseRel
  where
    raiseRel rel
      | tableSchema table == schema = rel
      | isJust newCols = rel{relFTable=fromJust newTable,relFColumns=fromJust newCols}
      | otherwise = rel
      where
        cols = relFColumns rel
        table = relFTable rel
        newCols = listToMaybe $ filter ((== schema) . tableSchema . colTable . head) (synonymousColumns syns cols)
        newTable = (colTable . head) <$> newCols

synonymousPrimaryKeys :: [(Column,Column)] -> [PrimaryKey] -> [PrimaryKey]
synonymousPrimaryKeys _ [] = []
synonymousPrimaryKeys syns (key:keys) = key : newKeys ++ synonymousPrimaryKeys syns keys
  where
    keySyns = filter ((\c -> colTable c == pkTable key && colName c == pkName key) . fst) syns
    newKeys = map ((\c -> PrimaryKey{pkTable=colTable c,pkName=colName c}) . snd) keySyns

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
            AND (pg_trigger.tgtype::integer & 69) = 69) ) AS insertable
      FROM pg_class c
      JOIN pg_namespace n ON n.oid = c.relnamespace
      WHERE c.relkind IN ('v','r','m')
        AND n.nspname NOT IN ('pg_catalog', 'information_schema')
      GROUP BY table_schema, table_name, insertable
      ORDER BY table_schema, table_name;
    |]
    return $ map tableFromRow rows

tableFromRow :: (Text, Text, Bool) -> Table
tableFromRow (s, n, i) = Table s n i

allColumns :: [Table] -> H.Tx P.Postgres s [Column]
allColumns tabs = do
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
  return $ map (columnFromRow tabs) cols

columnFromRow :: [Table] ->
                 (Text,       Text,      Text,
                  Int,        Bool,      Text,
                  Bool,       Maybe Int, Maybe Int,
                  Maybe Text, Maybe Text)
                 -> Column
columnFromRow tabs (s, t, n, pos, nul, typ, u, l, p, d, e) =
  Column table n pos nul typ u l p d (parseEnum e) Nothing
  where
    table = fromJust $ find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs
    parseEnum :: Maybe Text -> [Text]
    parseEnum str = fromMaybe [] $ split (==',') <$> str

allRelations :: [Table] -> [Column] -> H.Tx P.Postgres s [Relation]
allRelations tabs cols = do
  rels <- H.listEx $ [H.stmt|
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
  |]
  return $ map (relationFromRow tabs cols) rels

relationFromRow :: [Table] -> [Column] -> (Text, Text, [Text], Text, Text, [Text]) -> Relation
relationFromRow allTabs allCols (rs, rt, rcs, frs, frt, frcs) = Relation table cols tableF colsF Child Nothing Nothing Nothing
  where
    findTable s t = fromJust $ find (\tbl -> tableSchema tbl == s && tableName tbl == t) allTabs
    findCols s t cs = filter (\col -> tableSchema (colTable col) == s && tableName (colTable col) == t && colName col `elem` cs) allCols
    table  = findTable rs rt
    tableF = findTable frs frt
    cols  = findCols rs rt rcs
    colsF = findCols frs frt frcs

allPrimaryKeys :: [Table] -> H.Tx P.Postgres s [PrimaryKey]
allPrimaryKeys tabs = do
  pks <- H.listEx $ [H.stmt|
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
    |]
  return $ map (pkFromRow tabs) pks

pkFromRow :: [Table] -> (Schema, Text, Text) -> PrimaryKey
pkFromRow tabs (s, t, n) = PrimaryKey table n
  where
    table = fromJust $ find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs

allSynonyms :: [Column] -> H.Tx P.Postgres s [(Column,Column)]
allSynonyms allCols = do
  srcSyns <- H.listEx $ [H.stmt|
    WITH synonyms AS (
      SELECT
        vcu.table_schema AS src_table_schema,
        vcu.table_name AS src_table_name,
        vcu.column_name AS src_column_name,
        view.table_schema AS syn_table_schema,
        view.table_name AS syn_table_name,
        view.view_definition AS view_definition
      FROM
        information_schema.views AS view,
        information_schema.view_column_usage AS vcu
      WHERE
        view.table_schema = vcu.view_schema AND
        view.table_name = vcu.view_name AND
        view.table_schema NOT IN ('pg_catalog', 'information_schema') AND
        (SELECT COUNT(*) FROM information_schema.view_table_usage WHERE view_schema = view.table_schema AND view_name = view.table_name) = 1
    )
    SELECT
      src_table_schema, src_table_name, src_column_name,
      syn_table_schema, syn_table_name,
      (regexp_matches(view_definition, CONCAT('\.(', src_column_name, ')(?=,|$)'), 'gn'))[1]
    FROM synonyms
    UNION (
      SELECT
        src_table_schema, src_table_name, src_column_name,
        syn_table_schema, syn_table_name,
        (regexp_matches(view_definition, CONCAT('\.', src_column_name, '\sAS\s("?)(.+?)\1(,|$)'), 'gn'))[2] /* " <- for syntax highlighting */
      FROM synonyms
    )
    |]
  return $ map (\(a,b,c,d,e,f) -> (findCol a b c,findCol d e f)) srcSyns
  where
    findCol s t c = fromJust $ find (\col -> (tableSchema . colTable) col == s && (tableName . colTable) col == t && colName col == c) allCols
