{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module PostgREST.DbStructure (
  getDbStructure
, accessibleTables
, doesProcExist
, doesProcReturnJWT
) where

import qualified Hasql.Query             as H
import qualified Hasql.Encoders          as HE
import qualified Hasql.Decoders          as HD

import           Control.Applicative
import           Control.Monad          (join)
import           Data.Functor.Contravariant (contramap)
import           Data.Functor.Identity
import           Text.InterpolatedString.Perl6 (q)
import           Data.List              (elemIndex, find, subsequences, sort, transpose)
import           Data.Maybe             (fromMaybe, fromJust, isJust, mapMaybe, listToMaybe)
import           Data.Monoid
import           Data.Text              (Text, split)
import qualified Hasql.Session          as H
import           PostgREST.Types

import           GHC.Exts               (groupWith)
import           Prelude

getDbStructure :: Schema -> H.Session DbStructure
getDbStructure schema = do
  tabs <- allTables
  cols <- allColumns tabs
  syns <- allSynonyms cols
  rels <- allRelations tabs cols
  keys <- allPrimaryKeys tabs

  let rels' = (addManyToManyRelations . raiseRelations schema syns . addParentRelations . addSynonymousRelations syns) rels
      cols' = addForeignKeys rels' cols
      keys' = synonymousPrimaryKeys syns keys

  return DbStructure {
      dbTables = tabs
    , dbColumns = cols'
    , dbRelations = rels'
    , dbPrimaryKeys = keys'
    }

encodeQi :: HE.Params QualifiedIdentifier
encodeQi =
  contramap qiSchema (HE.value HE.text) <>
  contramap qiName   (HE.value HE.text)

decodeTable :: HD.Result Table
decodeTable =
  HD.singleRow standardRow
 where
  standardRow = Table <$> HD.value HD.text <*> HD.value HD.text
                      <*> HD.value HD.bool

doesProcExist :: H.Query QualifiedIdentifier Bool
doesProcExist =
  H.statement sql encodeQi (HD.singleRow (HD.value HD.bool)) True
 where
  sql = [q| SELECT EXISTS (
      SELECT 1
      FROM   pg_catalog.pg_namespace n
      JOIN   pg_catalog.pg_proc p
      ON     pronamespace = n.oid
      WHERE  nspname = ?
      AND    proname = ?
    ) |]

doesProcReturnJWT :: H.Query QualifiedIdentifier Bool
doesProcReturnJWT =
  H.statement sql encodeQi (HD.singleRow (HD.value HD.bool)) True
 where
  sql = [q| SELECT EXISTS (
      SELECT 1
      FROM   pg_catalog.pg_namespace n
      JOIN   pg_catalog.pg_proc p
      ON     pronamespace = n.oid
      WHERE  nspname = ?
      AND    proname = ?
      AND    pg_catalog.pg_get_function_result(p.oid) like '%jwt_claims'
    ) |]

accessibleTables :: Schema -> H.Tx P.Postgres s [Table]
accessibleTables schema = do
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

addSynonymousRelations :: [(Column,Column)] -> [Relation] -> [Relation]
addSynonymousRelations _ [] = []
addSynonymousRelations syns (rel:rels) = rel : synRelsP ++ synRelsF ++ addSynonymousRelations syns rels
  where
    synRelsP = synRels (relColumns rel) (\t cs -> rel{relTable=t,relColumns=cs})
    synRelsF = synRels (relFColumns rel) (\t cs -> rel{relFTable=t,relFColumns=cs})
    synRels cols mapFn = map (\cs -> mapFn (colTable $ head cs) cs) $ synonymousColumns syns cols

addParentRelations :: [Relation] -> [Relation]
addParentRelations [] = []
addParentRelations (rel@(Relation t c ft fc _ _ _ _):rels) = Relation ft fc t c Parent Nothing Nothing Nothing : rel : addParentRelations rels

addManyToManyRelations :: [Relation] -> [Relation]
addManyToManyRelations rels = rels ++ addMirrorRelation (mapMaybe link2Relation links)
  where
    links = join $ map (combinations 2) $ filter (not . null) $ groupWith groupFn $ filter ( (==Child). relType) rels
    groupFn :: Relation -> Text
    groupFn (Relation{relTable=Table{tableSchema=s, tableName=t}}) = s<>"_"<>t
    combinations k ns = filter ((k==).length) (subsequences ns)
    addMirrorRelation [] = []
    addMirrorRelation (rel@(Relation t c ft fc _ lt lc1 lc2):rels') = Relation ft fc t c Many lt lc2 lc1 : rel : addMirrorRelation rels'
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

allTables :: H.Session [Table]
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
      ORDER BY table_schema, table_name
    |]
    return $ map tableFromRow rows

tableFromRow :: (Text, Text, Bool) -> Table
tableFromRow (s, n, i) = Table s n i

allColumns :: [Table] -> H.Session [Column]
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
          /*
          -- CTE based on information_schema.columns to remove the owner filter
          */
          WITH columns AS (
              SELECT current_database()::information_schema.sql_identifier AS table_catalog,
                  nc.nspname::information_schema.sql_identifier AS table_schema,
                  c.relname::information_schema.sql_identifier AS table_name,
                  a.attname::information_schema.sql_identifier AS column_name,
                  a.attnum::information_schema.cardinal_number AS ordinal_position,
                  pg_get_expr(ad.adbin, ad.adrelid)::information_schema.character_data AS column_default,
                      CASE
                          WHEN a.attnotnull OR t.typtype = 'd'::"char" AND t.typnotnull THEN 'NO'::text
                          ELSE 'YES'::text
                      END::information_schema.yes_or_no AS is_nullable,
                      CASE
                          WHEN t.typtype = 'd'::"char" THEN
                          CASE
                              WHEN bt.typelem <> 0::oid AND bt.typlen = (-1) THEN 'ARRAY'::text
                              WHEN nbt.nspname = 'pg_catalog'::name THEN format_type(t.typbasetype, NULL::integer)
                              ELSE 'USER-DEFINED'::text
                          END
                          ELSE
                          CASE
                              WHEN t.typelem <> 0::oid AND t.typlen = (-1) THEN 'ARRAY'::text
                              WHEN nt.nspname = 'pg_catalog'::name THEN format_type(a.atttypid, NULL::integer)
                              ELSE 'USER-DEFINED'::text
                          END
                      END::information_schema.character_data AS data_type,
                  information_schema._pg_char_max_length(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS character_maximum_length,
                  information_schema._pg_char_octet_length(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS character_octet_length,
                  information_schema._pg_numeric_precision(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS numeric_precision,
                  information_schema._pg_numeric_precision_radix(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS numeric_precision_radix,
                  information_schema._pg_numeric_scale(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS numeric_scale,
                  information_schema._pg_datetime_precision(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.cardinal_number AS datetime_precision,
                  information_schema._pg_interval_type(information_schema._pg_truetypid(a.*, t.*), information_schema._pg_truetypmod(a.*, t.*))::information_schema.character_data AS interval_type,
                  NULL::integer::information_schema.cardinal_number AS interval_precision,
                  NULL::character varying::information_schema.sql_identifier AS character_set_catalog,
                  NULL::character varying::information_schema.sql_identifier AS character_set_schema,
                  NULL::character varying::information_schema.sql_identifier AS character_set_name,
                      CASE
                          WHEN nco.nspname IS NOT NULL THEN current_database()
                          ELSE NULL::name
                      END::information_schema.sql_identifier AS collation_catalog,
                  nco.nspname::information_schema.sql_identifier AS collation_schema,
                  co.collname::information_schema.sql_identifier AS collation_name,
                      CASE
                          WHEN t.typtype = 'd'::"char" THEN current_database()
                          ELSE NULL::name
                      END::information_schema.sql_identifier AS domain_catalog,
                      CASE
                          WHEN t.typtype = 'd'::"char" THEN nt.nspname
                          ELSE NULL::name
                      END::information_schema.sql_identifier AS domain_schema,
                      CASE
                          WHEN t.typtype = 'd'::"char" THEN t.typname
                          ELSE NULL::name
                      END::information_schema.sql_identifier AS domain_name,
                  current_database()::information_schema.sql_identifier AS udt_catalog,
                  COALESCE(nbt.nspname, nt.nspname)::information_schema.sql_identifier AS udt_schema,
                  COALESCE(bt.typname, t.typname)::information_schema.sql_identifier AS udt_name,
                  NULL::character varying::information_schema.sql_identifier AS scope_catalog,
                  NULL::character varying::information_schema.sql_identifier AS scope_schema,
                  NULL::character varying::information_schema.sql_identifier AS scope_name,
                  NULL::integer::information_schema.cardinal_number AS maximum_cardinality,
                  a.attnum::information_schema.sql_identifier AS dtd_identifier,
                  'NO'::character varying::information_schema.yes_or_no AS is_self_referencing,
                  'NO'::character varying::information_schema.yes_or_no AS is_identity,
                  NULL::character varying::information_schema.character_data AS identity_generation,
                  NULL::character varying::information_schema.character_data AS identity_start,
                  NULL::character varying::information_schema.character_data AS identity_increment,
                  NULL::character varying::information_schema.character_data AS identity_maximum,
                  NULL::character varying::information_schema.character_data AS identity_minimum,
                  NULL::character varying::information_schema.yes_or_no AS identity_cycle,
                  'NEVER'::character varying::information_schema.character_data AS is_generated,
                  NULL::character varying::information_schema.character_data AS generation_expression,
                  CASE
                      WHEN c.relkind = 'r'::"char" OR (c.relkind = ANY (ARRAY['v'::"char", 'f'::"char"])) AND pg_column_is_updatable(c.oid::regclass, a.attnum, false) THEN 'YES'::text
                      ELSE 'NO'::text
                  END::information_schema.yes_or_no AS is_updatable
              FROM pg_attribute a
                 LEFT JOIN pg_attrdef ad ON a.attrelid = ad.adrelid AND a.attnum = ad.adnum
                 JOIN (pg_class c
                 JOIN pg_namespace nc ON c.relnamespace = nc.oid) ON a.attrelid = c.oid
                 JOIN (pg_type t
                 JOIN pg_namespace nt ON t.typnamespace = nt.oid) ON a.atttypid = t.oid
                 LEFT JOIN (pg_type bt
                 JOIN pg_namespace nbt ON bt.typnamespace = nbt.oid) ON t.typtype = 'd'::"char" AND t.typbasetype = bt.oid
                 LEFT JOIN (pg_collation co
                 JOIN pg_namespace nco ON co.collnamespace = nco.oid) ON a.attcollation = co.oid AND (nco.nspname <> 'pg_catalog'::name OR co.collname <> 'default'::name)
              WHERE NOT pg_is_other_temp_schema(nc.oid) AND a.attnum > 0 AND NOT a.attisdropped AND (c.relkind = ANY (ARRAY['r'::"char", 'v'::"char", 'f'::"char"]))
                /*--AND (pg_has_role(c.relowner, 'USAGE'::text) OR has_column_privilege(c.oid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES'::text))*/
          )
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
          /*-- FROM information_schema.columns*/
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
      ORDER BY schema, position
  |]
  return $ mapMaybe (columnFromRow tabs) cols

columnFromRow :: [Table] ->
                 (Text,       Text,      Text,
                  Int,        Bool,      Text,
                  Bool,       Maybe Int, Maybe Int,
                  Maybe Text, Maybe Text)
                 -> Maybe Column
columnFromRow tabs (s, t, n, pos, nul, typ, u, l, p, d, e) = buildColumn <$> table
  where
    buildColumn tbl = Column tbl n pos nul typ u l p d (parseEnum e) Nothing
    table = find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs
    parseEnum :: Maybe Text -> [Text]
    parseEnum str = fromMaybe [] $ split (==',') <$> str

allRelations :: [Table] -> [Column] -> H.Session [Relation]
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
  return $ mapMaybe (relationFromRow tabs cols) rels

relationFromRow :: [Table] -> [Column] -> (Text, Text, [Text], Text, Text, [Text]) -> Maybe Relation
relationFromRow allTabs allCols (rs, rt, rcs, frs, frt, frcs) =
  Relation <$> table <*> cols <*> tableF <*> colsF <*> pure Child <*> pure Nothing <*> pure Nothing <*> pure Nothing
  where
    findTable s t = find (\tbl -> tableSchema tbl == s && tableName tbl == t) allTabs
    findCol s t c = find (\col -> tableSchema (colTable col) == s && tableName (colTable col) == t && colName col == c) allCols
    table  = findTable rs rt
    tableF = findTable frs frt
    cols  = mapM (findCol rs rt) rcs
    colsF = mapM (findCol frs frt) frcs

allPrimaryKeys :: [Table] -> H.Session [PrimaryKey]
allPrimaryKeys tabs = do
  pks <- H.listEx $ [H.stmt|
      /*
      -- CTE to replace information_schema.table_constraints to remove owner limit
      */
      WITH tc AS (
          SELECT current_database()::information_schema.sql_identifier AS constraint_catalog,
              nc.nspname::information_schema.sql_identifier AS constraint_schema,
              c.conname::information_schema.sql_identifier AS constraint_name,
              current_database()::information_schema.sql_identifier AS table_catalog,
              nr.nspname::information_schema.sql_identifier AS table_schema,
              r.relname::information_schema.sql_identifier AS table_name,
                  CASE c.contype
                      WHEN 'c'::"char" THEN 'CHECK'::text
                      WHEN 'f'::"char" THEN 'FOREIGN KEY'::text
                      WHEN 'p'::"char" THEN 'PRIMARY KEY'::text
                      WHEN 'u'::"char" THEN 'UNIQUE'::text
                      ELSE NULL::text
                  END::information_schema.character_data AS constraint_type,
                  CASE
                      WHEN c.condeferrable THEN 'YES'::text
                      ELSE 'NO'::text
                  END::information_schema.yes_or_no AS is_deferrable,
                  CASE
                      WHEN c.condeferred THEN 'YES'::text
                      ELSE 'NO'::text
                  END::information_schema.yes_or_no AS initially_deferred
          FROM pg_namespace nc,
              pg_namespace nr,
              pg_constraint c,
              pg_class r
          WHERE nc.oid = c.connamespace AND nr.oid = r.relnamespace AND c.conrelid = r.oid AND (c.contype <> ALL (ARRAY['t'::"char", 'x'::"char"])) AND r.relkind = 'r'::"char" AND NOT pg_is_other_temp_schema(nr.oid)
          /*--AND (pg_has_role(r.relowner, 'USAGE'::text) OR has_table_privilege(r.oid, 'INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text) OR has_any_column_privilege(r.oid, 'INSERT, UPDATE, REFERENCES'::text))*/
          UNION ALL
          SELECT current_database()::information_schema.sql_identifier AS constraint_catalog,
              nr.nspname::information_schema.sql_identifier AS constraint_schema,
              (((((nr.oid::text || '_'::text) || r.oid::text) || '_'::text) || a.attnum::text) || '_not_null'::text)::information_schema.sql_identifier AS constraint_name,
              current_database()::information_schema.sql_identifier AS table_catalog,
              nr.nspname::information_schema.sql_identifier AS table_schema,
              r.relname::information_schema.sql_identifier AS table_name,
              'CHECK'::character varying::information_schema.character_data AS constraint_type,
              'NO'::character varying::information_schema.yes_or_no AS is_deferrable,
              'NO'::character varying::information_schema.yes_or_no AS initially_deferred
          FROM pg_namespace nr,
              pg_class r,
              pg_attribute a
          WHERE nr.oid = r.relnamespace AND r.oid = a.attrelid AND a.attnotnull AND a.attnum > 0 AND NOT a.attisdropped AND r.relkind = 'r'::"char" AND NOT pg_is_other_temp_schema(nr.oid)
          /*--AND (pg_has_role(r.relowner, 'USAGE'::text) OR has_table_privilege(r.oid, 'INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text) OR has_any_column_privilege(r.oid, 'INSERT, UPDATE, REFERENCES'::text))*/
      ),
      /*
      -- CTE to replace information_schema.key_column_usage to remove owner limit
      */
      kc AS (
          SELECT current_database()::information_schema.sql_identifier AS constraint_catalog,
              ss.nc_nspname::information_schema.sql_identifier AS constraint_schema,
              ss.conname::information_schema.sql_identifier AS constraint_name,
              current_database()::information_schema.sql_identifier AS table_catalog,
              ss.nr_nspname::information_schema.sql_identifier AS table_schema,
              ss.relname::information_schema.sql_identifier AS table_name,
              a.attname::information_schema.sql_identifier AS column_name,
              (ss.x).n::information_schema.cardinal_number AS ordinal_position,
                  CASE
                      WHEN ss.contype = 'f'::"char" THEN information_schema._pg_index_position(ss.conindid, ss.confkey[(ss.x).n])
                      ELSE NULL::integer
                  END::information_schema.cardinal_number AS position_in_unique_constraint
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
                  c.confrelid,
                  information_schema._pg_expandarray(c.conkey) AS x
                 FROM pg_namespace nr,
                  pg_class r,
                  pg_namespace nc,
                  pg_constraint c
                WHERE nr.oid = r.relnamespace AND r.oid = c.conrelid AND nc.oid = c.connamespace AND (c.contype = ANY (ARRAY['p'::"char", 'u'::"char", 'f'::"char"])) AND r.relkind = 'r'::"char" AND NOT pg_is_other_temp_schema(nr.oid)) ss
          WHERE ss.roid = a.attrelid AND a.attnum = (ss.x).x AND NOT a.attisdropped
          /*--AND (pg_has_role(ss.relowner, 'USAGE'::text) OR has_column_privilege(ss.roid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES'::text))*/
      )
      SELECT
          kc.table_schema,
          kc.table_name,
          kc.column_name
      FROM
          /*
          --information_schema.table_constraints tc,
          --information_schema.key_column_usage kc
          */
          tc, kc
      WHERE
          tc.constraint_type = 'PRIMARY KEY' AND
          kc.table_name = tc.table_name AND
          kc.table_schema = tc.table_schema AND
          kc.constraint_name = tc.constraint_name AND
          kc.table_schema NOT IN ('pg_catalog', 'information_schema')
    |]
  return $ mapMaybe (pkFromRow tabs) pks

pkFromRow :: [Table] -> (Schema, Text, Text) -> Maybe PrimaryKey
pkFromRow tabs (s, t, n) = PrimaryKey <$> table <*> pure n
  where table = find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs

allSynonyms :: [Column] -> H.Session [(Column,Column)]
allSynonyms allCols = do
  syns <- H.listEx $ [H.stmt|
    WITH synonyms AS (
      /*
      -- CTE to replace the view from information_schema because the information in it depended on the logged in role
      -- notice the commented line
      */
      WITH view_column_usage AS (
        SELECT DISTINCT
               CAST(current_database() AS character varying) AS view_catalog,
               CAST(nv.nspname AS character varying) AS view_schema,
               CAST(v.relname AS character varying) AS view_name,
               CAST(current_database() AS character varying) AS table_catalog,
               CAST(nt.nspname AS character varying) AS table_schema,
               CAST(t.relname AS character varying) AS table_name,
               CAST(a.attname AS character varying) AS column_name
        FROM pg_namespace nv, pg_class v, pg_depend dv,
             pg_depend dt, pg_class t, pg_namespace nt,
             pg_attribute a
        WHERE nv.oid = v.relnamespace
              AND v.relkind = 'v'
              AND v.oid = dv.refobjid
              AND dv.refclassid = 'pg_catalog.pg_class'::regclass
              AND dv.classid = 'pg_catalog.pg_rewrite'::regclass
              AND dv.deptype = 'i'
              AND dv.objid = dt.objid
              AND dv.refobjid <> dt.refobjid
              AND dt.classid = 'pg_catalog.pg_rewrite'::regclass
              AND dt.refclassid = 'pg_catalog.pg_class'::regclass
              AND dt.refobjid = t.oid
              AND t.relnamespace = nt.oid
              AND t.relkind IN ('r', 'v', 'f')
              AND t.oid = a.attrelid
              AND dt.refobjsubid = a.attnum
              /*--AND pg_has_role(t.relowner, 'USAGE')*/
      )
      SELECT
        vcu.table_schema AS src_table_schema,
        vcu.table_name AS src_table_name,
        vcu.column_name AS src_column_name,
        view.schemaname AS syn_table_schema,
        view.viewname AS syn_table_name,
        view.definition AS view_definition
      FROM
        pg_catalog.pg_views AS view,
        view_column_usage AS vcu
      WHERE
        view.schemaname = vcu.view_schema AND
        view.viewname = vcu.view_name AND
        view.schemaname NOT IN ('pg_catalog', 'information_schema')
        /*--AND (SELECT COUNT(*) FROM information_schema.view_table_usage WHERE view_schema = view.schemaname AND view_name = view.viewname) = 1*/
    )
    SELECT
      src_table_schema, src_table_name, src_column_name,
      syn_table_schema, syn_table_name,
      (regexp_matches(view_definition, CONCAT('\.(', src_column_name, ')(?=,|$)'), 'gn'))[1] AS syn_column_name
    FROM synonyms
    UNION (
      SELECT
        src_table_schema, src_table_name, src_column_name,
        syn_table_schema, syn_table_name,
        (regexp_matches(view_definition, CONCAT('\.', src_column_name, '\sAS\s("?)(.+?)\1(,|$)'), 'gn'))[2] AS syn_column_name /* " <- for syntax highlighting */
      FROM synonyms
    )
    |]
  return $ mapMaybe (synonymFromRow allCols) syns

synonymFromRow :: [Column] -> (Text,Text,Text,Text,Text,Text) -> Maybe (Column,Column)
synonymFromRow allCols (s1,t1,c1,s2,t2,c2) = (,) <$> col1 <*> col2
  where
    col1 = findCol s1 t1 c1
    col2 = findCol s2 t2 c2
    findCol s t c = find (\col -> (tableSchema . colTable) col == s && (tableName . colTable) col == t && colName col == c) allCols
