{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE NamedFieldPuns  #-}
module PostgREST.DbStructure (
  getDbStructure
, accessibleTables
, accessibleProcs
, schemaDescription
, getPgVersion
) where

import qualified Hasql.Decoders                as HD
import qualified Hasql.Encoders                as HE
import qualified Hasql.Statement               as H

import           Control.Applicative
import qualified Data.HashMap.Strict           as M
import qualified Data.List                     as L
import           Data.Set                      as S (fromList)
import           Data.Text                     (split, strip,
                                                breakOn, dropAround,
                                                splitOn)
import qualified Data.Text                     as T
import qualified Hasql.Session                 as H
import           PostgREST.Types
import           Text.InterpolatedString.Perl6 (q, qc)

import           GHC.Exts                      (groupWith)
import           Protolude
import           Unsafe (unsafeHead)

getDbStructure :: Schema -> PgVersion -> H.Session DbStructure
getDbStructure schema pgVer = do
  tabs      <- H.statement () allTables
  cols      <- H.statement schema $ allColumns tabs
  syns      <- H.statement schema $ allSynonyms cols pgVer
  childRels <- H.statement () $ allChildRelations tabs cols
  keys      <- H.statement () $ allPrimaryKeys tabs
  procs     <- H.statement schema allProcs

  let rels = addManyToManyRelations . addParentRelations $ addViewChildRelations syns childRels
      cols' = addForeignKeys rels cols
      keys' = addViewPrimaryKeys syns keys

  return DbStructure {
      dbTables = tabs
    , dbColumns = cols'
    , dbRelations = rels
    , dbPrimaryKeys = keys'
    , dbProcs = procs
    , pgVersion = pgVer
    }

decodeTables :: HD.Result [Table]
decodeTables =
  HD.rowList tblRow
 where
  tblRow = Table <$> HD.column HD.text
                 <*> HD.column HD.text
                 <*> HD.nullableColumn HD.text
                 <*> HD.column HD.bool

decodeColumns :: [Table] -> HD.Result [Column]
decodeColumns tables =
  mapMaybe (columnFromRow tables) <$> HD.rowList colRow
 where
  colRow =
    (,,,,,,,,,,,)
      <$> HD.column HD.text <*> HD.column HD.text
      <*> HD.column HD.text <*> HD.nullableColumn HD.text
      <*> HD.column HD.int4 <*> HD.column HD.bool
      <*> HD.column HD.text <*> HD.column HD.bool
      <*> HD.nullableColumn HD.int4
      <*> HD.nullableColumn HD.int4
      <*> HD.nullableColumn HD.text
      <*> HD.nullableColumn HD.text

decodeRelations :: [Table] -> [Column] -> HD.Result [Relation]
decodeRelations tables cols =
  mapMaybe (relationFromRow tables cols) <$> HD.rowList relRow
 where
  relRow = (,,,,,)
    <$> HD.column HD.text
    <*> HD.column HD.text
    <*> HD.column (HD.array (HD.dimension replicateM (HD.element HD.text)))
    <*> HD.column HD.text
    <*> HD.column HD.text
    <*> HD.column (HD.array (HD.dimension replicateM (HD.element HD.text)))

decodePks :: [Table] -> HD.Result [PrimaryKey]
decodePks tables =
  mapMaybe (pkFromRow tables) <$> HD.rowList pkRow
 where
  pkRow = (,,) <$> HD.column HD.text <*> HD.column HD.text <*> HD.column HD.text

decodeSynonyms :: [Column] -> HD.Result [Synonym]
decodeSynonyms cols =
  mapMaybe (synonymFromRow cols) <$> HD.rowList synRow
 where
  synRow = (,,,,,)
    <$> HD.column HD.text <*> HD.column HD.text
    <*> HD.column HD.text <*> HD.column HD.text
    <*> HD.column HD.text <*> HD.column HD.text

decodeProcs :: HD.Result (M.HashMap Text [ProcDescription])
decodeProcs =
  -- Duplicate rows for a function means they're overloaded, order these by least args according to ProcDescription Ord instance
  map sort . M.fromListWith (++) . map ((\(x,y) -> (x, [y])) . addName) <$> HD.rowList tblRow
  where
    tblRow = ProcDescription
              <$> HD.column HD.text
              <*> HD.nullableColumn HD.text
              <*> (parseArgs <$> HD.column HD.text)
              <*> (parseRetType
                  <$> HD.column HD.text
                  <*> HD.column HD.text
                  <*> HD.column HD.bool
                  <*> HD.column HD.char)
              <*> (parseVolatility <$> HD.column HD.char)

    addName :: ProcDescription -> (Text, ProcDescription)
    addName pd = (pdName pd, pd)

    parseArgs :: Text -> [PgArg]
    parseArgs = mapMaybe parseArg . filter (not . isPrefixOf "OUT" . toS) . map strip . split (==',')

    parseArg :: Text -> Maybe PgArg
    parseArg a =
      let arg = lastDef "" $ splitOn "INOUT " a
          (body, def) = breakOn " DEFAULT " arg
          (name, typ) = breakOn " " body in
      if T.null typ
         then Nothing
         else Just $
           PgArg (dropAround (== '"') name) (strip typ) (T.null def)

    parseRetType :: Text -> Text -> Bool -> Char -> RetType
    parseRetType schema name isSetOf typ
      | isSetOf   = SetOf pgType
      | otherwise = Single pgType
      where
        qi = QualifiedIdentifier schema name
        pgType = case typ of
          'c' -> Composite qi
          'p' -> if name == "record" -- Only pg pseudo type that is a row type is 'record'
                   then Composite qi
                   else Scalar qi
          _   -> Scalar qi -- 'b'ase, 'd'omain, 'e'num, 'r'ange

    parseVolatility :: Char -> ProcVolatility
    parseVolatility v | v == 'i' = Immutable
                      | v == 's' = Stable
                      | otherwise = Volatile -- only 'v' can happen here

allProcs :: H.Statement Schema (M.HashMap Text [ProcDescription])
allProcs = H.Statement (toS procsSqlQuery) (HE.param HE.text) decodeProcs True

accessibleProcs :: H.Statement Schema (M.HashMap Text [ProcDescription])
accessibleProcs = H.Statement (toS sql) (HE.param HE.text) decodeProcs True
  where
    sql = procsSqlQuery <> " AND has_function_privilege(p.oid, 'execute')"

procsSqlQuery :: SqlQuery
procsSqlQuery = [q|
  SELECT p.proname as "proc_name",
         d.description as "proc_description",
         pg_get_function_arguments(p.oid) as "args",
         tn.nspname as "rettype_schema",
         coalesce(comp.relname, t.typname) as "rettype_name",
         p.proretset as "rettype_is_setof",
         t.typtype as "rettype_typ",
         p.provolatile
  FROM pg_proc p
    JOIN pg_namespace pn ON pn.oid = p.pronamespace
    JOIN pg_type t ON t.oid = p.prorettype
    JOIN pg_namespace tn ON tn.oid = t.typnamespace
    LEFT JOIN pg_class comp ON comp.oid = t.typrelid
    LEFT JOIN pg_catalog.pg_description as d on d.objoid = p.oid
  WHERE  pn.nspname = $1
|]

schemaDescription :: H.Statement Schema (Maybe Text)
schemaDescription =
    H.Statement sql (HE.param HE.text) (join <$> HD.rowMaybe (HD.nullableColumn HD.text)) True
  where
    sql = [q|
      select
        description
      from
        pg_catalog.pg_namespace n
        left join pg_catalog.pg_description d on d.objoid = n.oid
      where
        n.nspname = $1 |]

accessibleTables :: H.Statement Schema [Table]
accessibleTables =
  H.Statement sql (HE.param HE.text) decodeTables True
 where
  sql = [q|
    select
      n.nspname as table_schema,
      relname as table_name,
      d.description as table_description,
      c.relkind = 'r' or (c.relkind IN ('v', 'f')) and (pg_relation_is_updatable(c.oid::regclass, false) & 8) = 8
      or (exists (
         select 1
         from pg_trigger
         where pg_trigger.tgrelid = c.oid and (pg_trigger.tgtype::integer & 69) = 69)
      ) as insertable
    from
      pg_class c
      join pg_namespace n on n.oid = c.relnamespace
      left join pg_catalog.pg_description as d on d.objoid = c.oid and d.objsubid = 0
    where
      c.relkind in ('v', 'r', 'm', 'f')
      and n.nspname = $1
      and (
        pg_has_role(c.relowner, 'USAGE'::text)
        or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text)
        or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)
      )
    order by relname |]

addForeignKeys :: [Relation] -> [Column] -> [Column]
addForeignKeys rels = map addFk
  where
    addFk col = col { colFK = fk col }
    fk col = join $ relToFk col <$> find (lookupFn col) rels
    lookupFn :: Column -> Relation -> Bool
    lookupFn c Relation{relColumns=cs, relType=rty} = c `elem` cs && rty==Child
    relToFk col Relation{relColumns=cols, relFColumns=colsF} = do
      pos <- L.elemIndex col cols
      colF <- atMay colsF pos
      return $ ForeignKey colF

{-
Adds Views Child Relations based on Synonyms found, the logic is as follows:

Having a Relation{relTable=t1, relColumns=[c1], relFTable=t2, relFColumns=[c2], relType=Child} represented by:

t1.c1------t2.c2

When only having a t1_view.c1 synonym, we need to add a View to Table Child Relation

         t1.c1----t2.c2         t1.c1----------t2.c2
                         ->            ________/
                                      /
      t1_view.c1             t1_view.c1


When only having a t2_view.c2 synonym, we need to add a Table to View Child Relation

         t1.c1----t2.c2               t1.c1----------t2.c2
                               ->          \________
                                                    \
                    t2_view.c2                      t2_view.c1

When having t1_view.c1 and a t2_view.c2 synonyms, we need to add a View to View Child Relation in addition to the prior

         t1.c1----t2.c2               t1.c1----------t2.c2
                               ->          \________/
                                           /        \
    t1_view.c1     t2_view.c2     t1_view.c1-------t2_view.c1

The logic for composite pks is similar just need to make sure all the Relation columns have synonyms.
-}
addViewChildRelations :: [Synonym] -> [Relation] -> [Relation]
addViewChildRelations allSyns = concatMap (\rel ->
  rel : case rel of
    Relation{relType=Child, relTable, relColumns, relFTable, relFColumns} ->

      let colSynsGroupedByView :: [Column] -> [[Synonym]]
          colSynsGroupedByView relCols = L.groupBy (\(_, viewCol1) (_, viewCol2) -> colTable viewCol1 == colTable viewCol2) $
                                         filter (\(c, _) -> c `elem` relCols) allSyns
          colsSyns = colSynsGroupedByView relColumns
          fColsSyns = colSynsGroupedByView relFColumns
          getView :: [Synonym] -> Table
          getView = colTable . snd . unsafeHead
          syns `allSynsOf` cols = S.fromList (fst <$> syns) == S.fromList cols
          -- Relation is dependent on the order of relColumns and relFColumns to get the join conditions right in the generated query.
          -- So we need to change the order of the synonyms to match the relColumns
          -- This could be avoided if the Relation type is improved with a structure that maintains the association of relColumns and relFColumns
          syns `sortAccordingTo` columns = sortOn (\(k, _) -> L.lookup k $ zip columns [0::Int ..]) syns in

      -- View Table Child Relations
      [Relation (getView syns) (snd <$> syns `sortAccordingTo` relColumns) relFTable relFColumns Child Nothing Nothing Nothing
        | syns <- colsSyns, syns `allSynsOf` relColumns] ++

      -- Table View Child Relations
      [Relation relTable relColumns (getView fSyns) (snd <$> fSyns `sortAccordingTo` relFColumns) Child Nothing Nothing Nothing
        | fSyns <- fColsSyns, fSyns `allSynsOf` relFColumns] ++

      -- View View Child Relations
      [Relation (getView syns) (snd <$> syns `sortAccordingTo` relColumns) (getView fSyns) (snd <$> fSyns `sortAccordingTo` relFColumns) Child Nothing Nothing Nothing
        | syns <- colsSyns, fSyns <- fColsSyns, syns `allSynsOf` relColumns, fSyns `allSynsOf` relFColumns]

    _ -> [])

addParentRelations :: [Relation] -> [Relation]
addParentRelations = concatMap (\rel@(Relation t c ft fc _ _ _ _) -> [rel, Relation ft fc t c Parent Nothing Nothing Nothing])

addManyToManyRelations :: [Relation] -> [Relation]
addManyToManyRelations rels = rels ++ addMirrorRelation (mapMaybe link2Relation links)
  where
    links = join $ map (combinations 2) $ filter (not . null) $ groupWith groupFn $ filter ( (==Child). relType) rels
    groupFn :: Relation -> Text
    groupFn Relation{relTable=Table{tableSchema=s, tableName=t}} = s <> "_" <> t
    -- Reference : https://wiki.haskell.org/99_questions/Solutions/26
    combinations :: Int -> [a] -> [[a]]
    combinations 0 _  = [ [] ]
    combinations n xs = [ y:ys | y:xs' <- tails xs
                               , ys <- combinations (n-1) xs']
    addMirrorRelation = concatMap (\rel@(Relation t c ft fc _ lt lc1 lc2) -> [rel, Relation ft fc t c Many lt lc2 lc1])
    link2Relation [
      Relation{relTable=lt, relColumns=lc1, relFTable=t,  relFColumns=c},
      Relation{             relColumns=lc2, relFTable=ft, relFColumns=fc}
      ]
      | lc1 /= lc2 && length lc1 == 1 && length lc2 == 1 = Just $ Relation t c ft fc Many (Just lt) (Just lc1) (Just lc2)
      | otherwise = Nothing
    link2Relation _ = Nothing

addViewPrimaryKeys :: [Synonym] -> [PrimaryKey] -> [PrimaryKey]
addViewPrimaryKeys syns = concatMap (\pk ->
  let viewPks = (\(_, viewCol) -> PrimaryKey{pkTable=colTable viewCol, pkName=colName viewCol}) <$>
                filter (\(col, _) -> colTable col == pkTable pk && colName col == pkName pk) syns in
  pk : viewPks)

allTables :: H.Statement () [Table]
allTables =
  H.Statement sql HE.unit decodeTables True
 where
  sql = [q|
    SELECT
      n.nspname AS table_schema,
      c.relname AS table_name,
      NULL AS table_description,
      c.relkind = 'r' OR (c.relkind IN ('v','f'))
      AND (pg_relation_is_updatable(c.oid::regclass, FALSE) & 8) = 8
      OR (EXISTS
        ( SELECT 1
          FROM pg_trigger
          WHERE pg_trigger.tgrelid = c.oid
          AND (pg_trigger.tgtype::integer & 69) = 69) ) AS insertable
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace
    WHERE c.relkind IN ('v','r','m','f')
      AND n.nspname NOT IN ('pg_catalog', 'information_schema')
    GROUP BY table_schema, table_name, insertable
    ORDER BY table_schema, table_name |]

allColumns :: [Table] -> H.Statement Schema [Column]
allColumns tabs =
  H.Statement sql (HE.param HE.text) (decodeColumns tabs) True
 where
  sql = [q|
    SELECT DISTINCT
        info.table_schema AS schema,
        info.table_name AS table_name,
        info.column_name AS name,
        info.description AS description,
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
        -- CTE based on pg_catalog to get only Primary and Foreign key columns outside api schema
        */
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
               r.contype IN ('f', 'p')
               AND c.relkind IN ('r', 'v', 'f', 'm')
               AND r.conrelid = c.oid
               AND c.relnamespace = n.oid
               AND n.nspname NOT IN ('pg_catalog', 'information_schema', $1)
        ),
        /*
        -- CTE based on information_schema.columns
        -- changed:
        -- remove the owner filter
        -- limit columns to the ones in the api schema or PK/FK columns
        */
        columns AS (
            SELECT current_database()::information_schema.sql_identifier AS table_catalog,
                nc.nspname::information_schema.sql_identifier AS table_schema,
                c.relname::information_schema.sql_identifier AS table_name,
                a.attname::information_schema.sql_identifier AS column_name,
                d.description::information_schema.sql_identifier AS description,
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
                            ELSE format_type(a.atttypid, a.atttypmod)
                        END
                        ELSE
                        CASE
                            WHEN t.typelem <> 0::oid AND t.typlen = (-1) THEN 'ARRAY'::text
                            WHEN nt.nspname = 'pg_catalog'::name THEN format_type(a.atttypid, NULL::integer)
                            ELSE format_type(a.atttypid, a.atttypmod)
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
               LEFT JOIN key_columns kc ON kc.conkey = a.attnum AND kc.c_oid = a.attrelid
               LEFT JOIN pg_catalog.pg_description AS d ON d.objoid = a.attrelid and d.objsubid = a.attnum
               LEFT JOIN pg_attrdef ad ON a.attrelid = ad.adrelid AND a.attnum = ad.adnum
               JOIN (pg_class c
               JOIN pg_namespace nc ON c.relnamespace = nc.oid) ON a.attrelid = c.oid
               JOIN (pg_type t
               JOIN pg_namespace nt ON t.typnamespace = nt.oid) ON a.atttypid = t.oid
               LEFT JOIN (pg_type bt
               JOIN pg_namespace nbt ON bt.typnamespace = nbt.oid) ON t.typtype = 'd'::"char" AND t.typbasetype = bt.oid
               LEFT JOIN (pg_collation co
               JOIN pg_namespace nco ON co.collnamespace = nco.oid) ON a.attcollation = co.oid AND (nco.nspname <> 'pg_catalog'::name OR co.collname <> 'default'::name)
            WHERE
                NOT pg_is_other_temp_schema(nc.oid)
                AND a.attnum > 0
                AND NOT a.attisdropped
                AND (c.relkind = ANY (ARRAY['r'::"char", 'v'::"char", 'f'::"char", 'm'::"char"]))
                AND (nc.nspname = $1 OR kc.r_oid IS NOT NULL) /*--filter only columns that are FK/PK or in the api schema */
              /*--AND (pg_has_role(c.relowner, 'USAGE'::text) OR has_column_privilege(c.oid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES'::text))*/
        )
        SELECT
            table_schema,
            table_name,
            column_name,
            description,
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
    ORDER BY schema, position |]

columnFromRow :: [Table] ->
                 (Text,        Text,        Text,
                  Maybe Text,  Int32,       Bool,
                  Text,        Bool,        Maybe Int32,
                  Maybe Int32, Maybe Text,  Maybe Text)
                 -> Maybe Column
columnFromRow tabs (s, t, n, desc, pos, nul, typ, u, l, p, d, e) = buildColumn <$> table
  where
    buildColumn tbl = Column tbl n desc pos nul typ u l p d (parseEnum e) Nothing
    table = find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs
    parseEnum :: Maybe Text -> [Text]
    parseEnum str = fromMaybe [] $ split (==',') <$> str

allChildRelations :: [Table] -> [Column] -> H.Statement () [Relation]
allChildRelations tabs cols =
  H.Statement sql HE.unit (decodeRelations tabs cols) True
 where
  sql = [q|
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
    ORDER BY (conrelid, column_info.nums) |]

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

allPrimaryKeys :: [Table] -> H.Statement () [PrimaryKey]
allPrimaryKeys tabs =
  H.Statement sql HE.unit (decodePks tabs) True
 where
  sql = [q|
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
        kc.table_schema NOT IN ('pg_catalog', 'information_schema') |]

pkFromRow :: [Table] -> (Schema, Text, Text) -> Maybe PrimaryKey
pkFromRow tabs (s, t, n) = PrimaryKey <$> table <*> pure n
  where table = find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs

allSynonyms :: [Column] -> PgVersion -> H.Statement Schema [Synonym]
allSynonyms cols pgVer =
  H.Statement sql (HE.param HE.text) (decodeSynonyms cols) True
  -- query explanation at https://gist.github.com/steve-chavez/7ee0e6590cddafb532e5f00c46275569
  where
    subselectRegex :: Text
    subselectRegex | pgVer < pgVersion100 = ":subselect {.*?:constraintDeps <>} :location"
                   | otherwise = ":subselect {.*?:stmt_len 0} :location"
    sql = [qc|
      with
      views as (
        select
          n.nspname   as view_schema,
          c.relname   as view_name,
          r.ev_action as view_definition
        from pg_class c
        join pg_namespace n on n.oid = c.relnamespace
        join pg_rewrite r on r.ev_class = c.oid
        where (c.relkind = 'v'::char) and n.nspname = $1
      ),
      removed_subselects as(
        select
          view_schema, view_name,
          regexp_replace(view_definition, '{subselectRegex}', '', 'g') as x
        from views
      ),
      target_lists as(
        select
          view_schema, view_name,
          regexp_split_to_array(x, 'targetList') as x
        from removed_subselects
      ),
      last_target_list_wo_tail as(
        select
          view_schema, view_name,
          (regexp_split_to_array(x[array_upper(x, 1)], ':onConflict'))[1] as x
        from target_lists
      ),
      target_entries as(
        select
          view_schema, view_name,
          unnest(regexp_split_to_array(x, 'TARGETENTRY')) as entry
        from last_target_list_wo_tail
      ),
      results as(
        select
          view_schema, view_name,
          substring(entry from ':resname (.*?) :') as view_colum_name,
          substring(entry from ':resorigtbl (.*?) :') as resorigtbl,
          substring(entry from ':resorigcol (.*?) :') as resorigcol
        from target_entries
      )
      select
        sch.nspname as table_schema,
        tbl.relname as table_name,
        col.attname as table_column_name,
        res.view_schema,
        res.view_name,
        res.view_colum_name
      from results res
      join pg_class tbl on tbl.oid::text = res.resorigtbl
      join pg_attribute col on col.attrelid = tbl.oid and col.attnum::text = res.resorigcol
      join pg_namespace sch on sch.oid = tbl.relnamespace
      where resorigtbl <> '0'
      order by view_schema, view_name, view_colum_name; |]

synonymFromRow :: [Column] -> (Text,Text,Text,Text,Text,Text) -> Maybe Synonym
synonymFromRow allCols (s1,t1,c1,s2,t2,c2) = (,) <$> col1 <*> col2
  where
    col1 = findCol s1 t1 c1
    col2 = findCol s2 t2 c2
    findCol s t c = find (\col -> (tableSchema . colTable) col == s && (tableName . colTable) col == t && colName col == c) allCols

getPgVersion :: H.Session PgVersion
getPgVersion = H.statement () $ H.Statement sql HE.unit versionRow False
  where
    sql = "SELECT current_setting('server_version_num')::integer, current_setting('server_version')"
    versionRow = HD.singleRow $ PgVersion <$> HD.column HD.int4 <*> HD.column HD.text
