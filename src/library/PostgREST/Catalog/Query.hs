{-# LANGUAGE QuasiQuotes #-}

module PostgREST.Catalog.Query
  ( accessibleFuncs
  , accessibleTables
  , allFunctions
  , allM2OandO2ORels
  , allTables
  , allViewsKeyDependencies
  )
where

import Data.Functor.Contravariant ((>$<))
import NeatInterpolation (trimming)
import Protolude

import PostgREST.Catalog.Identifiers (escapeIdent)
import PostgREST.Catalog.Relationship (Relationship, ViewKeyDependency (..))
import PostgREST.Catalog.Routine (RoutineMap)
import PostgREST.Catalog.Table (TablesMap)
import PostgREST.Config (AppConfig (..))
import PostgREST.Config.PgVersion (PgVersion, pgVersion170)

import Hasql.DynamicStatements.Snippet qualified as SQL
import Hasql.Encoders qualified as HE
import Hasql.Statement qualified as Statement
import PostgREST.Catalog.Decoders qualified as Decoders

baseTypesCte :: PgVersion -> Text
baseTypesCte pgVer
  | pgVer >= pgVersion170 =
      [trimming|
      -- Get base types using pg_basetype() (PG 17+)
      base_types AS (
        SELECT
          t.oid,
          bt.typnamespace AS base_namespace,
          bt.oid AS base_type
        FROM pg_type t
        JOIN pg_type bt ON bt.oid = pg_basetype(t.oid)
      )
    |]
  | otherwise =
      [trimming|
      -- Recursively get the base types of domains (PG < 17)
      base_types AS (
        WITH RECURSIVE
        recurse AS (
          SELECT
            oid,
            typbasetype,
            typnamespace AS base_namespace,
            COALESCE(NULLIF(typbasetype, 0), oid) AS base_type
          FROM pg_type
          UNION
          SELECT
            t.oid,
            b.typbasetype,
            b.typnamespace AS base_namespace,
            COALESCE(NULLIF(b.typbasetype, 0), b.oid) AS base_type
          FROM recurse t
          JOIN pg_type b ON t.typbasetype = b.oid
        )
        SELECT
          oid,
          base_namespace,
          base_type
        FROM recurse
        WHERE typbasetype = 0
      )
    |]

accessibleTables :: Text -> SQL.Snippet
accessibleTables schema =
  SQL.sql
    ( encodeUtf8
        [trimming|
  SELECT
    n.nspname AS table_schema,
    c.relname AS table_name
  FROM pg_class c
  JOIN pg_namespace n ON n.oid = c.relnamespace
  WHERE c.relkind IN ('v','r','m','f','p')
  AND c.relnamespace = quote_ident(|]
    )
    <> encodedSchema
    <> SQL.sql
      ( encodeUtf8
          [trimming|
  )::regnamespace AND (
    pg_has_role(c.relowner, 'USAGE')
    or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
    or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
  )
  AND not c.relispartition
  ORDER BY table_schema, table_name|]
      )
  where
    encodedSchema = SQL.encoderAndParam (HE.nonNullable HE.text) schema

accessibleFuncs :: PgVersion -> Text -> SQL.Snippet
accessibleFuncs pgVer schema = baseFuncSqlQuery pgVer <> "AND p.pronamespace = quote_ident(" <> encodedSchema <> ")::regnamespace"
  where
    encodedSchema = SQL.encoderAndParam (HE.nonNullable HE.text) schema

-- | SQL query to get accessible functions for OpenAPI.
baseFuncSqlQuery :: PgVersion -> SQL.Snippet
baseFuncSqlQuery pgVer =
  let baseCte = baseTypesCte pgVer
  in  SQL.sql $
        encodeUtf8
          [trimming|
  WITH
  $baseCte,
  arguments AS (
    SELECT
      oid,
      array_agg((
        COALESCE(name, ''), -- name
        type::regtype::text,
        CASE type
          WHEN 'bit'::regtype THEN 'bit varying'
          WHEN 'bit[]'::regtype THEN 'bit varying[]'
          WHEN 'character'::regtype THEN 'character varying'
          WHEN 'character[]'::regtype THEN 'character varying[]'
          ELSE type::regtype::text
        END,
        idx <= (pronargs - pronargdefaults),
        COALESCE(mode = 'v', FALSE)
      ) ORDER BY idx) AS args,
      CASE COUNT(*) - COUNT(name)
        WHEN 0 THEN true
        WHEN 1 THEN (array_agg(type))[1] IN ('bytea'::regtype, 'json'::regtype, 'jsonb'::regtype, 'text'::regtype, 'xml'::regtype)
        ELSE false
      END AS callable
    FROM pg_proc,
         unnest(proargnames, proargtypes, proargmodes)
           WITH ORDINALITY AS _ (name, type, mode, idx)
    WHERE type IS NOT NULL
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
     or COALESCE(proargmodes::text[] && '{t,b,o}', false)
    ) AS rettype_is_composite,
    bt.oid <> bt.base_type as rettype_is_composite_alias,
    p.provolatile,
    p.provariadic > 0 as hasvariadic,
    'ignored' AS transaction_isolation_level,
    '{}'::text[] as kvs
  FROM pg_proc p
  LEFT JOIN arguments a ON a.oid = p.oid
  JOIN pg_namespace pn ON pn.oid = p.pronamespace
  JOIN base_types bt ON bt.oid = p.prorettype
  JOIN pg_type t ON t.oid = bt.base_type
  JOIN pg_namespace tn ON tn.oid = t.typnamespace
  LEFT JOIN pg_class comp ON comp.oid = t.typrelid
  LEFT JOIN pg_description as d ON d.objoid = p.oid AND d.classoid = 'pg_proc'::regclass
  WHERE t.oid <> 'trigger'::regtype AND COALESCE(a.callable, true)
  AND has_function_privilege(p.oid, 'execute')
  AND prokind = 'f' |]

funcsSqlQuery :: PgVersion -> ByteString
funcsSqlQuery pgVer =
  let baseCte = baseTypesCte pgVer
  in  encodeUtf8
        [trimming|
  WITH
  $baseCte,
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
        END, -- convert types that ignore the length and accept any value till maximum size
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
    bt.oid <> bt.base_type as rettype_is_composite_alias,
    p.provolatile,
    p.provariadic > 0 as hasvariadic,
    (regexp_split_to_array((regexp_split_to_array(iso_config, '='))[2], ','))[1] AS transaction_isolation_level,
    coalesce(func_settings.kvs, '{}') as kvs
  FROM pg_proc p
  LEFT JOIN arguments a ON a.oid = p.oid
  JOIN pg_namespace pn ON pn.oid = p.pronamespace
  JOIN base_types bt ON bt.oid = p.prorettype
  JOIN pg_type t ON t.oid = bt.base_type
  JOIN pg_namespace tn ON tn.oid = t.typnamespace
  LEFT JOIN pg_class comp ON comp.oid = t.typrelid
  LEFT JOIN pg_description as d ON d.objoid = p.oid AND d.classoid = 'pg_proc'::regclass
  LEFT JOIN LATERAL unnest(proconfig) iso_config ON iso_config LIKE 'default_transaction_isolation%'
  LEFT JOIN LATERAL (
    SELECT
      array_agg(row(
        substr(setting, 1, strpos(setting, '=') - 1),
        substr(setting, strpos(setting, '=') + 1)
      )) as kvs
    FROM unnest(proconfig) setting
    WHERE setting ~ ANY($$2)
  ) func_settings ON TRUE
  WHERE t.oid <> 'trigger'::regtype AND COALESCE(a.callable, true)
  AND prokind = 'f'
  AND p.pronamespace = ANY($$1::regnamespace[]) |]

-- | Gets tables with their PK cols
tablesSqlQuery :: PgVersion -> ByteString
tablesSqlQuery pgVer =
  -- the tbl_constraints/key_col_usage CTEs are based on the standard "information_schema.table_constraints"/"information_schema.key_column_usage" views,
  -- we cannot use those directly as they include the following privilege filter:
  -- (pg_has_role(ss.relowner, 'USAGE'::text) OR has_column_privilege(ss.roid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES'::text));
  -- on the "columns" CTE, left joining on pg_depend and pg_class is used to obtain the sequence name as a column default in case there are GENERATED .. AS IDENTITY,
  -- generated columns are only available from pg >= 10 but the query is agnostic to versions. dep.deptype = 'i' is done because there are other 'a' dependencies on PKs
  let baseCte = baseTypesCte pgVer
  in  encodeUtf8
        [trimming|
  WITH
  $baseCte,
  columns AS (
      SELECT
          c.oid AS relid,
          a.attname::name AS column_name,
          d.description AS description,
          -- typbasetype and typdefaultbin handles `CREATE DOMAIN .. DEFAULT val`,  attidentity/attgenerated handles generated columns, pg_get_expr gets the default of a column
          CASE
            WHEN (t.typbasetype != 0) AND (ad.adbin IS NULL) THEN pg_get_expr(t.typdefaultbin, 0)
            WHEN a.attidentity  = 'd' THEN format('nextval(%L)', seq.objid::regclass)
            WHEN a.attgenerated = 's' THEN null
            ELSE pg_get_expr(ad.adbin, ad.adrelid)::text
          END AS column_default,
          not (a.attnotnull OR t.typtype = 'd' AND t.typnotnull) AS is_nullable,
          CASE
              WHEN t.typtype = 'd' THEN
              CASE
                  WHEN bt.base_namespace = 'pg_catalog'::regnamespace THEN format_type(bt.base_type, NULL::integer)
                  ELSE format_type(a.atttypid, a.atttypmod)
              END
              ELSE
              CASE
                  WHEN t.typnamespace = 'pg_catalog'::regnamespace THEN format_type(a.atttypid, NULL::integer)
                  ELSE format_type(a.atttypid, a.atttypmod)
              END
          END::text AS data_type,
          format_type(a.atttypid, a.atttypmod)::text AS nominal_data_type,
          information_schema._pg_char_max_length(
              information_schema._pg_truetypid(a.*, t.*),
              information_schema._pg_truetypmod(a.*, t.*)
          )::integer AS character_maximum_length,
          bt.base_type,
          a.attnum::integer AS position
      FROM pg_attribute a
          LEFT JOIN pg_description AS d
              ON d.objoid = a.attrelid and d.objsubid = a.attnum and d.classoid = 'pg_class'::regclass
          LEFT JOIN pg_attrdef ad
              ON a.attrelid = ad.adrelid AND a.attnum = ad.adnum
          JOIN pg_class c
              ON a.attrelid = c.oid
          JOIN pg_type t
              ON a.atttypid = t.oid
          LEFT JOIN base_types bt
              ON t.oid = bt.oid
          LEFT JOIN pg_depend seq
              ON seq.refobjid = a.attrelid and seq.refobjsubid = a.attnum and seq.deptype = 'i'
      WHERE
          NOT pg_is_other_temp_schema(c.relnamespace)
          AND a.attnum > 0
          AND NOT a.attisdropped
          AND c.relkind in ('r', 'v', 'f', 'm', 'p')
          AND c.relnamespace = ANY($$1::regnamespace[])
  ),
  columns_agg AS (
    SELECT
      relid,
      array_agg(row(
        column_name,
        description,
        is_nullable::boolean,
        data_type,
        nominal_data_type,
        character_maximum_length,
        column_default,
        coalesce(
          (SELECT array_agg(enumlabel ORDER BY enumsortorder) FROM pg_enum WHERE enumtypid = base_type),
          '{}'
        )
      ) order by position) as columns
    FROM columns
    GROUP BY relid
  ),
  tbl_pk_cols AS (
    SELECT
      r.oid AS relid,
      array_agg(a.attname ORDER BY a.attname) AS pk_cols
    FROM pg_class r
    JOIN pg_constraint c
      ON r.oid = c.conrelid
    JOIN pg_attribute a
      ON a.attrelid = r.oid AND a.attnum = ANY (c.conkey)
    WHERE
      c.contype in ('p')
      AND r.relkind IN ('r', 'p')
      AND r.relnamespace NOT IN ('pg_catalog'::regnamespace, 'information_schema'::regnamespace)
      AND NOT pg_is_other_temp_schema(r.relnamespace)
      AND NOT a.attisdropped
    GROUP BY r.oid
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
  LEFT JOIN pg_description d on d.objoid = c.oid and d.objsubid = 0 and d.classoid = 'pg_class'::regclass
  LEFT JOIN tbl_pk_cols tpks ON c.oid = tpks.relid
  LEFT JOIN columns_agg cols_agg ON c.oid = cols_agg.relid
  WHERE c.relkind IN ('v','r','m','f','p')
  AND c.relnamespace NOT IN ('pg_catalog'::regnamespace, 'information_schema'::regnamespace)
  AND not c.relispartition
  ORDER BY table_schema, table_name|]

-- We use jsonb_agg for comparing the uniques/pks instead of array_agg to avoid the ERROR:  cannot accumulate arrays of different dimensionality
allM2OandO2ORelsQuery :: ByteString
allM2OandO2ORelsQuery =
  encodeUtf8
    [trimming|
    WITH
    pks_uniques_cols AS (
      SELECT
        conrelid,
        array_agg(key order by key) as cols
      FROM pg_constraint,
      LATERAL unnest(conkey) AS _(key)
      WHERE
        contype IN ('p', 'u')
        AND connamespace <> 'pg_catalog'::regnamespace
      GROUP BY oid, conrelid
    )
    SELECT
      ns1.nspname AS table_schema,
      tab.relname AS table_name,
      ns2.nspname AS foreign_table_schema,
      other.relname AS foreign_table_name,
      traint.conrelid = traint.confrelid AS is_self,
      traint.conname  AS constraint_name,
      column_info.cols_and_fcols,
      (column_info.cols IN (SELECT cols FROM pks_uniques_cols WHERE conrelid = traint.conrelid)) AS one_to_one
    FROM pg_constraint traint
    JOIN LATERAL (
      SELECT
        array_agg(row(cols.attname, refs.attname) order by ord) AS cols_and_fcols,
        array_agg(cols.attnum order by cols.attnum) AS cols
      FROM unnest(traint.conkey, traint.confkey) WITH ORDINALITY AS _(col, ref, ord)
      JOIN pg_attribute cols ON cols.attrelid = traint.conrelid AND cols.attnum = col
      JOIN pg_attribute refs ON refs.attrelid = traint.confrelid AND refs.attnum = ref
    ) AS column_info ON TRUE
    JOIN pg_namespace ns1 ON ns1.oid = traint.connamespace
    JOIN pg_class tab ON tab.oid = traint.conrelid
    JOIN pg_class other ON other.oid = traint.confrelid
    JOIN pg_namespace ns2 ON ns2.oid = other.relnamespace
    WHERE traint.contype = 'f'
    AND traint.conparentid = 0
    ORDER BY traint.conrelid, traint.conname|]

-- query explanation at:
--  * rationale: https://gist.github.com/wolfgangwalther/5425d64e7b0d20aad71f6f68474d9f19
--  * json transformation: https://gist.github.com/wolfgangwalther/3a8939da680c24ad767e93ad2c183089
allViewsKeyDependenciesQuery :: ByteString
allViewsKeyDependenciesQuery =
  encodeUtf8
    [trimming|
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
          c.oid          as view_id,
          c.relnamespace as view_schema_id,
          n.nspname      as view_schema,
          c.relname      as view_name,
          r.ev_action    as view_definition
        from pg_class c
        join pg_namespace n on n.oid = c.relnamespace
        join pg_rewrite r on r.ev_class = c.oid
        where c.relkind in ('v', 'm') and c.relnamespace = ANY($$1::regnamespace[] || $$2::regnamespace[])
      ),
      transform_json as (
        select
          view_id, view_schema_id, view_schema, view_name,
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
            -- add an empty key for the following node.
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
          view_id, view_schema_id, view_schema, view_name,
          json_array_elements(view_definition->0->'targetList') as entry
        from transform_json
      ),
      results as(
        select
          view_id, view_schema_id, view_schema, view_name,
          (entry->>'resno')::int as view_column,
          (entry->>'resorigtbl')::oid as resorigtbl,
          (entry->>'resorigcol')::int as resorigcol
        from target_entries
      ),
      -- CYCLE detection according to PG docs: https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-CYCLE
      -- Can be replaced with CYCLE clause once PG v13 is EOL.
      recursion(view_id, view_schema_id, view_schema, view_name, view_column, resorigtbl, resorigcol, is_cycle, path) as(
        select
          r.*,
          false,
          ARRAY[resorigtbl]
        from results r
        where view_schema_id = ANY ($$1::regnamespace[])
        union all
        select
          view.view_id,
          view.view_schema_id,
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
      group by sch.nspname, tbl.relname, rep.view_schema, rep.view_name, pks_fks.conname, pks_fks.contype, pks_fks.ncol
      -- make sure we only return key for which all columns are referenced in the view - no partial PKs or FKs
      having ncol = array_length(array_agg(row(col.attname, view_columns) order by pks_fks.ord), 1)
      |]

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable

arrayParam :: HE.Value a -> HE.Params [a]
arrayParam = param . HE.foldableArray . HE.nonNullable

allViewsKeyDependencies :: Statement.Statement AppConfig [ViewKeyDependency]
allViewsKeyDependencies = Statement.Statement allViewsKeyDependenciesQuery params Decoders.decodeViewKeyDeps True
  where
    params =
      (map escapeIdent . toList . configDbSchemas >$< arrayParam HE.text)
        <> (map escapeIdent . toList . configDbExtraSearchPath >$< arrayParam HE.text)

allM2OandO2ORels :: Statement.Statement () [Relationship]
allM2OandO2ORels = Statement.Statement allM2OandO2ORelsQuery HE.noParams Decoders.decodeRels True

allFunctions :: PgVersion -> Bool -> Statement.Statement AppConfig RoutineMap
allFunctions pgVer = Statement.Statement (funcsSqlQuery pgVer) params Decoders.decodeFuncs
  where
    params = (map escapeIdent . toList . configDbSchemas >$< arrayParam HE.text) <> (configDbHoistedTxSettings >$< arrayParam HE.text)

allTables :: PgVersion -> Bool -> Statement.Statement AppConfig TablesMap
allTables pgVer = Statement.Statement (tablesSqlQuery pgVer) params Decoders.decodeTables
  where
    params = map escapeIdent . toList . configDbSchemas >$< arrayParam HE.text
