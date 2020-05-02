with


  -- Postgres version

  pg_version as (
    SELECT
      current_setting('server_version_num')::integer as pgv_num,
      current_setting('server_version') as pgv_name
  ),


  -- Procedures

  procs as (
    SELECT
      pn.nspname as proc_schema,
      p.proname as proc_name,
      d.description as proc_description,
      pg_get_function_arguments(p.oid) as proc_args,
      tn.nspname as proc_return_type_schema,
      coalesce(comp.relname, t.typname) as proc_return_type_name,
      p.proretset as proc_return_type_is_setof,
      t.typtype as proc_return_type,
      p.provolatile as proc_volatility,
      has_function_privilege(p.oid, 'execute') as proc_is_accessible
    FROM pg_proc p
      JOIN pg_namespace pn ON pn.oid = p.pronamespace
      JOIN pg_type t ON t.oid = p.prorettype
      JOIN pg_namespace tn ON tn.oid = t.typnamespace
      LEFT JOIN pg_class comp ON comp.oid = t.typrelid
      LEFT JOIN pg_catalog.pg_description as d on d.objoid = p.oid
    WHERE
      pn.nspname = any ($1)
  ),


  -- Schema description

  schema_description as (
      select
        description
      from
        pg_catalog.pg_namespace n
        left join pg_catalog.pg_description d on d.objoid = n.oid
      where
        n.nspname = any ($1)
  ),



  -- Tables

  tables as (
     SELECT
       n.nspname as table_schema,
       c.relname as table_name,
       d.description as table_description,
       (
         c.relkind in ('r', 'v', 'f')
         and (pg_relation_is_updatable(c.oid::regclass, false) & 8) = 8
         -- The function `pg_relation_is_updateable` returns a bitmask where 8
         -- corresponds to `1 << CMD_INSERT` in the PostgreSQL source code, i.e.
         -- it's possible to insert into the relation.
         or exists (
           select 1
           from pg_trigger
           where
             pg_trigger.tgrelid = c.oid
             and (pg_trigger.tgtype::integer & 69) = 69
             -- The trigger type `tgtype` is a bitmask where 69 corresponds to
             -- TRIGGER_TYPE_ROW + TRIGGER_TYPE_INSTEAD + TRIGGER_TYPE_INSERT
             -- in the PostgreSQL source code.
         )
       ) as table_insertable,
       (
         pg_has_role(c.relowner, 'USAGE')
         or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
         or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
       ) as table_is_accessible
     FROM pg_class c
     JOIN pg_namespace n ON n.oid = c.relnamespace
     left join pg_catalog.pg_description as d on d.objoid = c.oid and d.objsubid = 0
     WHERE c.relkind IN ('v','r','m','f')
       AND n.nspname NOT IN ('pg_catalog', 'information_schema')
     ORDER BY table_schema, table_name
  ),


  -- Columns

  columns as (
     SELECT DISTINCT
         col_table as col_table,
         info.column_name AS col_name,
         info.table_schema AS col_schema,
         info.table_name as col_table_name,
         info.description AS col_description,
         info.ordinal_position AS col_position,
         info.is_nullable::boolean AS col_nullable,
         info.data_type AS col_type,
         info.is_updatable::boolean AS col_updatable,
         info.character_maximum_length AS col_max_len,
         info.numeric_precision AS col_precision,
         info.column_default AS col_default,
         coalesce(enum_info.vals, array[]::text[]) AS col_enum
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
                AND c.relkind IN ('r', 'v', 'f', 'm')
                AND r.conrelid = c.oid
                AND c.relnamespace = n.oid
                AND n.nspname <> ANY (ARRAY['pg_catalog', 'information_schema'] || $1)
         ),
         -- CTE based on information_schema.columns
         columns AS (
             SELECT
                 nc.nspname::name AS table_schema,
                 c.relname::name AS table_name,
                 a.attname::name AS column_name,
                 d.description AS description,
                 a.attnum::integer AS ordinal_position,
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
                 information_schema._pg_numeric_precision(
                     information_schema._pg_truetypid(a.*, t.*),
                     information_schema._pg_truetypmod(a.*, t.*)
                 )::integer AS numeric_precision,
                 COALESCE(bt.typname, t.typname)::name AS udt_name,
                 (
                     c.relkind in ('r', 'v', 'f')
                     AND pg_column_is_updatable(c.oid::regclass, a.attnum, false)
                 )::bool is_updatable
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
                 AND c.relkind in ('r', 'v', 'f', 'm')
                 -- Filter only columns that are FK/PK or in the api schema:
                 AND (nc.nspname = ANY ($1) OR kc.r_oid IS NOT NULL)
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
     , lateral (
        select
          -- explicit columns needed for Postgres < 10
          table_schema::text,
          table_name::text,
          table_description::text,
          table_insertable::bool,
          table_is_accessible::bool
        from tables
        where
          tables.table_schema::text = info.table_schema::text
          and tables.table_name::text = info.table_name::text
     ) col_table
     ORDER BY col_schema, col_position
  ),


  -- M2O relations

  m2o_rels as (
     SELECT
        rel_table,
        rel_columns.array_agg as rel_columns,
        conname as rel_constraint,
        rel_f_table,
        rel_f_columns.array_agg as rel_f_columns,
        'M2O' as rel_type,
        null as rel_junction
     FROM pg_constraint,
     LATERAL (
       SELECT array_agg(cols.attname) AS cols,
                     array_agg(cols.attnum)  AS nums,
                     array_agg(refs.attname) AS refs
       FROM (
          SELECT unnest(conkey) AS col, unnest(confkey) AS ref) k,
       LATERAL (SELECT * FROM pg_attribute WHERE attrelid = conrelid AND attnum = col) AS cols,
       LATERAL (SELECT * FROM pg_attribute WHERE attrelid = confrelid AND attnum = ref) AS refs) AS column_info,
     LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = connamespace) AS ns1,
     LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = conrelid) AS tab,
     LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = confrelid) AS other,
     LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = other.relnamespace) AS ns2
         , lateral (
            select * from tables
            where
              tables.table_schema::text = ns1.nspname::text
              and tables.table_name::text = tab.relname::text
         ) rel_table
         , lateral (
            select * from tables
            where
              tables.table_schema::text = ns2.nspname::text
              and tables.table_name::text = other.relname::text
         ) rel_f_table
         , lateral (
            select array_agg(columns) from columns
            where
              col_schema = ns1.nspname
              and col_table_name = tab.relname
              and col_name = any (column_info.cols)
         ) rel_columns
         , lateral (
            select array_agg(columns) from columns
            where
              col_schema = ns2.nspname
              and col_table_name = other.relname
              and col_name = any (column_info.refs)
         ) rel_f_columns
     WHERE confrelid != 0
     ORDER BY (conrelid, column_info.nums)
  ),


  -- Primary keys

  primary_keys as (
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
             AND r.relkind = 'r'
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
                 AND r.relkind = 'r'
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
         kc.column_name as pk_name,
         pk_table
     FROM
         tc, kc
         , lateral (
            select *
            from tables
            where
              tables.table_schema::text = kc.table_schema::text
              and tables.table_name::text = kc.table_name::text
         ) pk_table
     WHERE
         kc.table_name = tc.table_name AND
         kc.table_schema = tc.table_schema AND
         kc.constraint_name = tc.constraint_name AND
         kc.table_schema NOT IN ('pg_catalog', 'information_schema')
  ),


  -- Source columns

  -- query explanation at https://gist.github.com/steve-chavez/7ee0e6590cddafb532e5f00c46275569

  source_columns as (
       with
       views as (
         select
           n.nspname   as view_schema,
           c.relname   as view_name,
           r.ev_action as view_definition
         from pg_class c
         join pg_namespace n on n.oid = c.relnamespace
         join pg_rewrite r on r.ev_class = c.oid
         where c.relkind in ('v', 'm') and n.nspname = ANY ($1)
       ),
       removed_subselects as(
         select
           view_schema, view_name,
           regexp_replace(view_definition,
             -- "result" appears when the subselect is used inside "case when", see
             -- `authors_have_book_in_decade` fixture
             -- "resno"  appears in every other case
             case when (select pgv_num from pg_version) < 100000
                then ':subselect {.*?:constraintDeps <>} :location \d+} :res(no|ult)'
                else ':subselect {.*?:stmt_len 0} :location \d+} :res(no|ult)'
             end,
           '', 'g') as x
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
         res.view_colum_name,
         source_column.c1 as src_source,
         view_column.c2 as src_view
       from results res
       join pg_class tbl on tbl.oid::text = res.resorigtbl
       join pg_attribute col on col.attrelid = tbl.oid and col.attnum::text = res.resorigcol
       join pg_namespace sch on sch.oid = tbl.relnamespace,
       lateral (
            select c1
            from columns c1
            where
                sch.nspname = c1.col_schema
                and tbl.relname = c1.col_table_name
                and col.attname = c1.col_name
       ) source_column,
       lateral (
            select c2
            from columns c2
            where
                res.view_schema = c2.col_schema
                and res.view_name = c2.col_table_name
                and res.view_colum_name = c2.col_name
       ) as view_column
       where resorigtbl <> '0'
       order by view_schema, view_name, view_colum_name
  )


  -- Main query

  select
    json_build_object(
        'raw_db_procs', coalesce(procs_agg.array_agg, array[]::record[]),
        'raw_db_schema_descriptions', schema_description_agg.array_agg,
        'raw_db_tables', coalesce(tables_agg.array_agg, array[]::record[]),
        'raw_db_columns', columns_agg.array_agg,
        'raw_db_m2o_rels', coalesce(m2o_rels_agg.array_agg, array[]::record[]),
        'raw_db_primary_keys', primary_keys_agg.array_agg,
        'raw_db_source_columns', coalesce(source_columns_agg.array_agg, array[]::record[]),
        'raw_db_pg_ver', pg_version
    )::json as dbstructure
  from
    (select array_agg(procs) from procs) procs_agg,
    (select array_agg(schema_description) from schema_description) schema_description_agg,
    (select array_agg(tables) from tables) as tables_agg,
    (select array_agg(columns) from columns) as columns_agg,
    (select array_agg(m2o_rels) from m2o_rels) as m2o_rels_agg,
    (select array_agg(primary_keys) from primary_keys) as primary_keys_agg,
    (select array_agg(source_columns) from source_columns) as source_columns_agg,
    pg_version
