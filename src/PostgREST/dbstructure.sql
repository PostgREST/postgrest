with
  -- Postgres version

  pg_version as (
    select
      current_setting('server_version_num')::integer as pgv_num,
      current_setting('server_version') as pgv_name
  ),


  -- Schema descriptions

  schemas as (
    select
      n.oid as schema_oid,
      n.nspname as schema_name,
      description as schema_description
    from
      pg_namespace n
      left join pg_description d on d.objoid = n.oid
    where
      n.nspname = any ($1)
  ),


  -- Procedures

  procs as (
    select
      p.oid as proc_oid,
      pn.nspname as proc_schema,
      p.proname as proc_name,
      d.description as proc_description,
      json_build_object(
        'qi_schema', tn.nspname,
        'qi_name', coalesce(comp.relname, t.typname)
      ) as proc_return_type_qi,
      p.proretset as proc_return_type_is_setof,
      (
        t.typtype = 'c'
        or (t.typtype = 'p' and coalesce(comp.relname, t.typname) = 'record')
        -- Only pg pseudo type that is a row type is 'record'
      ) as proc_return_type_is_composite,
      p.provolatile as proc_volatility,
      has_function_privilege(p.oid, 'execute') as proc_is_accessible,
      coalesce(array(
        select
          json_build_object(
            'pga_name', parsed.name,
            'pga_type', parsed.typ,
            'pga_req', not parsed.has_default
          )
        from
          regexp_split_to_table(pg_get_function_arguments(p.oid), ', ') as args
          , regexp_split_to_array(args, ' DEFAULT ') as args_with_default
          , regexp_matches(
              args_with_default[1],
              '^(IN |INOUT |OUT |)(([^\" ]\S*?)|\"(\S+?)\")( (.+?))?$'
          ) as groups
          , lateral (
              select
                groups[1] as inout,
                coalesce(groups[3], groups[4]) as name,
                coalesce(groups[6], '') as typ,
                args_with_default[2] is not null as has_default
          ) as parsed
        where parsed.inout <> 'OUT '
      ), array[]::json[]) as proc_args
    from
      pg_proc p
      join pg_namespace pn on pn.oid = p.pronamespace
      join pg_type t on t.oid = p.prorettype
      join pg_namespace tn on tn.oid = t.typnamespace
      left join pg_class comp on comp.oid = t.typrelid
      left join pg_description as d on d.objoid = p.oid
    where
      pn.nspname = any ($1)
  ),


  -- Tables

  tables as (
    select
      c.oid as table_oid,
      n.nspname as table_schema,
      c.relname as table_name,
      d.description as table_description,
      (
        c.relkind in ('r', 'v', 'f')

        and pg_relation_is_updatable(c.oid, true) & 8 = 8

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
    from
      pg_class c
      join pg_namespace n
        on n.oid = c.relnamespace
      left join pg_catalog.pg_description as d
        on d.objoid = c.oid and d.objsubid = 0
    where
      c.relkind in ('v','r','m','f')
      and n.nspname not in ('pg_catalog'::name, 'information_schema'::name)
  ),


  -- Source columns, query explanation at
  -- https://gist.github.com/steve-chavez/7ee0e6590cddafb532e5f00c46275569

  view_col_rels as (
    select
      c.oid as view_oid,
      targetentries.attnum as view_col_position,
      targetentries.table_oid,
      targetentries.table_col_position
    from
      pg_class c
      join pg_namespace n on n.oid = c.relnamespace
      join pg_rewrite r on r.ev_class = c.oid
      join pg_attribute a on a.attrelid = c.oid
      , lateral (
          select
            matches[1]::smallint as attnum,
            matches[2] as attname,
            matches[3]::oid as table_oid,
            matches[3]::smallint as table_col_position
          from
            regexp_matches(
              r.ev_action,
              ':resno (\d+) :resname (\S+) :ressortgroupref \d+ :resorigtbl (\d+) :resorigcol (\d+)',
              'g'
            ) as matches
          where
            matches[3] <> '0'
        ) targetentries
    where
      c.relkind in ('v', 'm')
      and n.nspname = any ($1::name[])
      and targetentries.attnum = a.attnum
      and targetentries.attname = a.attname
/*
    select
      c.oid as view_oid,
      substring(entry from ':resno (\d+) ')::smallint as view_col_position,
      substring(entry from ':resorigtbl (\d+) ')::oid as table_oid,
      substring(entry from ':resorigcol (\d+) ')::smallint as table_col_position
    from
      pg_class c
      join pg_namespace n on n.oid = c.relnamespace
      join pg_rewrite r on r.ev_class = c.oid
      , (
          -- "result" appears when the subselect is used inside "case when", see
          -- `authors_have_book_in_decade` fixture
          -- "resno"  appears in every other case
          select
            case when (select pgv_num from pg_version) < 100000 then
              ':subselect {.*?:constraintDeps <>} :location \d+} :res(no|ult)'
            else
              ':subselect {.*?:stmt_len 0} :location \d+} :res(no|ult)'
            end as subselect
        ) as subselect
      , regexp_replace(r.ev_action, subselect.subselect, '', 'g') as x
      , regexp_split_to_array(x, 'targetList') as target_lists
      , regexp_split_to_array(
          target_lists[array_upper(target_lists, 1)], ':onConflict'
        ) last_target_list
      , unnest(regexp_split_to_array(last_target_list[1]::text, 'TARGETENTRY')) as entry
    where
      c.relkind in ('v', 'm')
      and n.nspname = any ($1::name[])
*/
  ),


  -- Primary keys

  table_primary_keys as (
    select
      r.oid as pk_table_oid,
      a.attnum as pk_col_position
    from
      pg_constraint c
      join pg_class r on r.oid = c.conrelid
      join pg_namespace nr on nr.oid = r.relnamespace
      join pg_attribute a on a.attrelid = r.oid
      , unnest(c.conkey) as con(key)
    where
      c.contype = 'p'
      and r.relkind = 'r'
      --and nr.nspname not in ('pg_catalog', 'information_schema')
      and nr.nspname = any($1::name[])
      and not pg_is_other_temp_schema(nr.oid)
      and a.attnum = con.key
      and not a.attisdropped
  ),

  view_primary_keys as (
    select
      view_cols.view_oid as pk_table_oid,
      view_col_position as pk_col_position
    from
      table_primary_keys pks
      join view_col_rels view_cols
        on view_cols.table_oid = pks.pk_table_oid
  ),

  primary_keys as (
    select * from table_primary_keys
    union all
    select * from view_primary_keys
  ),

  -- Columns

  columns as (
    select
      a.attrelid as col_table_oid,
      a.attnum as col_position,
      a.attname as col_name,
      d.description as col_description,
      pg_get_expr(ad.adbin, ad.adrelid)::text as col_default,
      not (a.attnotnull or t.typtype = 'd' and t.typnotnull) as col_nullable,
      case
        when t.typtype = 'd' then
          case
            when bt.typelem <> 0::oid and bt.typlen = (-1) then
              'ARRAY'::text
            when nbt.nspname = 'pg_catalog'::name then
              format_type(t.typbasetype, null::integer)
            else
              format_type(a.atttypid, a.atttypmod)
          end
        else
          case
            when t.typelem <> 0::oid and t.typlen = (-1) then
              'ARRAY'
            when nt.nspname = 'pg_catalog'::name then
              format_type(a.atttypid, null::integer)
            else
              format_type(a.atttypid, a.atttypmod)
          end
      end as col_type,
      information_schema._pg_char_max_length(truetypid, truetypmod) as col_max_len,
      information_schema._pg_numeric_precision(truetypid, truetypmod) as col_precision,
      (
        c.relkind in ('r', 'v', 'f')
        and pg_column_is_updatable(c.oid::regclass, a.attnum, false)
      ) col_updatable,
      coalesce(enum_info.vals, array[]::text[]) as col_enum,
      pks is not null as col_is_primary_key
    from
      pg_attribute a
      left join pg_description d on d.objoid = a.attrelid and d.objsubid = a.attnum
      left join pg_attrdef ad on a.attrelid = ad.adrelid and a.attnum = ad.adnum
      join pg_class c on c.oid = a.attrelid
      join pg_namespace nc on nc.oid = c.relnamespace
      join pg_type t on t.oid = a.atttypid
      join pg_namespace nt on t.typnamespace = nt.oid
      left join pg_type bt on t.typtype = 'd' and t.typbasetype = bt.oid
      left join pg_namespace nbt on bt.typnamespace = nbt.oid
      left join primary_keys pks
        on
          pks.pk_table_oid = a.attrelid
          and pks.pk_col_position = a.attnum
      , lateral (
          select array_agg(e.enumlabel order by e.enumsortorder) as vals
          from
            pg_type et
            join pg_enum e on et.oid = e.enumtypid
          where
            et.oid = t.oid
      ) as enum_info
      , information_schema._pg_truetypid(a.*, t.*) truetypid
      , information_schema._pg_truetypmod(a.*, t.*) truetypmod
    where
      a.attnum <> 0
      and not a.attisdropped
      and c.relkind in ('r', 'v', 'f', 'm')
      --and (nc.nspname = any ($1::name[]) or pks is not null)
      and nc.nspname = any ($1::name[])
      and not pg_is_other_temp_schema(c.relnamespace)
  ),


  -- M2O relations

  table_table_m2o_rel_cols as (
    select
      c.conname as rel_constraint,
      c.conrelid as rel_table_oid,
      c.confrelid as rel_f_table_oid,
      col_map.from_col,
      col_map.to_col
    from
      pg_constraint c
      join pg_class rel on rel.oid = c.conrelid
      join pg_class frel on frel.oid = c.confrelid
      , unnest(c.conkey, c.confkey) as col_map(from_col, to_col)
    where
      confrelid != 0
      and exists(
        select 1
        from schemas s
        where s.schema_oid in (rel.relnamespace, frel.relnamespace)
      )
  ),

  view_table_m2o_rel_cols as (
    select
      rel_constraint,
      view_col_rels.view_oid as rel_table_oid,
      rel_f_table_oid,
      view_col_rels.view_col_position as from_col,
      to_col
    from
      table_table_m2o_rel_cols rel_cols
      join view_col_rels
        on view_col_rels.table_oid = rel_cols.rel_table_oid
        and view_col_rels.table_col_position = rel_cols.from_col
  ),

  table_view_m2o_rel_cols as (
    select
      rel_constraint,
      rel_table_oid,
      view_col_rels.view_oid as rel_f_table_oid,
      from_col,
      view_col_rels.view_col_position as to_col
    from
      table_table_m2o_rel_cols rel_cols
      join view_col_rels
        on view_col_rels.table_oid = rel_cols.rel_f_table_oid
        and view_col_rels.table_col_position = rel_cols.to_col
  ),

  view_view_m2o_rel_cols as (
    select
      rel_constraint,
      left_view.view_oid as rel_table_oid,
      right_view.view_oid as rel_f_table_oid,
      left_view.view_col_position as from_col,
      right_view.view_col_position as to_col
    from
      table_table_m2o_rel_cols rel_cols
      join view_col_rels left_view
        on left_view.table_oid = rel_cols.rel_table_oid
        and left_view.table_col_position = rel_cols.from_col
      join view_col_rels right_view
        on right_view.table_oid = rel_cols.rel_f_table_oid
        and right_view.table_col_position = rel_cols.to_col
  ),

  m2o_rel_cols as (
    select * from table_table_m2o_rel_cols
    union all
    select * from view_table_m2o_rel_cols
    union all
    select * from table_view_m2o_rel_cols
    union all
    select * from view_view_m2o_rel_cols
/*  ),

  m2o_rels as (
    select
      rel_constraint,
      rel_table_oid,
      rel_f_table_oid,
      array_agg (
        array[from_col, to_col]
      ) as rel_col_map
    from
      m2o_rel_cols
    group by rel_constraint, rel_table_oid, rel_f_table_oid
    order by rel_table_oid, rel_col_map
  ),

  o2m_rels as (
    select
      rel_constraint,
      rel_f_table_oid as rel_table_oid,
      rel_table_oid as rel_f_table_oid,
      array_agg (
        array[to_col, from_col]
      ) as rel_col_map
    from
      m2o_rel_cols
    group by rel_constraint, rel_table_oid, rel_f_table_oid
    order by rel_table_oid, rel_col_map
*//*  ),

  m2m_rels as (
    select
      left_rels.rel_f_table_oid as rel_table_oid,
      right_rels.rel_f_table_oid as rel_f_table_oid,
      m2m_col_maps.m2m_col_map as rel_col_map,
      json_build_object(
        'jun_table_oid', left_rels.rel_table_oid,
        'jun_constraint1', left_rels.rel_constraint,
        'jun_constraint2', right_rels.rel_constraint,
        'jun_col_map', junction_col_maps.junction_col_map
      ) as rel_junction
    from
      m2o_rels left_rels
      , lateral (
          select array_agg(from_col order by from_col) as col_map
          from unnest(left_rels.rel_col_map) as col_map(from_col smallint, to_col smallint)
        ) left_cols
      , lateral (
          select array_agg(to_col order by from_col) as col_map
          from unnest(left_rels.rel_col_map) as col_map(from_col smallint, to_col smallint)
        ) left_f_cols
      , m2o_rels right_rels
      , lateral (
          select array_agg(from_col order by from_col) as col_map
          from unnest(right_rels.rel_col_map) as col_map(from_col smallint, to_col smallint)
        ) right_cols
      , lateral (
          select array_agg(to_col order by from_col) as col_map
          from unnest(right_rels.rel_col_map) as col_map(from_col smallint, to_col smallint)
        ) right_f_cols
      , lateral (
          select array_agg(m2m_col_map) as m2m_col_map
          from
            unnest(left_f_cols.col_map, right_f_cols.col_map) as m2m_col_map(from_col, to_col)
        ) as m2m_col_maps
      , lateral (
          select array_agg(junction_col_map) as junction_col_map
          from
            unnest(left_cols.col_map, right_cols.col_map) as junction_col_map(from_col, to_col)
        ) as junction_col_maps
    where
      left_rels.rel_table_oid = right_rels.rel_table_oid
      and left_cols <> right_cols
      and array_length(junction_col_maps.junction_col_map, 1) = 1

*/ /* ),

  rels as (
    select
      'M2O' as rel_type,
      rel_constraint,
      rel_table_oid,
      rel_f_table_oid,
      rel_col_map,
      null::json as rel_junction
    from m2o_rels
    union all
    select
      'O2M' as rel_type,
      rel_constraint,
      rel_table_oid,
      rel_f_table_oid,
      rel_col_map,
      null::json as rel_junction
    from o2m_rels
*//*
    union all
    select
      'M2M' as rel_type,
      null::text as rel_constraint,
      rel_table_oid,
      rel_f_table_oid,
      rel_col_map,
      rel_junction
    from m2m_rels
*/
  )

  -- Main query
select
  json_build_object(
    'raw_db_procs', coalesce(procs_agg.array_agg, array[]::record[]),
    'raw_db_schemas', coalesce(schemas_agg.array_agg, array[]::record[]),
    'raw_db_tables', coalesce(tables_agg.array_agg, array[]::record[]),
    'raw_db_columns', coalesce(columns_agg.array_agg, array[]::record[]),
    'raw_db_m2o_rels', coalesce(null, array[]::record[]),
    --'raw_db_view_col_rels', coalesce(view_col_rels_agg.array_agg, array[]::record[]),
    'raw_db_rels', coalesce(null, array[]::record[]),
    'raw_db_pg_ver', pg_version
  ) as dbstructure
from
  (select array_agg(procs order by proc_oid) from procs) procs_agg,
  (select array_agg(schemas order by schema_oid) from schemas) schemas_agg,
  (select array_agg(tables order by table_oid) from tables) as tables_agg,
  (select array_agg(columns order by col_table_oid, col_position) from columns) as columns_agg,
  --(select array_agg(m2o_rels) from m2o_rels) as m2o_rels_agg,
  --(select array_agg(view_col_rels) from view_col_rels) as view_col_rels_agg,
  --(select array_agg(rels) from rels) as rels_agg,
  pg_version
