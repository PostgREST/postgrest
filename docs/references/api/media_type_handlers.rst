.. _custom_media:

Media Type Handlers
###################

Media Type Handlers allows PostgREST to deliver custom media types. These handlers extend the :ref:`builtin ones <builtin_media>` and can also override them.

Media types are expressed as type aliases using `domains <https://www.postgresql.org/docs/current/sql-createdomain.html>`_ and their name must comply to `RFC 6838 requirements <https://datatracker.ietf.org/doc/html/rfc6838#section-4.2>`_.

.. code-block:: postgres

   CREATE DOMAIN "application/json" AS json;

By using these domains as return types:

- Of :ref:`Functions <s_procs>`, these will turn into handlers.

- Of `Aggregates <https://www.postgresql.org/docs/current/sql-createaggregate.html>`_ transition or final functions, these will serve as handlers for :ref:`tables_views` and :ref:`table_functions`.

.. note::

  PostgREST vendor media types (``application/vnd.pgrst.plan``, ``application/vnd.pgrst.object`` and ``application/vnd.pgrst.array``) cannot be overriden in this way.

Handler Function
================

As an example, let's obtain the `TWKB <https://postgis.net/docs/ST_AsTWKB.html>`_ compressed binary format for a PostGIS geometry.

.. code-block:: postgres

  create extension postgis;

  create table lines (
    id   int primary key
  , name text
  , geom geometry(LINESTRING, 4326)
  );

  insert into lines values (1, 'line-1', 'LINESTRING(1 1,5 5)'::geometry), (2, 'line-2', 'LINESTRING(2 2,6 6)'::geometry);

For this you can create a vendor media type and use it as a return type on a function.

.. code-block:: postgres

  create domain "application/vnd.twkb" as bytea;

  create or replace function get_line (id int)
  returns "application/vnd.twkb" as $$
    select st_astwkb(geom) from lines where id = get_line.id;
  $$ language sql;

.. note::

   For PostgreSQL <= 12, you'll need a cast on the function body :code:`st_astwkb(geom)::"application/vnd.twkb"`.

Now you can request the ``TWKB`` output like so:

.. code-block:: bash

  curl 'localhost:3000/rpc/get_line?id=1' -i \
    -H "Accept: application/vnd.twkb"

  HTTP/1.1 200 OK
  Content-Type: application/vnd.twkb

  # binary output

Note that PostgREST will automatically set the  ``Content-Type`` to ``application/vnd.twkb``.

Handlers for Tables/Views
=========================

To benefit from a compressed format like ``TWKB``, it makes more sense to obtain many rows instead of one. Let's allow that by adding a handler for the table. You'll need an aggregate:

.. code-block:: postgres

  -- let's add the vendor type as return of the transition function
  create or replace function twkb_handler_transition (state bytea, next lines)
  returns "application/vnd.twkb" as $$
    select state || st_astwkb(next.geom);
  $$ language sql;

  -- use the transition function on the aggregate
  create or replace aggregate twkb_agg (lines) (
    initcond = ''
  , stype = "application/vnd.twkb"
  , sfunc = twkb_handler_transition
  );

  -- quick test
  -- SELECT twkb_agg(l) from lines l;
  --                            twkb_agg
  ------------------------------------------------------------------
  -- \xa20002c09a0cc09a0c80ea3080ea30a2000280b51880b51880ea3080ea30
  --(1 row)

Now you can request the table endpoint with the ``twkb`` media type:

.. code-block:: bash

  curl 'localhost:3000/lines' -i \
    -H "Accept: application/vnd.twkb"

  HTTP/1.1 200 OK
  Content-Type: application/vnd.twkb

  # binary output

If you have a table-valued function returning the same table type, the handler can also act upon on it.

.. code-block:: postgres

  create or replace function get_lines ()
  returns setof lines as $$
    select * from lines;
  $$ language sql;

.. code-block:: bash

  curl 'localhost:3000/get_lines' -i \
    -H "Accept: application/vnd.twkb"

  HTTP/1.1 200 OK
  Content-Type: application/vnd.twkb

  # binary output

Overriding a Builtin Handler
============================

Let's override the existing ``text/csv`` handler for the table to provide a more complex CSV output.
It'll include a `Byte order mark <https://en.wikipedia.org/wiki/Byte_order_mark>`_ plus a ``Content-Disposition`` header to set a name for the downloaded file.

.. code-block:: postgres

  create domain "text/csv" as text;

  create or replace function bom_csv_trans (state text, next lines)
  returns "text/csv" as $$
    select state || next.id::text || ',' || next.name || ',' || next.geom::text || E'\n';
  $$ language sql;

  create or replace function bom_csv_final (data "text/csv")
  returns "text/csv" as $$
    -- set the Content-Disposition header
    select set_config('response.headers', '[{"Content-Disposition": "attachment; filename=\"lines.csv\""}]', true);
    select
      -- EFBBBF is the BOM in UTF8 https://en.wikipedia.org/wiki/Byte_order_mark#UTF-8
      convert_from (decode (E'EFBBBF', 'hex'),'UTF8') ||
      -- the header for the CSV
      (E'id,name,geom\n' || data);
  $$ language sql;

  create or replace aggregate bom_csv_agg (lines) (
    initcond = ''
  , stype = "text/csv"
  , sfunc = bom_csv_trans
  , finalfunc = bom_csv_final
  );

You can now request it like:

.. code-block:: bash

  curl 'localhost:3000/lines' -i \
    -H "Accept: text/csv"

  HTTP/1.1 200 OK
  Content-Type: text/csv
  Content-Disposition: attachment; filename="lines.csv"

  id,name,geom
  1,line-1,0102000020E610000002000000000000000000F03F000000000000F03F00000000000014400000000000001440
  2,line-2,0102000020E6100000020000000000000000000040000000000000004000000000000018400000000000001840

.. _any_handler:

The "Any" Handler
=================

For more flexibility, you can also define a catch-all handler by using a domain named ``*/*`` (any media type). This will respond to all media types and even to requests that don't include an ``Accept`` header.

Note that this will take priority over all other handlers (builtin or custom), so it's better to do it for an isolated function or view.

Let's define an any handler for a view that will always respond with ``XML`` output. It will accept ``text/xml``, ``application/xml``, ``*/*`` and reject other media types.

.. code-block:: postgres

  create domain "*/*" as pg_catalog.xml;

  -- we'll use an .xml suffix for the view to be clear it's output is always XML
  create view "lines.xml" as
  select * from lines;

  create or replace function lines_xml_trans (state "*/*", next "lines.xml")
  returns "*/*" as $$
    select xmlconcat(state, xmlelement(name line, xmlattributes(next.id as id, next.name as name), next.geom));
  $$ language sql;

  create or replace function lines_xml_final (data "*/*")
  returns "*/*" as $$
  declare
    -- get the Accept header
    req_accept text := current_setting('request.headers', true)::json->>'accept';
  begin
    -- when receiving */*, we need to set the Content-Type, otherwise PostgREST will set a default one.
    if req_accept = '*/*'
      then perform set_config('response.headers', '[{"Content-Type": "text/xml"}]', true);
    -- we'll reject other non XML media types, we need to reject manually since */* will command PostgREST to accept all media types
    elsif req_accept NOT IN ('application/xml', 'text/xml')
      then raise sqlstate 'PT415' using message = 'Unsupported Media Type';
    end if;

    return data;
  end; $$ language plpgsql;

  create or replace aggregate testlines_xml_agg ("lines.xml") (
    stype = "*/*"
  , sfunc = lines_xml_trans
  , finalfunc = lines_xml_final
  );

Now we can omit the ``Accept`` header and it will respond with XML.

.. code-block:: bash

  curl 'localhost:3000/lines.xml' -i

  HTTP/1.1 200 OK
  Content-Type: text/xml

  <line id="1" name="line-1">0102000020E610000002000000000000000000F03F000000000000F03F00000000000014400000000000001440</line>
  <line id="2" name="line-2">0102000020E6100000020000000000000000000040000000000000004000000000000018400000000000001840</line>

And it will accept only XML media types.

.. code-block:: bash

  curl 'localhost:3000/lines.xml' -i \
    -H "Accept: text/xml"

  HTTP/1.1 200 OK
  Content-Type: text/xml

  curl 'localhost:3000/lines.xml' -i  \
    -H "Accept: application/xml"

  HTTP/1.1 200 OK
  Content-Type: text/xml

  curl 'localhost:3000/lines.xml' -i \
    -H "Accept: unknown/media"

  HTTP/1.1 415 Unsupported Media Type
