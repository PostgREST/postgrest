.. _api:

API
###

PostgREST exposes three database objects of a schema as resources: tables, views and stored procedures.

.. toctree::
   :glob:
   :maxdepth: 1

   api/tables_views.rst
   api/stored_procedures.rst
   api/schemas.rst
   api/resource_embedding.rst
   api/openapi.rst
   api/resource_representation.rst
   api/*

.. raw:: html

  <script type="text/javascript">
    let hash = window.location.hash;

    const redirects = {
      // Tables and Views
      '#horizontal-filtering-rows': 'api/tables_views.html#horizontal-filtering-rows',
      '#operators': 'api/tables_views.html#operators',
      '#logical-operators': 'api/tables_views.html#logical-operators',
      '#pattern-matching': 'api/tables_views.html#pattern-matching',
      '#full-text-search': 'api/tables_views.html#full-text-search',
      '#vertical-filtering-columns': 'api/tables_views.html#vertical-filtering-columns',
      '#renaming-columns': 'api/tables_views.html#renaming-columns',
      '#casting-columns': 'api/tables_views.html#casting-columns',
      '#json-columns': 'api/tables_views.html#json-columns',
      '#composite-array-columns': 'api/tables_views.html#composite-array-columns',
      '#computed-virtual-columns': 'api/tables_views.html#computed-virtual-columns',
      '#ordering': 'api/tables_views.html#ordering',
      '#limits-and-pagination': 'api/tables_views.html#limits-and-pagination',
      '#exact-count': 'api/tables_views.html#exact-count',
      '#planned-count': 'api/tables_views.html#planned-count',
      '#estimated-count': 'api/tables_views.html#estimated-count',
      '#updates': 'api/tables_views.html#update',
      '#insertions': 'api/tables_views.html#insert',
      '#bulk-insert': 'api/tables_views.html#bulk-insert',
      '#specifying-columns': 'api/tables_views.html#specifying-columns',
      '#upsert': 'api/tables_views.html#upsert',
      '#on-conflict': 'api/tables_views.html#on-conflict',
      '#put': 'api/tables_views.html#put',
      '#deletions': 'api/tables_views.html#delete',
      '#limited-updates-deletions': 'api/tables_views.html#limited-update-delete',
      // Stored procedures
      '#stored-procedures': 'api/stored_procedures.html#stored-procedures',
      '#calling-functions-with-a-single-json-parameter': 'api/stored_procedures.html#functions-with-a-single-json-parameter',
      '#calling-functions-with-a-single-unnamed-parameter': 'api/stored_procedures.html#functions-with-a-single-unnamed-parameter',
      '#calling-functions-with-array-parameters': 'api/stored_procedures.html#functions-with-array-parameters',
      '#calling-variadic-functions': 'api/stored_procedures.html#variadic-functions',
      '#scalar-functions': 'api/stored_procedures.html#scalar-functions',
      '#function-filters': 'api/stored_procedures.html#table-valued-functions',
      '#overloaded-functions': 'api/stored_procedures.html#overloaded-functions',
      // Schemas
      '#switching-schemas': 'api/schemas.html',
      // Resource Embedding
      '#resource-embedding': 'api/resource_embedding.html#resource-embedding',
      '#many-to-one-relationships': 'api/resource_embedding.html#many-to-one-relationships',
      '#one-to-many-relationships': 'api/resource_embedding.html#one-to-many-relationships',
      '#many-to-many-relationships': 'api/resource_embedding.html#many-to-many-relationships',
      '#one-to-one-relationships': 'api/resource_embedding.html#one-to-one-relationships',
      '#computed-relationships': 'api/resource_embedding.html#computed-relationships',
      '#nested-embedding': 'api/resource_embedding.html#nested-embedding',
      '#embedded-filters': 'api/resource_embedding.html#embedded-filters',
      '#embedding-with-top-level-filtering': 'api/resource_embedding.html#top-level-filtering',
      '#embedding-partitioned-tables': 'api/resource_embedding.html#embedding-partitioned-tables',
      '#embedding-views': 'api/resource_embedding.html#embedding-views',
      '#embedding-chains-of-views': 'api/resource_embedding.html#embedding-chains-of-views',
      '#embedding-on-stored-procedures': 'api/resource_embedding.html#embedding-on-stored-procedures',
      '#embedding-after-insertions-updates-deletions': 'api/resource_embedding.html#embedding-after-insertions-updates-deletions',
      '#embedding-disambiguation': 'api/resource_embedding.html#embedding-disambiguation',
      '#target-disambiguation': 'api/resource_embedding.html#target-disambiguation',
      '#hint-disambiguation': 'api/resource_embedding.html#hint-disambiguation',
      // OpenAPI
      '#openapi-support': 'api/openapi.html',
      // Resource Representation
      '#response-format': 'api/resource_representation.html#response-format',
      '#singular-or-plural': 'api/resource_representation.html#singular-or-plural',
      '#response-formats-for-scalar-responses': 'api/resource_representation.html#scalar-function-response-format',
      // CORS
      '#cors': 'api/cors.html',
      // OPTIONS
      '#options': 'api/options.html',
      // URL Grammar
      '#custom-queries': 'api/url_grammar.html#custom-queries',
      '#unicode-support': 'api/url_grammar.html#unicode-support',
      '#table-columns-with-spaces': 'api/url_grammar.html#table-columns-with-spaces',
      '#reserved-characters': 'api/url_grammar.html#reserved-characters',
      // Transactions
      '#immutable-and-stable-functions': 'transactions.html#access-mode',
      '#http-context': 'transactions.html#transaction-scoped-settings',
      '#accessing-request-headers-cookies-and-jwt-claims': 'transactions.html#request-headers-cookies-and-jwt-claims',
      '#legacy-guc-variable-names': 'transactions.html#legacy-settings',
      '#accessing-request-path-and-method': 'transactions.html#request-path-and-method',
      '#setting-response-headers': 'transactions.html#response-headers',
      '#setting-headers-via-pre-request': 'transactions.html#setting-headers-via-pre-request',
      '#setting-response-status-code': 'transactions.html#response-status-code',
      '#raise-errors-with-http-status-codes': 'transactions.html#raise-errors-with-http-status-codes',
      // Admin
      '#execution-plan': 'admin.html#execution-plan',
      // Deprecated
      '#bulk-call': '../releases/v11.0.1.html#breaking-changes',
    };

    let willRedirectTo = redirects[hash];

    if (willRedirectTo) {
      window.location.href = willRedirectTo;
    }
  </script>