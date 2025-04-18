.. _opentelemetry:

OpenTelemetry
-------------

PostgREST supports producing OpenTelemetry traces distributed tracing.
This is useful for monitoring and debugging the performance of your PostgREST server.

OpenTelemetry is implemented in PostgREST using the hs-opentelemetry_ library and is configured
using ``OTEL_*`` environment variables, per the `OpenTelemetry specification`_.

Example configuration:

.. code-block:: shell

    OTEL_EXPORTER_OTLP_ENDPOINT='https://api.honeycomb.io/' \
    OTEL_EXPORTER_OTLP_HEADERS="x-honeycomb-team=<honeycomb_api_key>"  \
    OTEL_SERVICE_NAME='PostgREST' OTEL_LOG_LEVEL='debug' OTEL_TRACES_SAMPLER='always_on' \
    postgrest-run

Developer notes
----------------

At this moment trace spans have to be explicit, by wrapping the code in ``inSpanM`` calls. In order produced spans to have correct code locations, all the functions across the call stack up to the
``inSpanM`` call must have ``HasCallStack`` constraint, because `GHC is never inferring it`_ for us.

.. _hs-opentelemetry: https://github.com/iand675/hs-opentelemetry/

.. _`OpenTelemetry specification`: https://opentelemetry.io/docs/languages/sdk-configuration/

.. _`GHC is never inferring it`: https://downloads.haskell.org/ghc/9.8.4/docs/users_guide/exts/callstack.html
