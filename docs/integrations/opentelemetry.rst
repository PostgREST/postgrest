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
    postgrest

.. _hs-opentelemetry: https://github.com/iand675/hs-opentelemetry/

.. _`OpenTelemetry specification`: https://opentelemetry.io/docs/languages/sdk-configuration/
