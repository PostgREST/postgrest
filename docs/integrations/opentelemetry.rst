.. _opentelemetry:

OpenTelemetry
-------------

PostgREST is able to act as OpenTelemetry traces producer. OpenTelemetry is configured
using ``OTEL_*`` environment variables, per the `OpenTelemetry specification`_.

The OpenTelemetry support is currently both experimental and in early stages of development, so expect some rough edges
or lack of functionality, such as metrics or logs. Since current OpenTelemetry implementation incurs a small
(~6% in our "Loadtest (mixed)" suite) performance hit, it is gated behind the :ref:`server-otel-enabled`
configuration option, disabled by default.

Example configuration:

.. code-block:: shell

    OTEL_EXPORTER_OTLP_ENDPOINT='https://api.honeycomb.io/' \
    OTEL_EXPORTER_OTLP_HEADERS="x-honeycomb-team=<honeycomb_api_key>"  \
    OTEL_SERVICE_NAME='PostgREST'\
    OTEL_LOG_LEVEL='debug'\
    OTEL_TRACES_SAMPLER='always_on' \
    postgrest

.. _`OpenTelemetry specification`: https://opentelemetry.io/docs/specs/otel/configuration/sdk-environment-variables/

