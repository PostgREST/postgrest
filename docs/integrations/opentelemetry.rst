.. _opentelemetry:

OpenTelemetry
-------------

PostgREST supports producing OpenTelemetry traces distributed tracing.
This is useful for monitoring and debugging the performance of your PostgREST server.

OpenTelemetry is implemented in PostgREST using the hs-opentelemetry_ library and is configured
using ``OTEL_*`` environment variables, per the `OpenTelemetry specification`_.

.. _hs-opentelemetry: https://github.com/iand675/hs-opentelemetry/

.. _`OpenTelemetry specification`: https://opentelemetry.io/docs/languages/sdk-configuration/
