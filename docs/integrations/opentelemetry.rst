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

Prometheus metrics through the OpenTelemetry Collector
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PostgREST currently exports traces through OpenTelemetry, but not metrics. But,
it's possible to scrape the :ref:`metrics` endpoint exposed by the
:ref:`admin_server` and relay it through the OpenTelemetry Collector.

Example collector configuration:

.. code-block:: yaml

    receivers:
      prometheus:
        config:
          scrape_configs:
            - job_name: postgrest
              scrape_interval: 15s
              metrics_path: /metrics
              static_configs:
                - targets: ["127.0.0.1:3001"]

    processors:
      batch:

    exporters:
      otlp:
        endpoint: otel-collector:4317
        tls:
          insecure: true

    service:
      pipelines:
        metrics:
          receivers: [prometheus]
          processors: [batch]
          exporters: [otlp]

This assumes the PostgREST :ref:`admin_server` is listening on port ``3001`` and that
its ``/metrics`` endpoint is reachable by the collector. Adjust the target address and
OTLP exporter settings for your deployment.

.. _`OpenTelemetry specification`: https://opentelemetry.io/docs/specs/otel/configuration/sdk-environment-variables/
