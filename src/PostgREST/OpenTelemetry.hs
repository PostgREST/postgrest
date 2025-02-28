module PostgREST.OpenTelemetry (withTracer) where

import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Trace      (InstrumentationLibrary (..), Tracer,
                                 initializeGlobalTracerProvider,
                                 makeTracer, shutdownTracerProvider,
                                 tracerOptions)
import PostgREST.Version        (prettyVersion)
import Protolude

withTracer :: Text -> (Tracer -> IO c) -> IO c
withTracer label f = bracket
    initializeGlobalTracerProvider
    shutdownTracerProvider
    (\tracerProvider -> f $ makeTracer tracerProvider instrumentationLibrary tracerOptions)
    where
        instrumentationLibrary =
            InstrumentationLibrary
            { libraryName = label
            , libraryVersion = decodeUtf8 prettyVersion
            , librarySchemaUrl = ""
            , libraryAttributes = emptyAttributes}
