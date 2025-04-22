{- |
Module      : PostgREST.OpenTelemetry
Description : OpenTelemetry integration
Maintains the OpenTelemetry Tracer and provides a function to run
PostgREST with it.

Basically, you want to use `withTracer` in your main function, and then
use `getOTelTracer` in your application code to get the tracer and
create spans with `inSpanM`.

At this moment trace spans have to be explicit, by wrapping the code in `inSpanM` calls.
In order produced spans to have correct code locations, all the functions across the call stack up to the
`inSpanM` call must have `HasCallStack` constraint, because
[GHC is never inferring it](https://downloads.haskell.org/ghc/9.8.4/docs/users_guide/exts/callstack.html) for us.
-}
module PostgREST.OpenTelemetry (withTracer) where

import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Trace      (InstrumentationLibrary (..), Tracer,
                                 initializeGlobalTracerProvider,
                                 makeTracer, shutdownTracerProvider,
                                 tracerOptions)
import PostgREST.Version        (prettyVersion)
import Protolude

{- | Wrap user's code with OpenTelemetry Tracer, initializing it with sensible defaults -}
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

