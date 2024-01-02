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
module PostgREST.OpenTelemetry (Tracer, withTracer, middleware, inSpanM) where

import Control.Monad.Catch               (MonadMask)
import Network.Wai                       (Middleware)
import OpenTelemetry.Attributes          (emptyAttributes)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Trace               (InstrumentationLibrary (..),
                                          SpanArguments, Tracer,
                                          initializeGlobalTracerProvider,
                                          makeTracer,
                                          shutdownTracerProvider,
                                          tracerOptions)
import OpenTelemetry.Trace.Core          (getTracerTracerProvider)
import OpenTelemetry.Utils.Exceptions    (inSpanM'')
import PostgREST.AppState                (AppState, getOTelTracer)
import PostgREST.Version                 (prettyVersion)
import Protolude

{- | Wrap user's code with OpenTelemetry Tracer, initializing it with sensible defaults -}
withTracer :: (Tracer -> IO c) -> IO c
withTracer f = bracket
    initializeGlobalTracerProvider
    shutdownTracerProvider
    (\tracerProvider -> f $ makeTracer tracerProvider instrumentationLibrary tracerOptions)
    where
        instrumentationLibrary =
            InstrumentationLibrary
            { libraryName = "PostgREST"
            , libraryVersion = decodeUtf8 prettyVersion
            , librarySchemaUrl = ""
            , libraryAttributes = emptyAttributes}

middleware :: AppState -> Network.Wai.Middleware
middleware s = case getOTelTracer s of
  Just t -> newOpenTelemetryWaiMiddleware' $ getTracerTracerProvider t
  -- Make sure OTel code is actually noop
  Nothing -> identity


-- | The simplest function for annotating code with trace information.
-- In case the tracer is @Nothing@, i.e. disabled, the function is noop.
inSpanM
  :: (MonadIO m, MonadMask m, HasCallStack)
  => Maybe Tracer
  -> Text
  -- ^ The name of the span. This may be updated later via 'updateName'
  -> SpanArguments
  -- ^ Additional options for creating the span, such as 'SpanKind',
  -- span links, starting attributes, etc.
  -> m a
  -- ^ The action to perform. 'inSpan' will record the time spent on the
  -- action without forcing strict evaluation of the result. Any uncaught
  -- exceptions will be recorded and rethrown.
  -> m a
inSpanM (Just t) n args m = inSpanM'' t callStack n args (const m)
inSpanM Nothing _ _ m     = m
