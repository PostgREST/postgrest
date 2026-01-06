{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenTelemetry.Utils.Exceptions (inSpanM, inSpanM'') where

import           Control.Monad.Catch               (MonadMask)
import qualified Control.Monad.Catch               as MonadMask
import qualified Data.Text                         as T
import           GHC.Exception                     (SrcLoc (..))
import           OpenTelemetry.Context             (insertSpan,
                                                    lookupSpan,
                                                    removeSpan)
import           OpenTelemetry.Context.ThreadLocal (adjustContext)
import qualified OpenTelemetry.Context.ThreadLocal as TraceCore.SpanContext
import qualified OpenTelemetry.Trace               as Trace
import           OpenTelemetry.Trace.Core          (ToAttribute (..),
                                                    endSpan,
                                                    recordException,
                                                    setStatus,
                                                    whenSpanIsRecording)
import qualified OpenTelemetry.Trace.Core          as TraceCore
import           Protolude

bracketError' :: (MonadMask m) => m a -> (Maybe SomeException -> a -> m b) -> (a -> m c) -> m c
bracketError' before after thing = MonadMask.mask $ \restore -> do
  x <- before
  res1 <- MonadMask.try $ restore $ thing x
  case res1 of
    Left (e1 :: SomeException) -> do
      -- explicitly ignore exceptions from after. We know that
      -- no async exceptions were thrown there, so therefore
      -- the stronger exception must come from thing
      --
      -- https://github.com/fpco/safe-exceptions/issues/2
      _ :: Either SomeException b <-
        MonadMask.try $ MonadMask.uninterruptibleMask_ $ after (Just e1) x
      MonadMask.throwM e1
    Right y -> do
      _ <- MonadMask.uninterruptibleMask_ $ after Nothing x
      return y


-- | The simplest function for annotating code with trace information.
inSpanM
  :: (MonadIO m, MonadMask m, HasCallStack)
  => Trace.Tracer
  -> Text
  -- ^ The name of the span. This may be updated later via 'updateName'
  -> Trace.SpanArguments
  -- ^ Additional options for creating the span, such as 'SpanKind',
  -- span links, starting attributes, etc.
  -> m a
  -- ^ The action to perform. 'inSpan' will record the time spent on the
  -- action without forcing strict evaluation of the result. Any uncaught
  -- exceptions will be recorded and rethrown.
  -> m a
inSpanM t n args m = if TraceCore.tracerIsEnabled t then inSpanM'' t callStack n args (const m) else m

inSpanM''
  :: (MonadMask m, HasCallStack, MonadIO m)
  => Trace.Tracer
  -> CallStack
  -- ^ Record the location of the span in the codebase using the provided
  -- callstack for source location info.
  -> Text
  -- ^ The name of the span. This may be updated later via 'updateName'
  -> Trace.SpanArguments
  -> (Trace.Span -> m a)
  -> m a
inSpanM'' t cs n args f =
  {-# SCC "inSpanM_doubleprime" #-} bracketError' before after (f . snd)
  where
    before = {-# SCC "inSpanM_before" #-} do
      ctx <- TraceCore.SpanContext.getContext
      s <- {-# SCC "createSpanWithoutCallStack" #-} TraceCore.createSpanWithoutCallStack t ctx n args
      {-# SCC "adjustContext_insert" #-} adjustContext (insertSpan s)
      whenSpanIsRecording s $
        {-# SCC "span_attribute_enrichment" #-} do
          case getCallStack cs of
            [] -> pure ()
            (fn, loc) : _ -> do
              TraceCore.addAttributes
                s
                [ ("code.function", toAttribute $ T.pack fn)
                , ("code.namespace", toAttribute $ T.pack $ srcLocModule loc)
                , ("code.filepath", toAttribute $ T.pack $ srcLocFile loc)
                , ("code.lineno", toAttribute $ srcLocStartLine loc)
                , ("code.package", toAttribute $ T.pack $ srcLocPackage loc)
                ]
      pure (lookupSpan ctx, s)

    after e (parent, s) =
      {-# SCC "inSpanM_after" #-} do
        forM_ e $ \(MonadMask.SomeException inner) ->
          {-# SCC "record_exception" #-} do
            setStatus s $ Trace.Error $ T.pack $ MonadMask.displayException inner
            recordException s [("exception.escaped", toAttribute True)] Nothing inner
        endSpan s Nothing
        {-# SCC "adjustContext_restore" #-}
          adjustContext
          $ \ctx -> maybe (removeSpan ctx) (`insertSpan` ctx) parent
