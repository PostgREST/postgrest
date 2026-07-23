{-# LANGUAGE RankNTypes #-}

module PostgREST.Process.Restart.Impl
  ( runRestartable
  ) where

import PostgREST.Process.Restart.Shared

import Protolude

-- | Run an application without handover support on platforms where the
-- replacement protocol is unavailable.
runRestartable ::
  ReplacementConfig ->
  AppRun a ->
  IO a
runRestartable _ runApp =
  runApp (HandoverMode False) readyUnsupported
  where
    readyUnsupported _ withRequestReplacement =
      withRequestReplacement $ throwIO $ HandoverFailed "Restart handover is not supported on this platform."
