{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Protolude.Debug (
  undefined,
  trace,
  traceM,
  traceId,
  traceIO,
  traceShow,
  traceShowId,
  traceShowM,
  notImplemented,
  witness,
) where

import Data.Text (Text, unpack)
import Control.Monad (Monad, return)

import qualified Protolude.Base as P
import Protolude.Error (error)
import Protolude.Show (Print, hPutStrLn)

import System.IO(stderr)
import System.IO.Unsafe (unsafePerformIO)

{-# WARNING trace "'trace' remains in code" #-}
trace :: Print b => b -> a -> a
trace string expr = unsafePerformIO (do
    hPutStrLn stderr string
    return expr)

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: Print b => b -> a -> P.IO a
traceIO string expr = do
    hPutStrLn stderr string
    return expr

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> b -> b
traceShow a b = trace (P.show a) b

{-# WARNING traceShowId "'traceShowId' remains in code" #-}
traceShowId :: P.Show a => a -> a
traceShowId a = trace (P.show a) a

{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (P.Show a, Monad m) => a -> m ()
traceShowM a = trace (P.show a) (return ())

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: (Monad m) => Text -> m ()
traceM s = trace (unpack s) (return ())

{-# WARNING traceId "'traceId' remains in code" #-}
traceId :: Text -> Text
traceId s = trace s s

{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = error "Not implemented"

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = error "Prelude.undefined"

witness :: a
witness = error "Type witness should not be evaluated"
