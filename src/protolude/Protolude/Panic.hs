{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Protolude.Panic (
  FatalError(FatalError, fatalErrorMessage),
  panic,
) where

import Protolude.Base (Show)
import Protolude.CallStack (HasCallStack)
import Data.Text (Text)
import Control.Exception as X

-- | Uncatchable exceptions thrown and never caught.
newtype FatalError = FatalError { fatalErrorMessage :: Text }
  deriving (Show)

instance Exception FatalError

panic :: HasCallStack => Text -> a
panic a = throw (FatalError a)
