module Hasql.Transaction.Sessions
  ( transactionNoRetry

    -- * Transaction settings
  , C.Mode (..)
  , C.IsolationLevel (..)
  )
where

import Hasql.Session qualified as B
import Hasql.Transaction.Config qualified as C
import Hasql.Transaction.Private.Transaction qualified as A

-- |
-- Execute the transaction but do not retry it on errors.
{-# INLINE transactionNoRetry #-}
transactionNoRetry :: C.IsolationLevel -> C.Mode -> A.Transaction a -> B.Session a
transactionNoRetry isolation mode transaction' =
  A.run transaction' isolation mode
