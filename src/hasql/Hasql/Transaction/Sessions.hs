module Hasql.Transaction.Sessions
  ( transaction,
    transactionNoRetry,

    -- * Transaction settings
    C.Mode (..),
    C.IsolationLevel (..),
  )
where

import qualified Hasql.Session                         as B
import qualified Hasql.Transaction.Config              as C
import           Hasql.Transaction.Private.Prelude
import qualified Hasql.Transaction.Private.Transaction as A

-- |
-- Execute the transaction using the provided isolation level and mode.
{-# INLINE transaction #-}
transaction :: C.IsolationLevel -> C.Mode -> A.Transaction a -> B.Session a
transaction isolation mode transaction =
  A.run transaction isolation mode True

-- |
-- Execute the transaction but do not retry it on errors.
{-# INLINE transactionNoRetry #-}
transactionNoRetry :: C.IsolationLevel -> C.Mode -> A.Transaction a -> B.Session a
transactionNoRetry isolation mode transaction =
  A.run transaction isolation mode False
