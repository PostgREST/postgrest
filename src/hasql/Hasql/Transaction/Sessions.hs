module Hasql.Transaction.Sessions
  ( transactionNoRetry
  , TxEnd (..)
  , transactionSequential
  , transactionPipelined
  , transactionPipelinedStaticEnd

    -- * Transaction settings
  , C.Mode (..)
  , C.IsolationLevel (..)
  )
where

import Hasql.Pipeline (Pipeline)
import Hasql.Statement (Statement (..))
import Hasql.StatementRunner (StatementRunner (..))
import Hasql.Transaction.Private.Prelude

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as B
import Hasql.Transaction.Config qualified as C
import Hasql.Transaction.Private.SQL qualified as SQL
import Hasql.Transaction.Private.Transaction qualified as A

-- |
-- Execute the transaction but do not retry it on errors.
{-# INLINE transactionNoRetry #-}
transactionNoRetry :: C.IsolationLevel -> C.Mode -> A.Transaction a -> B.Session a
transactionNoRetry isolation mode transaction' =
  A.run transaction' isolation mode

-- | How the transaction ends. 'TxAbort' carries whether deferred constraints are
-- checked first, which is what still surfaces a constraint violation on a request
-- that asked not to commit.
data TxEnd
  = TxCommit
  | TxAbort Bool

-- | A transaction-control statement. libpq rejects the simple query protocol
-- inside a pipeline, so these cannot go through 'B.sql' -- they have to be
-- extended-protocol statements like every other one. They are marked
-- preparable so hasql prepares them once per connection and reuses them by
-- name: on a connection running at loadtest rates the saved parses of
-- BEGIN/COMMIT/ABORT (and SET CONSTRAINTS when a request rolls back) dominate
-- the registry overhead.
control :: ByteString -> Statement () ()
control statementSql = Statement statementSql Encoders.noParams Decoders.noResult True

beginStmt :: C.IsolationLevel -> C.Mode -> Statement () ()
beginStmt isoLvl mode = control $ SQL.beginTransaction isoLvl mode

terminate :: (StatementRunner t) => TxEnd -> t ()
terminate TxCommit = runStatement $ control "COMMIT"
terminate (TxAbort checkDeferred) =
  when checkDeferred (runStatement $ control "SET CONSTRAINTS ALL IMMEDIATE") *> runStatement (control "ABORT")

-- | Cleans up after a failed statement. An error inside a pipeline for
-- BEGIN..COMMIT leaves the session in an aborted transaction. Postgres treats
-- ROLLBACK with no transaction in progress as a no-op, so this is equally
-- safe on a path that never opened one.
rollbackOnError :: B.Session a -> B.Session a
rollbackOnError session = session `catchError` \err -> B.sql "ROLLBACK" >> throwError err

-- | Run a sequential transaction: BEGIN, then the body, then the chosen terminator.
-- Every statement is its own round trip.
transactionSequential :: C.IsolationLevel -> C.Mode -> B.Session (a, TxEnd) -> B.Session a
transactionSequential isoLvl mode body = rollbackOnError $ do
  runStatement $ beginStmt isoLvl mode
  (res, end) <- body
  terminate end
  pure res

-- | Run a pipelined transaction where the terminator depends on the body result.
-- BEGIN rides with the body in a single round trip; the terminator takes a second
-- round trip because its choice (COMMIT vs ABORT) cannot be made until the body
-- has been read.
transactionPipelined :: C.IsolationLevel -> C.Mode -> Pipeline (a, TxEnd) -> B.Session a
transactionPipelined isoLvl mode body = rollbackOnError $ do
  (res, end) <- B.pipeline $ runStatement (beginStmt isoLvl mode) *> body
  terminate end
  pure res

-- | Run a pipelined transaction with a terminator chosen in advance. BEGIN, the
-- body, and the terminator all fit in one round trip.
transactionPipelinedStaticEnd :: C.IsolationLevel -> C.Mode -> TxEnd -> Pipeline a -> B.Session a
transactionPipelinedStaticEnd isoLvl mode end body =
  rollbackOnError
    $ B.pipeline
    $ runStatement (beginStmt isoLvl mode)
    *> body
    <* terminate end
