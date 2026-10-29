module Hasql.Transaction.Private.Sessions where

import Hasql.Session
import Hasql.Transaction.Config
import Hasql.Transaction.Private.Prelude hiding (error, retry)

import Hasql.Transaction.Private.Statements qualified as Statements

tryTransaction :: IsolationLevel -> Mode -> Session (a, Bool) -> Session a
tryTransaction level mode body = do
  statement () (Statements.beginTransaction level mode)

  (res, commit) <- catchError body $ \error -> do
    statement () Statements.rollbackTransaction
    throwError error

  commitOrRollback commit $> res

commitOrRollback :: Bool -> Session ()
commitOrRollback commit =
  if commit then
    statement () Statements.commitTransaction
  else
    statement () Statements.rollbackTransaction
