module Hasql.StatementRunner
  ( StatementRunner (..)
  )
where

import Hasql.Pipeline (Pipeline)
import Hasql.Prelude
import Hasql.Session (Session)
import Hasql.Statement (Statement)

import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session

-- | Statement transport. A 'Session' sends one statement and waits for its
-- result before sending the next. 'Pipeline' sends them all and reads all the
-- results back with one round trip. No result can be seen mid-batch.
class (Applicative t) => StatementRunner t where
  runStatement :: Statement () a -> t a

instance StatementRunner Session where
  runStatement = Session.statement ()

instance StatementRunner Pipeline where
  runStatement = Pipeline.statement ()
