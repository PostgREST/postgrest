module Hasql.Transaction.Config where

import Hasql.Transaction.Private.Prelude

data Mode
  = -- |
    -- Read-only. No writes possible.
    Read
  | -- |
    -- Write and commit.
    Write
  deriving (Show, Eq, Ord, Enum, Bounded)

-- |
-- For reference see
-- <http://www.postgresql.org/docs/current/static/transaction-iso.html the Postgres' documentation>.
data IsolationLevel
  = ReadCommitted
  | RepeatableRead
  | Serializable
  deriving (Show, Eq, Ord, Enum, Bounded)
