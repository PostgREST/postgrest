module PostgREST.ApiRequest.Preferences where

import GHC.Show
import Protolude


data PreferResolution
  = MergeDuplicates
  | IgnoreDuplicates

instance Show PreferResolution where
  show MergeDuplicates  = "resolution=merge-duplicates"
  show IgnoreDuplicates = "resolution=ignore-duplicates"

-- | How to return the mutated data. From https://tools.ietf.org/html/rfc7240#section-4.2
data PreferRepresentation
  = Full        -- ^ Return the body plus the Location header(in case of POST).
  | HeadersOnly -- ^ Return the Location header(in case of POST). This needs a SELECT privilege on the pk.
  | None        -- ^ Return nothing from the mutated data.
  deriving Eq

instance Show PreferRepresentation where
  show Full        = "return=representation"
  show None        = "return=minimal"
  show HeadersOnly = mempty

data PreferParameters
  = SingleObject    -- ^ Pass all parameters as a single json object to a stored procedure
  | MultipleObjects -- ^ Pass an array of json objects as params to a stored procedure
  deriving Eq

instance Show PreferParameters where
  show SingleObject    = "params=single-object"
  show MultipleObjects = "params=multiple-objects"

data PreferCount
  = ExactCount     -- ^ exact count(slower)
  | PlannedCount   -- ^ PostgreSQL query planner rows count guess. Done by using EXPLAIN {query}.
  | EstimatedCount -- ^ use the query planner rows if the count is superior to max-rows, otherwise get the exact count.
  deriving Eq

instance Show PreferCount where
  show ExactCount     = "count=exact"
  show PlannedCount   = "count=planned"
  show EstimatedCount = "count=estimated"

data PreferTransaction
  = Commit   -- Commit transaction - the default.
  | Rollback -- Rollback transaction after sending the response - does not persist changes, e.g. for running tests.
  deriving Eq

instance Show PreferTransaction where
  show Commit   = "tx=commit"
  show Rollback = "tx=rollback"
