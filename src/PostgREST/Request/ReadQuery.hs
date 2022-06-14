module PostgREST.Request.ReadQuery
  ( ReadNode
  , ReadQuery(..)
  , ReadRequest
  , SelectItem
  , fstFieldNames
  ) where

import Data.Tree (Tree (..))

import PostgREST.DbStructure.Identifiers  (FieldName,
                                           QualifiedIdentifier)
import PostgREST.DbStructure.Relationship (Relationship)
import PostgREST.RangeQuery               (NonnegRange)
import PostgREST.Request.Types            (Alias, Cast, Depth, Field,
                                           Hint, JoinCondition,
                                           JoinType, LogicTree,
                                           NodeName, OrderTerm)


import Protolude

type ReadRequest = Tree ReadNode

type ReadNode =
  (ReadQuery, (NodeName, Maybe Relationship, Maybe Alias, Maybe Hint, Maybe JoinType, Depth))

-- | The select value in `/tbl?select=alias:field::cast`
type SelectItem = (Field, Maybe Cast, Maybe Alias, Maybe Hint, Maybe JoinType)

data ReadQuery = Select
  { select         :: [SelectItem]
  , from           :: QualifiedIdentifier
  -- ^ A table alias is used in case of self joins
  , fromAlias      :: Maybe Alias
  -- ^ Only used for Many to Many joins. Parent and Child joins use explicit joins.
  , implicitJoins  :: [QualifiedIdentifier]
  , where_         :: [LogicTree]
  , joinConditions :: [JoinCondition]
  , order          :: [OrderTerm]
  , range_         :: NonnegRange
  }
  deriving (Eq)

-- First level FieldNames(e.g get a,b from /table?select=a,b,other(c,d))
fstFieldNames :: ReadRequest -> [FieldName]
fstFieldNames (Node (sel, _) _) =
  fst . (\(f, _, _, _, _) -> f) <$> select sel
