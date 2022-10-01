module PostgREST.Request.ReadQuery
  ( ReadNode
  , ReadQuery(..)
  , ReadRequest
  , fstFieldNames
  ) where

import Data.Tree (Tree (..))

import PostgREST.DbStructure.Identifiers  (FieldName,
                                           QualifiedIdentifier)
import PostgREST.DbStructure.Relationship (Relationship)
import PostgREST.RangeQuery               (NonnegRange)
import PostgREST.Request.Types            (Alias, Depth, Hint,
                                           JoinCondition, JoinType,
                                           LogicTree, NodeName,
                                           OrderTerm, SelectItem)


import Protolude

type ReadRequest = Tree ReadNode

type ReadNode =
  (ReadQuery, (NodeName, Maybe Relationship, Maybe Alias, Maybe Hint, Maybe JoinType, Depth))

data ReadQuery = Select
  { select         :: [SelectItem]
  , from           :: QualifiedIdentifier
  , fromAlias      :: Maybe Alias
  -- ^ A table alias is used in case of self joins
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
