{-# LANGUAGE NamedFieldPuns #-}
module PostgREST.Plan.ReadPlan
  ( ReadPlanTree
  , ReadPlan(..)
  , fstFieldNames
  , JoinCondition(..)
  ) where

import Data.Tree (Tree (..))

import PostgREST.ApiRequest.Types         (Alias, Depth, Hint,
                                           JoinType, LogicTree,
                                           NodeName, OrderTerm,
                                           SelectItem)
import PostgREST.RangeQuery               (NonnegRange)
import PostgREST.SchemaCache.Identifiers  (FieldName,
                                           QualifiedIdentifier)
import PostgREST.SchemaCache.Relationship (Relationship)


import Protolude

type ReadPlanTree = Tree ReadPlan

data JoinCondition =
  JoinCondition
    (QualifiedIdentifier, FieldName)
    (QualifiedIdentifier, FieldName)
  deriving (Eq)

data ReadPlan = ReadPlan
  { select       :: [SelectItem]
  , from         :: QualifiedIdentifier
  , fromAlias    :: Maybe Alias
  , where_       :: [LogicTree]
  , order        :: [OrderTerm]
  , range_       :: NonnegRange
  , relName      :: NodeName
  , relToParent  :: Maybe Relationship
  , relJoinConds :: [JoinCondition]
  , relAlias     :: Maybe Alias
  , relHint      :: Maybe Hint
  , relJoinType  :: Maybe JoinType
  , depth        :: Depth
  -- depth is used bc when a self join occurs we
  -- need to differentiate the parent from the child tables by having an alias like
  -- "table_depth". See http://github.com/PostgREST/postgrest/issues/987.
  }
  deriving (Eq)

-- First level FieldNames(e.g get a,b from /table?select=a,b,other(c,d))
fstFieldNames :: ReadPlanTree -> [FieldName]
fstFieldNames (Node ReadPlan{select} _) =
  fst . (\(f, _, _, _, _) -> f) <$> select
