module PostgREST.Plan.ReadPlan
  ( ReadPlanTree
  , ReadPlan(..)
  , JoinCondition(..)
  , SpreadType(..)
  ) where

import Data.Tree (Tree (..))

import PostgREST.ApiRequest.Types         (Alias, Depth, Hint,
                                           JoinType, NodeName)
import PostgREST.Plan.Types               (CoercibleLogicTree,
                                           CoercibleOrderTerm,
                                           CoercibleSelectField (..),
                                           RelSelectField (..),
                                           SpreadType (..))
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
  deriving (Eq, Show)

-- TODO: Enforce uniqueness of columns by changing to a Set instead of a List where applicable
data ReadPlan = ReadPlan
  { select       :: [CoercibleSelectField]
  , from         :: QualifiedIdentifier
  , fromAlias    :: Maybe Alias
  , where_       :: [CoercibleLogicTree]
  , order        :: [CoercibleOrderTerm]
  , range_       :: NonnegRange
  , relName      :: NodeName
  , relToParent  :: Maybe Relationship
  , relJoinConds :: [JoinCondition]
  , relAlias     :: Maybe Alias
  , relAggAlias  :: Alias
  , relHint      :: Maybe Hint
  , relJoinType  :: Maybe JoinType
  , relSpread    :: Maybe SpreadType
  , relSelect    :: [RelSelectField]
  , depth        :: Depth
  -- ^ used for aliasing
  }
  deriving (Eq, Show)
