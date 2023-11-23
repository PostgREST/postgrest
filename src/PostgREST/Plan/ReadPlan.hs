module PostgREST.Plan.ReadPlan
  ( ReadPlanTree
  , ReadPlan(..)
  , JoinCondition(..)
  ) where

import Data.Tree (Tree (..))

import PostgREST.ApiRequest.Types         (Alias, Depth, Hint,
                                           JoinType, NodeName)
import PostgREST.Plan.Types               (CoercibleLogicTree,
                                           CoercibleOrderTerm,
                                           CoercibleSelectField (..),
                                           RelSelectField (..))
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
  , relIsSpread  :: Bool
  , relSelect    :: [RelSelectField]
  , depth        :: Depth
  -- ^ used for aliasing
  }
  deriving (Eq, Show)
