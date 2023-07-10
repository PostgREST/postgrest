module PostgREST.Plan.ReadPlan
  ( ReadPlanTree
  , ReadPlan(..)
  , JoinCondition(..)
  ) where

import Data.Tree (Tree (..))

import PostgREST.ApiRequest.Types         (Alias, Cast, Depth, Hint,
                                           JoinType, NodeName)
import PostgREST.Plan.Types               (CoercibleField (..),
                                           CoercibleLogicTree,
                                           CoercibleOrderTerm)
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
  { select       :: [(CoercibleField, Maybe Cast, Maybe Alias)]
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
  , depth        :: Depth
  -- ^ used for aliasing
  }
  deriving (Eq, Show)
