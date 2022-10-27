{-# LANGUAGE NamedFieldPuns #-}
module PostgREST.Plan.ReadPlan
  ( ReadPlanTree
  , ReadPlan(..)
  , fstFieldNames
  , JoinCondition(..)
  ) where

import Data.Tree (Tree (..))

import PostgREST.ApiRequest.Types         (Alias, Cast, Depth, Field,
                                           Hint, JoinType, LogicTree,
                                           NodeName, OrderTerm)
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
  { select       :: [(Field, Maybe Cast, Maybe Alias)]
  , from         :: QualifiedIdentifier
  , fromAlias    :: Maybe Alias
  , where_       :: [LogicTree]
  , order        :: [OrderTerm]
  , range_       :: NonnegRange
  , relName      :: NodeName
  , relToParent  :: Maybe Relationship
  , relJoinConds :: [JoinCondition]
  , relAlias     :: Maybe Alias
  , relAggAlias  :: Alias
  , relHint      :: Maybe Hint
  , relJoinType  :: Maybe JoinType
  , depth        :: Depth
  -- ^ used for aliasing
  }
  deriving (Eq)

-- First level FieldNames(e.g get a,b from /table?select=a,b,other(c,d))
fstFieldNames :: ReadPlanTree -> [FieldName]
fstFieldNames (Node ReadPlan{select} _) =
  fst . (\(f, _, _) -> f) <$> select
