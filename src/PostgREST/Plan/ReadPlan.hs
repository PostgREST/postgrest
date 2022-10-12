{-# LANGUAGE NamedFieldPuns #-}
module PostgREST.Plan.ReadPlan
  ( ReadPlanTree
  , ReadPlan(..)
  , fstFieldNames
  ) where

import Data.Tree (Tree (..))

import PostgREST.ApiRequest.Types         (Alias, Depth, Hint,
                                           JoinCondition, JoinType,
                                           LogicTree, NodeName,
                                           OrderTerm, SelectItem)
import PostgREST.RangeQuery               (NonnegRange)
import PostgREST.SchemaCache.Identifiers  (FieldName,
                                           QualifiedIdentifier)
import PostgREST.SchemaCache.Relationship (Relationship)


import Protolude

type ReadPlanTree = Tree ReadPlan

data ReadPlan = ReadPlan
  { select         :: [SelectItem]
  , from           :: QualifiedIdentifier
  , fromAlias      :: Maybe Alias
  -- ^ A table alias is used in case of self joins
  , where_         :: [LogicTree]
  , joinConditions :: [JoinCondition]
  , order          :: [OrderTerm]
  , range_         :: NonnegRange
  , relName        :: NodeName
  , relToParent    :: Maybe Relationship
  , relAlias       :: Maybe Alias
  , relHint        :: Maybe Hint
  , relJoinType    :: Maybe JoinType
  , depth          :: Depth
  }
  deriving (Eq)

-- First level FieldNames(e.g get a,b from /table?select=a,b,other(c,d))
fstFieldNames :: ReadPlanTree -> [FieldName]
fstFieldNames (Node ReadPlan{select} _) =
  fst . (\(f, _, _, _, _) -> f) <$> select
