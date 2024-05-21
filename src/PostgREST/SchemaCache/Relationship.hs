{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.SchemaCache.Relationship
  ( Cardinality(..)
  , Relationship(..)
  , Junction(..)
  , RelationshipsMap
  , relIsToOne
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM

import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier, Schema)

import Protolude


-- | Relationship between two tables.
data Relationship = Relationship
  { relTable        :: QualifiedIdentifier
  , relForeignTable :: QualifiedIdentifier
  , relIsSelf       :: Bool -- ^ Whether is a self relationship
  , relCardinality  :: Cardinality
  , relTableIsView  :: Bool
  , relFTableIsView :: Bool
  }
  | ComputedRelationship
  { relFunction     :: QualifiedIdentifier
  , relTable        :: QualifiedIdentifier
  , relForeignTable :: QualifiedIdentifier
  , relTableAlias   :: QualifiedIdentifier
  , relToOne        :: Bool
  , relIsSelf       :: Bool
  }
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

-- | The relationship cardinality
-- | https://en.wikipedia.org/wiki/Cardinality_(data_modeling)
data Cardinality
  = O2M {relCons :: FKConstraint, relColumns :: [(FieldName, FieldName)]}
  -- ^ one-to-many
  | M2O {relCons :: FKConstraint, relColumns :: [(FieldName, FieldName)]}
  -- ^ many-to-one
  | O2O {relCons :: FKConstraint, relColumns :: [(FieldName, FieldName)], isParent :: Bool}
  -- ^ one-to-one, this is a refinement over M2O, operating on it is pretty much the same as M2O when isParent == False
  | M2M Junction
  -- ^ many-to-many
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

type FKConstraint = Text

-- | Junction table on an M2M relationship
data Junction = Junction
  { junTable       :: QualifiedIdentifier
  , junConstraint1 :: FKConstraint
  , junConstraint2 :: FKConstraint
  , junColsSource  :: [(FieldName, FieldName)]
  , junColsTarget  :: [(FieldName, FieldName)]
  }
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

-- | Key based on the source table and the foreign table schema
type RelationshipsMap = HM.HashMap (QualifiedIdentifier, Schema)  [Relationship]

relIsToOne :: Relationship -> Bool
relIsToOne rel = case rel of
  Relationship{relCardinality=M2O {}} -> True
  Relationship{relCardinality=O2O {}} -> True
  ComputedRelationship{relToOne=True} -> True
  _                                   -> False
