{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Relationship
  ( Cardinality(..)
  , Relationship(..)
  , Junction(..)
  , isSelfReference
  ) where

import qualified Data.Aeson as JSON

import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier)

import Protolude


-- | Relationship between two tables.
data Relationship = Relationship
  { relTable        :: QualifiedIdentifier
  , relForeignTable :: QualifiedIdentifier
  , relCardinality  :: Cardinality
  }
  deriving (Eq, Generic, JSON.ToJSON)

-- | The relationship cardinality
-- | https://en.wikipedia.org/wiki/Cardinality_(data_modeling)
-- TODO: missing one-to-one
data Cardinality
  = O2M {relCons :: FKConstraint, relColumns :: [(FieldName, FieldName)]}
  -- ^ one-to-many
  | M2O {relCons :: FKConstraint, relColumns :: [(FieldName, FieldName)]}
  -- ^ many-to-one
  | M2M Junction
  -- ^ many-to-many
  deriving (Eq, Generic, JSON.ToJSON)

type FKConstraint = Text

-- | Junction table on an M2M relationship
data Junction = Junction
  { junTable       :: QualifiedIdentifier
  , junConstraint1 :: FKConstraint
  , junConstraint2 :: FKConstraint
  , junColumns1    :: [(FieldName, FieldName)]
  , junColumns2    :: [(FieldName, FieldName)]
  }
  deriving (Eq, Generic, JSON.ToJSON)

isSelfReference :: Relationship -> Bool
isSelfReference r = relTable r == relForeignTable r
