{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Relation
  ( Cardinality(..)
  , ForeignKey(..)
  , PrimaryKey(..)
  , Relation(..)
  , Junction(..)
  , isSelfReference
  ) where

import qualified Data.Aeson as JSON

import PostgREST.DbStructure.Table (Column (..), ForeignKey (..),
                                    Table (..))

import Protolude


-- | "Relation"ship between two tables.
--
-- The order of the relColumns and relFColumns should be maintained to get the
-- join conditions right.
--
-- TODO merge relColumns and relFColumns to a tuple or Data.Bimap
data Relation = Relation
  { relTable          :: Table
  , relColumns        :: [Column]
  , relForeignTable   :: Table
  , relForeignColumns :: [Column]
  , relCardinality    :: Cardinality
  }
  deriving (Eq, Generic, JSON.ToJSON)

-- | The relationship cardinality
-- | https://en.wikipedia.org/wiki/Cardinality_(data_modeling)
-- TODO: missing one-to-one
data Cardinality
  = O2M FKConstraint -- ^ one-to-many cardinality
  | M2O FKConstraint -- ^ many-to-one cardinality
  | M2M Junction     -- ^ many-to-many cardinality
  deriving (Eq, Generic, JSON.ToJSON)

type FKConstraint = Text

-- | Junction table on an M2M relationship
data Junction = Junction
  { junTable       :: Table
  , junConstraint1 :: FKConstraint
  , junColumns1    :: [Column]
  , junConstraint2 :: FKConstraint
  , junColumns2    :: [Column]
  }
  deriving (Eq, Generic, JSON.ToJSON)

isSelfReference :: Relation -> Bool
isSelfReference r = relTable r == relForeignTable r

data PrimaryKey = PrimaryKey
  { pkTable :: Table
  , pkName  :: Text
  }
  deriving (Generic, JSON.ToJSON)
