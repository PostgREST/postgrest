{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Relation
  ( Cardinality(..)
  , Constraint
  , ForeignKey(..)
  , Link(..)
  , PrimaryKey(..)
  , Relation(..)
  , isSelfReference
  ) where

import qualified Data.Aeson as JSON

import PostgREST.DbStructure.Table (Column (..), ForeignKey (..),
                                    Table (..))

import qualified GHC.Show (show)

import Protolude


-- | "Relation"ship between two tables.
--
-- The order of the relColumns and relFColumns should be maintained to get the
-- join conditions right.
--
-- TODO merge relColumns and relFColumns to a tuple or Data.Bimap
data Relation = Relation
  { relTable    :: Table
  , relColumns  :: [Column]
  , relFTable   :: Table
  , relFColumns :: [Column]
  , relType     :: Cardinality
  , relLink     :: Link -- ^ Constraint on O2M/M2O, Junction for M2M Cardinality
  }
  deriving (Eq, Generic, JSON.ToJSON)

type ConstraintName = Text

-- | Junction table on an M2M relationship
data Link
  = Constraint
      { constName :: ConstraintName }
  | Junction
      { junTable :: Table
      , junLink1 :: Link
      , junCols1 :: [Column]
      , junLink2 :: Link
      , junCols2 :: [Column]
      }
  deriving (Eq, Generic, JSON.ToJSON)

data PrimaryKey = PrimaryKey
  { pkTable :: Table
  , pkName  :: Text
  }
  deriving (Generic, JSON.ToJSON)

-- | The relationship
-- [cardinality](https://en.wikipedia.org/wiki/Cardinality_(data_modeling)).
-- TODO: missing one-to-one
data Cardinality
  = O2M -- ^ one-to-many,  previously known as Parent
  | M2O -- ^ many-to-one,  previously known as Child
  | M2M -- ^ many-to-many, previously known as Many
  deriving (Eq, Generic, JSON.ToJSON)

instance Show Cardinality where
  show O2M = "o2m"
  show M2O = "m2o"
  show M2M = "m2m"

isSelfReference :: Relation -> Bool
isSelfReference r = relTable r == relFTable r
