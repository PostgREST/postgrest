{-# LANGUAGE DeriveAnyClass #-}

module PostgREST.Catalog.Relationship
  ( Cardinality (..)
  , addViewM2OAndO2ORels
  , addViewPrimaryKeys
  , KeyDep (..)
  , Relationship (..)
  , Junction (..)
  , RelationshipsMap
  , ViewKeyDependency (..)
  , relIsToOne
  )
where

import Control.Arrow ((&&&))
import Protolude

import Data.Aeson qualified as JSON
import Data.HashMap.Strict qualified as HM

import PostgREST.Catalog.Identifiers
  ( FieldName
  , QualifiedIdentifier (..)
  , Schema
  )
import PostgREST.Catalog.Table (Table (..), TablesMap)

-- | A view foreign key or primary key dependency detected on its source table
-- Each column of the key could be referenced multiple times in the view, e.g.
--
-- create view projects_view as
-- select
--   id as id_1,
--   id as id_2,
--   id as id_3,
--   name
-- from projects
--
-- In this case, the keyDepCols mapping maps projects.id to all three of the columns:
--
-- [('id', ['id_1', 'id_2', 'id_3'])]
--
-- Depending on key type, we can then choose how to handle this case. Primary keys
-- can arbitrarily choose one of the columns, but for foreign keys we need to create
-- relationships for each possible mutations.
--
-- Previously, we stored a (FieldName, FieldName) tuple only, but then we had no
-- way to make a difference between a multi-column-key and a single-column-key with multiple
-- references in the view. Or even worse in the multi-column-key-multi-reference case...
data ViewKeyDependency = ViewKeyDependency
  { keyDepTable :: QualifiedIdentifier
  , keyDepView :: QualifiedIdentifier
  , keyDepCons :: Text
  , keyDepType :: KeyDep
  , keyDepCols :: [(FieldName, [FieldName])]
  -- ^ First element is the table column, second is a list of view columns
  }
  deriving (Eq)

data KeyDep
  = -- | PK dependency
    PKDep
  | -- | FK dependency
    FKDep
  | -- | FK reference dependency
    FKDepRef
  deriving (Eq, Generic, Hashable)

-- | Relationship between two tables.
data Relationship
  = Relationship
      { relTable :: QualifiedIdentifier
      , relForeignTable :: QualifiedIdentifier
      , relIsSelf :: Bool
      -- ^ Whether is a self relationship
      , relCardinality :: Cardinality
      , relTableIsView :: Bool
      , relFTableIsView :: Bool
      }
  | ComputedRelationship
      { relFunction :: QualifiedIdentifier
      , relTable :: QualifiedIdentifier
      , relForeignTable :: QualifiedIdentifier
      , relTableAlias :: QualifiedIdentifier
      , relToOne :: Bool
      , relIsSelf :: Bool
      }
  deriving (Eq, Generic, JSON.ToJSON, Ord, Show)

-- | The relationship cardinality
-- | https://en.wikipedia.org/wiki/Cardinality_(data_modeling)
data Cardinality
  = -- | one-to-many
    O2M {relCons :: FKConstraint, relColumns :: [(FieldName, FieldName)]}
  | -- | many-to-one
    M2O {relCons :: FKConstraint, relColumns :: [(FieldName, FieldName)]}
  | -- | one-to-one, this is a refinement over M2O, operating on it is pretty much the same as M2O when isParent == False
    O2O {relCons :: FKConstraint, relColumns :: [(FieldName, FieldName)], isParent :: Bool}
  | -- | many-to-many
    M2M Junction
  deriving (Eq, Generic, JSON.ToJSON, Ord, Show)

type FKConstraint = Text

-- | Junction table on an M2M relationship
data Junction = Junction
  { junTable :: QualifiedIdentifier
  , junConstraint1 :: FKConstraint
  , junConstraint2 :: FKConstraint
  , junColsSource :: [(FieldName, FieldName)]
  , junColsTarget :: [(FieldName, FieldName)]
  }
  deriving (Eq, Generic, JSON.ToJSON, Ord, Show)

-- | Key based on the source table and the foreign table schema
type RelationshipsMap = HM.HashMap (QualifiedIdentifier, Schema) [Relationship]

relIsToOne :: Relationship -> Bool
relIsToOne rel = case rel of
  Relationship{relCardinality = M2O{}} -> True
  Relationship{relCardinality = O2O{}} -> True
  ComputedRelationship{relToOne = True} -> True
  _ -> False

addViewPrimaryKeys :: TablesMap -> [ViewKeyDependency] -> TablesMap
addViewPrimaryKeys tabs keyDeps =
  ( \tbl@Table{tableSchema, tableName, tableIsView} ->
      if tableIsView then
        tbl{tablePKCols = findViewPKCols tableSchema tableName}
      else
        tbl
  )
    <$> tabs
  where
    findViewPKCols sch vw =
      concatMap (\ViewKeyDependency{keyDepCols} -> takeFirstPK keyDepCols) $
        fold $
          HM.lookup (PKDep, QualifiedIdentifier sch vw) indexedDeps
    takeFirstPK :: [(FieldName, [FieldName])] -> [FieldName]
    takeFirstPK = mapMaybe (headMay . snd)
    indexedDeps = HM.fromListWith (++) $ fmap ((keyDepType &&& keyDepView) &&& pure) keyDeps

addViewM2OAndO2ORels :: [ViewKeyDependency] -> [Relationship] -> [Relationship]
addViewM2OAndO2ORels keyDeps rels =
  rels ++ concatMap viewRels rels
  where
    isM2O card = case card of M2O _ _ -> True; _ -> False
    isO2O card = case card of O2O _ _ False -> True; _ -> False
    viewRels Relationship{relTable, relForeignTable, relCardinality = card}
      | isM2O card || isO2O card =
          let
            cons = relCons card
            relCols = relColumns card
            buildCard cns cls = if isM2O card then M2O cns cls else O2O cns cls False
            viewTableRels = fold $ HM.lookup (relTable, (cons, FKDep)) indexedKeyDeps
            tableViewRels = fold $ HM.lookup (relForeignTable, (cons, FKDepRef)) indexedKeyDeps
          in
            [ Relationship
                (keyDepView vwTbl)
                relForeignTable
                False
                (buildCard cons $ zipWith (\(_, vCol) (_, fCol) -> (vCol, fCol)) keyDepColsVwTbl relCols)
                True
                False
            | vwTbl <- viewTableRels
            , keyDepColsVwTbl <- expandKeyDepCols $ keyDepCols vwTbl
            ]
              ++ [ Relationship
                     relTable
                     (keyDepView tblVw)
                     False
                     (buildCard cons $ zipWith (\(tCol, _) (_, vCol) -> (tCol, vCol)) relCols keyDepColsTblVw)
                     False
                     True
                 | tblVw <- tableViewRels
                 , keyDepColsTblVw <- expandKeyDepCols $ keyDepCols tblVw
                 ]
              ++ [ let
                     vw1 = keyDepView vwTbl
                     vw2 = keyDepView tblVw
                   in
                     Relationship
                       vw1
                       vw2
                       (vw1 == vw2)
                       (buildCard cons $ zipWith (\(_, vcol1) (_, vcol2) -> (vcol1, vcol2)) keyDepColsVwTbl keyDepColsTblVw)
                       True
                       True
                 | vwTbl <- viewTableRels
                 , keyDepColsVwTbl <- expandKeyDepCols $ keyDepCols vwTbl
                 , tblVw <- tableViewRels
                 , keyDepColsTblVw <- expandKeyDepCols $ keyDepCols tblVw
                 ]
    viewRels _ = []
    expandKeyDepCols kdc = zip (fst <$> kdc) <$> traverse snd kdc
    indexedKeyDeps = HM.fromListWith (<>) $ fmap ((keyDepTable &&& keyDepCons &&& keyDepType) &&& pure) keyDeps
