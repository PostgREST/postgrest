{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.SchemaCache.Table
  ( Column(..)
  , Table(..)
  , TablesMap
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM

import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema, TableName)

import Protolude


data Table = Table
  { tableSchema      :: Schema
  , tableName        :: TableName
  , tableDescription :: Maybe Text
     -- TODO Find a better way to separate tables and views
   , tableIsView     :: Bool
    -- The following fields identify what can be done on the table/view, they're not related to the privileges granted to it
  , tableInsertable  :: Bool
  , tableUpdatable   :: Bool
  , tableDeletable   :: Bool
  , tablePKCols      :: [FieldName]
  , tableColumns     :: [Column]
  }
  deriving (Show, Ord, Generic, JSON.ToJSON)

instance Eq Table where
  Table{tableSchema=s1,tableName=n1} == Table{tableSchema=s2,tableName=n2} = s1 == s2 && n1 == n2

data Column = Column
  { colName        :: FieldName
  , colDescription :: Maybe Text
  , colNullable    :: Bool
  , colType        :: Text
  , colMaxLen      :: Maybe Int32
  , colDefault     :: Maybe Text
  , colEnum        :: [Text]
  }
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

type TablesMap = HM.HashMap QualifiedIdentifier Table
