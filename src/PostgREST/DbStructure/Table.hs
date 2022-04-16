{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Table
  ( Column(..)
  , Table(..)
  , TablesMap
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as M

import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema, TableName)

import Protolude


data Table = Table
  { tableSchema      :: Schema
  , tableName        :: TableName
  , tableDescription :: Maybe Text
    -- TODO Find a better way to separate tables and views
  , tableIsView      :: Bool
    -- The following fields identify what can be done on the table/view, they're not related to the privileges granted to it
  , tableInsertable  :: Bool
  , tableUpdatable   :: Bool
  , tableDeletable   :: Bool
  , tablePKCols      :: [FieldName]
  }
  deriving (Show, Ord, Generic, JSON.ToJSON)

instance Eq Table where
  Table{tableSchema=s1,tableName=n1} == Table{tableSchema=s2,tableName=n2} = s1 == s2 && n1 == n2

data Column = Column
  { colTable       :: Table
  , colName        :: FieldName
  , colDescription :: Maybe Text
  , colNullable    :: Bool
  , colType        :: Text
  , colMaxLen      :: Maybe Int32
  , colDefault     :: Maybe Text
  , colEnum        :: [Text]
  }
  deriving (Ord, Generic, JSON.ToJSON)

instance Eq Column where
  Column{colTable=t1,colName=n1} == Column{colTable=t2,colName=n2} = t1 == t2 && n1 == n2

type TablesMap = M.HashMap QualifiedIdentifier Table
