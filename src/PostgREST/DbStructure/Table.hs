{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Table
  ( Column(..)
  , ForeignKey(..)
  , Table(..)
  , tableQi
  ) where

import qualified Data.Aeson as JSON

import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema, TableName)

import Protolude


data Table = Table
  { tableSchema      :: Schema
  , tableName        :: TableName
  , tableDescription :: Maybe Text
  , tableInsertable  :: Bool
  }
  deriving (Show, Ord, Generic, JSON.ToJSON)

instance Eq Table where
  Table{tableSchema=s1,tableName=n1} == Table{tableSchema=s2,tableName=n2} = s1 == s2 && n1 == n2

tableQi :: Table -> QualifiedIdentifier
tableQi Table{tableSchema=s, tableName=n} = QualifiedIdentifier s n

newtype ForeignKey = ForeignKey
  { fkCol :: Column }
  deriving (Eq, Ord, Generic, JSON.ToJSON)

data Column = Column
  { colTable       :: Table
  , colName        :: FieldName
  , colDescription :: Maybe Text
  , colNullable    :: Bool
  , colType        :: Text
  , colMaxLen      :: Maybe Int32
  , colDefault     :: Maybe Text
  , colEnum        :: [Text]
  , colFK          :: Maybe ForeignKey
  }
  deriving (Ord, Generic, JSON.ToJSON)

instance Eq Column where
  Column{colTable=t1,colName=n1} == Column{colTable=t2,colName=n2} = t1 == t2 && n1 == n2

data PrimaryKey = PrimaryKey
  { pkTable :: Table
  , pkName  :: Text
  }
  deriving (Generic, JSON.ToJSON)
