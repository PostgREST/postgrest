{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Identifiers
  ( QualifiedIdentifier(..)
  , Schema
  , TableName
  , FieldName
  ) where

import qualified Data.Aeson as JSON

import Protolude


-- | Represents a pg identifier with a prepended schema name "schema.table".
-- When qiSchema is "", the schema is defined by the pg search_path.
data QualifiedIdentifier = QualifiedIdentifier
  { qiSchema :: Schema
  , qiName   :: TableName
  }
  deriving (Eq, Ord, Generic, JSON.ToJSON, JSON.ToJSONKey)

instance Hashable QualifiedIdentifier

type Schema = Text
type TableName = Text
type FieldName = Text
