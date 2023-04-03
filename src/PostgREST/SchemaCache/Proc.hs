{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.SchemaCache.Proc
  ( PgType(..)
  , ProcDescription(..)
  , ProcParam(..)
  , ProcVolatility(..)
  , ProcsMap
  , RetType(..)
  , procReturnsScalar
  , procReturnsSetOfScalar
  , procReturnsSingleComposite
  , procReturnsVoid
  , procTableName
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM

import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..),
                                          Schema, TableName)

import Protolude

data PgType
  = Scalar Bool -- True if the type is void
  | Composite QualifiedIdentifier
  deriving (Eq, Ord, Generic, JSON.ToJSON)

data RetType
  = Single PgType
  | SetOf PgType
  deriving (Eq, Ord, Generic, JSON.ToJSON)

data ProcVolatility
  = Volatile
  | Stable
  | Immutable
  deriving (Eq, Ord, Generic, JSON.ToJSON)

data ProcDescription = ProcDescription
  { pdSchema      :: Schema
  , pdName        :: Text
  , pdDescription :: Maybe Text
  , pdParams      :: [ProcParam]
  , pdReturnType  :: RetType
  , pdVolatility  :: ProcVolatility
  , pdHasVariadic :: Bool
  }
  deriving (Eq, Generic, JSON.ToJSON)

data ProcParam = ProcParam
  { ppName :: Text
  , ppType :: Text
  , ppReq  :: Bool
  , ppVar  :: Bool
  }
  deriving (Eq, Ord, Generic, JSON.ToJSON)

-- Order by least number of params in the case of overloaded functions
instance Ord ProcDescription where
  ProcDescription schema1 name1 des1 prms1 rt1 vol1 hasVar1 `compare` ProcDescription schema2 name2 des2 prms2 rt2 vol2 hasVar2
    | schema1 == schema2 && name1 == name2 && length prms1 < length prms2  = LT
    | schema2 == schema2 && name1 == name2 && length prms1 > length prms2  = GT
    | otherwise = (schema1, name1, des1, prms1, rt1, vol1, hasVar1) `compare` (schema2, name2, des2, prms2, rt2, vol2, hasVar2)

-- | A map of all procs, all of which can be overloaded(one entry will have more than one ProcDescription).
-- | It uses a HashMap for a faster lookup.
type ProcsMap = HM.HashMap QualifiedIdentifier [ProcDescription]

procReturnsScalar :: ProcDescription -> Bool
procReturnsScalar proc = case proc of
  ProcDescription{pdReturnType = Single (Scalar _)} -> True
  _                                                 -> False

procReturnsSetOfScalar :: ProcDescription -> Bool
procReturnsSetOfScalar proc = case proc of
  ProcDescription{pdReturnType = SetOf (Scalar _)} -> True
  _                                                -> False

procReturnsSingleComposite :: ProcDescription -> Bool
procReturnsSingleComposite proc = case proc of
  ProcDescription{pdReturnType = Single (Composite _)} -> True
  _                                                    -> False

procReturnsVoid :: ProcDescription -> Bool
procReturnsVoid proc = case proc of
  ProcDescription{pdReturnType = Single (Scalar True)} -> True
  _                                                    -> False

procTableName :: ProcDescription -> Maybe TableName
procTableName proc = case pdReturnType proc of
  SetOf  (Composite qi) -> Just $ qiName qi
  Single (Composite qi) -> Just $ qiName qi
  _                     -> Nothing
