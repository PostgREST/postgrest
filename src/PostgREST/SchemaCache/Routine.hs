{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.SchemaCache.Routine
  ( PgType(..)
  , Routine(..)
  , RoutineParam(..)
  , FuncVolatility(..)
  , RoutineMap
  , RetType(..)
  , funcReturnsScalar
  , funcReturnsSetOfScalar
  , funcReturnsSingleComposite
  , funcReturnsVoid
  , funcTableName
  , funcReturnsCompositeAlias
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM

import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..),
                                          Schema, TableName)

import Protolude

data PgType
  = Scalar Bool -- True if the type is void
  | Composite QualifiedIdentifier Bool -- True if the composite is a domain alias(used to work around a bug in pg 11 and 12, see QueryBuilder.hs)
  deriving (Eq, Ord, Generic, JSON.ToJSON)

data RetType
  = Single PgType
  | SetOf PgType
  deriving (Eq, Ord, Generic, JSON.ToJSON)

data FuncVolatility
  = Volatile
  | Stable
  | Immutable
  deriving (Eq, Ord, Generic, JSON.ToJSON)

data Routine = Function
  { pdSchema      :: Schema
  , pdName        :: Text
  , pdDescription :: Maybe Text
  , pdParams      :: [RoutineParam]
  , pdReturnType  :: RetType
  , pdVolatility  :: FuncVolatility
  , pdHasVariadic :: Bool
  }
  deriving (Eq, Generic, JSON.ToJSON)

data RoutineParam = RoutineParam
  { ppName :: Text
  , ppType :: Text
  , ppReq  :: Bool
  , ppVar  :: Bool
  }
  deriving (Eq, Ord, Generic, JSON.ToJSON)

-- Order by least number of params in the case of overloaded functions
instance Ord Routine where
  Function schema1 name1 des1 prms1 rt1 vol1 hasVar1 `compare` Function schema2 name2 des2 prms2 rt2 vol2 hasVar2
    | schema1 == schema2 && name1 == name2 && length prms1 < length prms2  = LT
    | schema2 == schema2 && name1 == name2 && length prms1 > length prms2  = GT
    | otherwise = (schema1, name1, des1, prms1, rt1, vol1, hasVar1) `compare` (schema2, name2, des2, prms2, rt2, vol2, hasVar2)

-- | A map of all procs, all of which can be overloaded(one entry will have more than one Routine).
-- | It uses a HashMap for a faster lookup.
type RoutineMap = HM.HashMap QualifiedIdentifier [Routine]

funcReturnsScalar :: Routine -> Bool
funcReturnsScalar proc = case proc of
  Function{pdReturnType = Single (Scalar _)} -> True
  _                                          -> False

funcReturnsSetOfScalar :: Routine -> Bool
funcReturnsSetOfScalar proc = case proc of
  Function{pdReturnType = SetOf (Scalar _)} -> True
  _                                         -> False

funcReturnsCompositeAlias :: Routine -> Bool
funcReturnsCompositeAlias proc = case proc of
  Function{pdReturnType = Single (Composite _ True)} -> True
  Function{pdReturnType = SetOf (Composite _ True)}  -> True
  _                                                  -> False

funcReturnsSingleComposite :: Routine -> Bool
funcReturnsSingleComposite proc = case proc of
  Function{pdReturnType = Single (Composite _ _)} -> True
  _                                               -> False

funcReturnsVoid :: Routine -> Bool
funcReturnsVoid proc = case proc of
  Function{pdReturnType = Single (Scalar True)} -> True
  _                                             -> False

funcTableName :: Routine -> Maybe TableName
funcTableName proc = case pdReturnType proc of
  SetOf  (Composite qi _) -> Just $ qiName qi
  Single (Composite qi _) -> Just $ qiName qi
  _                       -> Nothing
