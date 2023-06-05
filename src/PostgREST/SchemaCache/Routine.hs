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

import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as JSON
import qualified Data.HashMap.Strict        as HM
import qualified Hasql.Transaction.Sessions as SQL

import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..),
                                          Schema, TableName)

import Protolude

data PgType
  = Scalar QualifiedIdentifier
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
  , pdIsoLvl      :: Maybe SQL.IsolationLevel
  }
  deriving (Eq, Generic)
-- need to define JSON manually bc SQL.IsolationLevel doesn't have a JSON instance(and we can't define one for that type without getting a compiler error)
instance JSON.ToJSON Routine where
  toJSON (Function sch nam desc params ret vol hasVar _) = JSON.object
    [
      "pdSchema"      .= sch
    , "pdName"        .= nam
    , "pdDescription" .= desc
    , "pdParams"      .= JSON.toJSON params
    , "pdReturnType"  .= JSON.toJSON ret
    , "pdVolatility"  .= JSON.toJSON vol
    , "pdHasVariadic" .= JSON.toJSON hasVar
    ]

data RoutineParam = RoutineParam
  { ppName :: Text
  , ppType :: Text
  , ppReq  :: Bool
  , ppVar  :: Bool
  }
  deriving (Eq, Ord, Generic, JSON.ToJSON)

-- Order by least number of params in the case of overloaded functions
instance Ord Routine where
  Function schema1 name1 des1 prms1 rt1 vol1 hasVar1 iso1 `compare` Function schema2 name2 des2 prms2 rt2 vol2 hasVar2 iso2
    | schema1 == schema2 && name1 == name2 && length prms1 < length prms2  = LT
    | schema2 == schema2 && name1 == name2 && length prms1 > length prms2  = GT
    | otherwise = (schema1, name1, des1, prms1, rt1, vol1, hasVar1, iso1) `compare` (schema2, name2, des2, prms2, rt2, vol2, hasVar2, iso2)

-- | A map of all procs, all of which can be overloaded(one entry will have more than one Routine).
-- | It uses a HashMap for a faster lookup.
type RoutineMap = HM.HashMap QualifiedIdentifier [Routine]

funcReturnsScalar :: Routine -> Bool
funcReturnsScalar proc = case proc of
  Function{pdReturnType = Single (Scalar{})} -> True
  _                                          -> False

funcReturnsSetOfScalar :: Routine -> Bool
funcReturnsSetOfScalar proc = case proc of
  Function{pdReturnType = SetOf (Scalar{})} -> True
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
  Function{pdReturnType = Single (Scalar (QualifiedIdentifier "pg_catalog" "void"))} -> True
  _                                                                                  -> False

funcTableName :: Routine -> Maybe TableName
funcTableName proc = case pdReturnType proc of
  SetOf  (Composite qi _) -> Just $ qiName qi
  Single (Composite qi _) -> Just $ qiName qi
  _                       -> Nothing
