{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Proc
  ( PgType(..)
  , ProcDescription(..)
  , ProcParam(..)
  , ProcVolatility(..)
  , ProcsMap
  , RetType(..)
  , procReturnsScalar
  , procReturnsSingle
  , procTableName
  , specifiedProcParams
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S

import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema, TableName)

import Protolude

data PgType
  = Scalar
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
type ProcsMap = M.HashMap QualifiedIdentifier [ProcDescription]

{-|
  Search the procedure parameters by matching them with the specified keys.
  If the key doesn't match a parameter, a parameter with a default type "text" is assumed.
-}
specifiedProcParams :: S.Set FieldName -> ProcDescription -> [ProcParam]
specifiedProcParams keys proc =
  (\k -> fromMaybe (ProcParam k "text" True False) (find ((==) k . ppName) (pdParams proc))) <$> S.toList keys

procReturnsScalar :: ProcDescription -> Bool
procReturnsScalar proc = case proc of
  ProcDescription{pdReturnType = (Single Scalar)} -> True
  ProcDescription{pdReturnType = (SetOf Scalar)}  -> True
  _                                               -> False

procReturnsSingle :: ProcDescription -> Bool
procReturnsSingle proc = case proc of
  ProcDescription{pdReturnType = (Single _)} -> True
  _                                          -> False

procTableName :: ProcDescription -> Maybe TableName
procTableName proc = case pdReturnType proc of
  SetOf  (Composite qi) -> Just $ qiName qi
  Single (Composite qi) -> Just $ qiName qi
  _                     -> Nothing
