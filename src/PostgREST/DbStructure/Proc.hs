{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Proc
  ( PgArg(..)
  , PgType(..)
  , ProcDescription(..)
  , ProcVolatility(..)
  , ProcsMap
  , RetType(..)
  , findProc
  , procReturnsScalar
  , procReturnsSingle
  , procTableName
  , specifiedProcArgs
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S

import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema, TableName)

import Protolude


data PgArg = PgArg
  { pgaName :: Text
  , pgaType :: Text
  , pgaReq  :: Bool
  , pgaVar  :: Bool
  }
  deriving (Eq, Ord, Generic, JSON.ToJSON)

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
  , pdArgs        :: [PgArg]
  , pdReturnType  :: RetType
  , pdVolatility  :: ProcVolatility
  , pdHasVariadic :: Bool
  }
  deriving (Eq, Generic, JSON.ToJSON)

-- Order by least number of args in the case of overloaded functions
instance Ord ProcDescription where
  ProcDescription schema1 name1 des1 args1 rt1 vol1 hasVar1 `compare` ProcDescription schema2 name2 des2 args2 rt2 vol2 hasVar2
    | schema1 == schema2 && name1 == name2 && length args1 < length args2  = LT
    | schema2 == schema2 && name1 == name2 && length args1 > length args2  = GT
    | otherwise = (schema1, name1, des1, args1, rt1, vol1, hasVar1) `compare` (schema2, name2, des2, args2, rt2, vol2, hasVar2)

-- | A map of all procs, all of which can be overloaded(one entry will have more than one ProcDescription).
-- | It uses a HashMap for a faster lookup.
type ProcsMap = M.HashMap QualifiedIdentifier [ProcDescription]

{-|
  Search a pg procedure by its parameters. Since a function can be overloaded, the name is not enough to find it.
  An overloaded function can have a different volatility or even a different return type.
  Ideally, handling overloaded functions should be left to pg itself. But we need to know certain proc attributes in advance.
-}
findProc :: QualifiedIdentifier -> S.Set Text -> Bool -> ProcsMap -> ProcDescription
findProc qi payloadKeys paramsAsSingleObject allProcs = fromMaybe fallback bestMatch
  where
    -- instead of passing Maybe ProcDescription around, we create a fallback description here when we can't find a matching function
    -- args is empty, but because "specifiedProcArgs" will fill the missing arguments with default type text, this is not a problem
    fallback = ProcDescription (qiSchema qi) (qiName qi) Nothing mempty (SetOf $ Composite $ QualifiedIdentifier mempty "record") Volatile False
    bestMatch =
      case M.lookup qi allProcs of
        Nothing     -> Nothing
        Just [proc] -> Just proc           -- if it's not an overloaded function then immediately get the ProcDescription
        Just procs  -> find matches procs  -- Handle overloaded functions case
    matches proc =
      if paramsAsSingleObject
        -- if the arg is not of json type let the db give the err
        then length (pdArgs proc) == 1
        else payloadKeys `S.isSubsetOf` S.fromList (pgaName <$> pdArgs proc)

{-|
  Search the procedure parameters by matching them with the specified keys.
  If the key doesn't match a parameter, a parameter with a default type "text" is assumed.
-}
specifiedProcArgs :: S.Set FieldName -> ProcDescription -> [PgArg]
specifiedProcArgs keys proc =
  (\k -> fromMaybe (PgArg k "text" True False) (find ((==) k . pgaName) (pdArgs proc))) <$> S.toList keys

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
