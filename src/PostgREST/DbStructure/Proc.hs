{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructure.Proc
  ( PgArg(..)
  , PgType(..)
  , ProcDescription(..)
  , ProcVolatility(..)
  , ProcsMap
  , RetType(..)
  , filterProc
  , procReturnsScalar
  , procReturnsSingle
  , procTableName
  , specifiedProcArgs
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import qualified Data.Set            as S
import qualified Data.Text           as T

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
filterProc :: QualifiedIdentifier -> S.Set Text -> Bool -> ProcsMap -> [ProcDescription]
filterProc qi payloadKeys paramsAsSingleObject allProcs = bestMatch
  where
    bestMatch =
      case M.lookup qi allProcs of
        Nothing     -> []
        Just [proc] -> [proc | matches proc]
        Just procs  -> filter matches procs
    -- Find the exact arguments match
    matches proc
      | paramsAsSingleObject = case pdArgs proc of
                               [arg] -> pgaType arg `elem` ["json", "jsonb"]
                               _     -> False
      | otherwise            = case pdArgs proc of
                               []   -> null payloadKeys
                               args -> matchesArg args
    matchesArg args =
      -- The function's required arguments are separated from the ones with a default value assigned.
      -- The set of keys supplied by the client are compared with a set of those arguments' names.
      -- 1. If only required arguments are found, the keys must be exactly the same as those arguments
      -- 2. If only optional arguments are found, the keys must be a subset of those arguments
      -- 3. If both required and optional arguments are found, the result of taking away the optional arguments
      --    from the keys must be exactly the same as the required arguments
      case L.partition pgaReq args of
        (reqArgs, [])      -> payloadKeys == S.fromList (pgaName <$> reqArgs)
        ([], defArgs)      -> payloadKeys `S.isSubsetOf` S.fromList (pgaName <$> defArgs)
        (reqArgs, defArgs) -> payloadKeys `S.difference` S.fromList (pgaName <$> defArgs) == S.fromList (pgaName <$> reqArgs)

{-|
  Search the procedure parameters by matching them with the specified keys.
  If the key doesn't match a parameter, a parameter with a default type "text" is assumed.
-}
specifiedProcArgs :: S.Set FieldName -> ProcDescription -> [PgArg]
specifiedProcArgs keys proc =
  (\k -> fromMaybe (PgArg k "text" True False) (find ((==) k . pgaName) (pdArgs proc))) <$> S.toList (S.map (fst . T.breakOn "::") keys)

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
