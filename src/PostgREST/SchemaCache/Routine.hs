{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.SchemaCache.Routine
  ( PgType(..)
  , Routine(..)
  , FuncIdent(..)
  , RoutineParam(..)
  , FuncVolatility(..)
  , FuncSettings
  , RoutineMap
  , RetType(..)
  , funcReturnsScalar
  , funcReturnsSetOfScalar
  , funcReturnsSingleComposite
  , funcReturnsVoid
  , funcTableName
  , funcReturnsSingle
  , MediaHandlerMap
  , ResolvedHandler
  , MediaHandler(..)
  ) where

import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as JSON
import qualified Data.HashMap.Strict        as HM
import qualified Hasql.Transaction.Sessions as SQL
import qualified PostgREST.MediaType        as MediaType

import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..),
                                          RelIdentifier (..), Schema,
                                          TableName)


import Protolude

data PgType
  = Scalar QualifiedIdentifier
  | Composite QualifiedIdentifier Bool -- True if the composite is a domain alias(used to work around a bug in pg 11 and 12, see QueryBuilder.hs)
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

data RetType
  = Single PgType
  | SetOf PgType
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

data FuncVolatility
  = Volatile
  | Stable
  | Immutable
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

type FuncSettings = [(Text,Text)]

-- In PostgreSQL a function identity is defined by schema + name + parameters
data FuncIdent = FuncIdent
  { pdSchema :: Schema
  , pdName   :: Text
  , pdParams :: [RoutineParam]
  }
  deriving (Eq, Show, Ord, Generic)

data Routine = Function
  { pdIdent        :: FuncIdent
  , pdDescription  :: Maybe Text
  , pdReturnType   :: RetType
  , pdVolatility   :: FuncVolatility
  , pdHasVariadic  :: Bool
  , pdIsoLvl       :: Maybe SQL.IsolationLevel
  , pdFuncSettings :: FuncSettings
  }
  deriving (Eq, Show, Generic)
-- need to define JSON manually bc SQL.IsolationLevel doesn't have a JSON instance(and we can't define one for that type without getting a compiler error)
instance JSON.ToJSON Routine where
  toJSON (Function (FuncIdent sch nam params) desc ret vol hasVar _ sets) = JSON.object
    [
      "pdSchema"       .= sch
    , "pdName"         .= nam
    , "pdDescription"  .= desc
    , "pdParams"       .= JSON.toJSON params
    , "pdReturnType"   .= JSON.toJSON ret
    , "pdVolatility"   .= JSON.toJSON vol
    , "pdHasVariadic"  .= JSON.toJSON hasVar
    , "pdFuncSettings" .= JSON.toJSON sets
    ]

-- Order by least number of params in the case of overloaded functions
instance Ord Routine where
  Function (FuncIdent schema1 name1 prms1) des1 rt1 vol1 hasVar1 iso1 sets1 `compare` Function (FuncIdent schema2 name2 prms2) des2 rt2 vol2 hasVar2 iso2 sets2
    | schema1 == schema2 && name1 == name2 && length prms1 < length prms2  = LT
    | schema2 == schema2 && name1 == name2 && length prms1 > length prms2  = GT
    | otherwise = (schema1, name1, des1, prms1, rt1, vol1, hasVar1, iso1, sets1) `compare` (schema2, name2, des2, prms2, rt2, vol2, hasVar2, iso2, sets2)

data RoutineParam = RoutineParam
  { ppName          :: Text
  , ppType          :: Text
  , ppTypeMaxLength :: Text
  , ppReq           :: Bool
  , ppVar           :: Bool
  }
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

-- | A map of all procs, all of which can be overloaded(one entry will have more than one Routine).
-- | It uses a HashMap for a faster lookup.
type RoutineMap = HM.HashMap QualifiedIdentifier [Routine]

-- | A media handler can be an aggregate over a composite type or a function over a scalar
data MediaHandler
   -- non overridable builtins
   = BuiltinAggSingleJson Bool
   | BuiltinAggArrayJsonStrip
   -- these builtins are overridable
   | BuiltinOvAggJson
   | BuiltinOvAggGeoJson
   | BuiltinOvAggCsv
   -- custom
   | CustomFunc QualifiedIdentifier RelIdentifier
   | NoAgg
   deriving (Eq, Show, Generic, JSON.ToJSON)

funcReturnsSingle :: Routine -> Bool
funcReturnsSingle proc = case proc of
  Function{pdReturnType = Single _} -> True
  _                                 -> False

funcReturnsScalar :: Routine -> Bool
funcReturnsScalar proc = case proc of
  Function{pdReturnType = Single (Scalar{})} -> True
  _                                          -> False

funcReturnsSetOfScalar :: Routine -> Bool
funcReturnsSetOfScalar proc = case proc of
  Function{pdReturnType = SetOf (Scalar{})} -> True
  _                                         -> False

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

-- the resolved handler also carries the media type because MTAny (*/*) is resolved to a different media type
type ResolvedHandler = (MediaHandler, MediaType.MediaType)
type MediaHandlerMap = HM.HashMap (RelIdentifier, MediaType.MediaType) ResolvedHandler
