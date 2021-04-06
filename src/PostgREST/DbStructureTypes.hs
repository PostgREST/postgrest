{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.DbStructureTypes where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S

import PostgREST.PgVersions (PgVersion(..))

import qualified GHC.Show (show)

import Protolude


data DbStructure = DbStructure
  { dbTables      :: [Table]
  , dbColumns     :: [Column]
  , dbRelations   :: [Relation]
  , dbPrimaryKeys :: [PrimaryKey]
  , dbProcs       :: ProcsMap
  , pgVersion     :: PgVersion
  }
  deriving (Generic, JSON.ToJSON)

-- TODO Table could hold references to all its Columns
tableCols :: DbStructure -> Schema -> TableName -> [Column]
tableCols dbs tSchema tName = filter (\Column{colTable=Table{tableSchema=s, tableName=t}} -> s==tSchema && t==tName) $ dbColumns dbs

-- TODO Table could hold references to all its PrimaryKeys
tablePKCols :: DbStructure -> Schema -> TableName -> [Text]
tablePKCols dbs tSchema tName =  pkName <$> filter (\pk -> tSchema == (tableSchema . pkTable) pk && tName == (tableName . pkTable) pk) (dbPrimaryKeys dbs)

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

type Schema = Text
type TableName = Text

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

newtype ForeignKey = ForeignKey { fkCol :: Column } deriving (Eq, Ord, Generic, JSON.ToJSON)

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

data PrimaryKey = PrimaryKey {
    pkTable :: Table
  , pkName  :: Text
} deriving (Generic, JSON.ToJSON)


{-|
  Represents a pg identifier with a prepended schema name "schema.table"
  When qiSchema is "", the schema is defined by the pg search_path
-}
data QualifiedIdentifier = QualifiedIdentifier {
  qiSchema :: Schema
, qiName   :: TableName
} deriving (Eq, Ord, Generic, JSON.ToJSON, JSON.ToJSONKey)
instance Hashable QualifiedIdentifier

-- | The relationship [cardinality](https://en.wikipedia.org/wiki/Cardinality_(data_modeling)).
-- | TODO: missing one-to-one
data Cardinality = O2M -- ^ one-to-many,  previously known as Parent
                 | M2O -- ^ many-to-one,  previously known as Child
                 | M2M -- ^ many-to-many, previously known as Many
                 deriving (Eq, Generic, JSON.ToJSON)
instance Show Cardinality where
  show O2M = "o2m"
  show M2O = "m2o"
  show M2M = "m2m"

{-|
  "Relation"ship between two tables.
  The order of the relColumns and relFColumns should be maintained to get the join conditions right.
  TODO merge relColumns and relFColumns to a tuple or Data.Bimap
-}
data Relation = Relation {
  relTable    :: Table
, relColumns  :: [Column]
, relFTable   :: Table
, relFColumns :: [Column]
, relType     :: Cardinality
, relLink     :: Link -- ^ Constraint on O2M/M2O, Junction for M2M Cardinality
} deriving (Eq, Generic, JSON.ToJSON)

type ConstraintName = Text

-- | Junction table on an M2M relationship
data Link
  = Constraint { constName :: ConstraintName }
  | Junction {
    junTable :: Table
  , junLink1 :: Link
  , junCols1 :: [Column]
  , junLink2 :: Link
  , junCols2 :: [Column]
  }
  deriving (Eq, Generic, JSON.ToJSON)

isSelfReference :: Relation -> Bool
isSelfReference r = relTable r == relFTable r

type FieldName = Text
