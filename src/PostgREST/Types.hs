{-|
Module      : PostgREST.Types
Description : PostgREST common types and functions used by the rest of the modules
-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PostgREST.Types where


import qualified Data.Aeson               as JSON
import qualified Data.ByteString.Lazy     as BL
import qualified Data.CaseInsensitive     as CI
import qualified Data.HashMap.Strict      as M
import qualified Data.Set                 as S

import Control.Lens.Getter (view)
import Control.Lens.Tuple  (_1)
import Data.Tree (Tree(..))
import Network.HTTP.Types.Header (Header)

import qualified GHC.Show (show)

import PostgREST.RangeQuery (NonnegRange)
import Protolude            hiding (toS)
import Protolude.Conv       (toS)


-- | A SQL query that can be executed independently
type SqlQuery = ByteString

-- | A part of a SQL query that cannot be executed independently
type SqlFragment = ByteString

data PreferResolution = MergeDuplicates | IgnoreDuplicates
instance Show PreferResolution where
  show MergeDuplicates  = "resolution=merge-duplicates"
  show IgnoreDuplicates = "resolution=ignore-duplicates"

-- | How to return the mutated data. From https://tools.ietf.org/html/rfc7240#section-4.2
data PreferRepresentation = Full        -- ^ Return the body plus the Location header(in case of POST).
                          | HeadersOnly -- ^ Return the Location header(in case of POST). This needs a SELECT privilege on the pk.
                          | None        -- ^ Return nothing from the mutated data.
                          deriving Eq
instance Show PreferRepresentation where
  show Full        = "return=representation"
  show None        = "return=minimal"
  show HeadersOnly = mempty

data PreferParameters
  = SingleObject    -- ^ Pass all parameters as a single json object to a stored procedure
  | MultipleObjects -- ^ Pass an array of json objects as params to a stored procedure
  deriving Eq

instance Show PreferParameters where
  show SingleObject    = "params=single-object"
  show MultipleObjects = "params=multiple-objects"

data PreferCount
  = ExactCount     -- ^ exact count(slower)
  | PlannedCount   -- ^ PostgreSQL query planner rows count guess. Done by using EXPLAIN {query}.
  | EstimatedCount -- ^ use the query planner rows if the count is superior to max-rows, otherwise get the exact count.
  deriving Eq

instance Show PreferCount where
  show ExactCount     = "count=exact"
  show PlannedCount   = "count=planned"
  show EstimatedCount = "count=estimated"

data PreferTransaction
  = Commit   -- Commit transaction - the default.
  | Rollback -- Rollback transaction after sending the response - does not persist changes, e.g. for running tests.
  deriving Eq

instance Show PreferTransaction where
  show Commit   = "tx=commit"
  show Rollback = "tx=rollback"

data DbStructure = DbStructure {
  dbTables      :: [Table]
, dbColumns     :: [Column]
, dbRelations   :: [Relation]
, dbPrimaryKeys :: [PrimaryKey]
, dbProcs       :: ProcsMap
, pgVersion     :: PgVersion
} deriving (Generic, JSON.ToJSON)

-- TODO Table could hold references to all its Columns
tableCols :: DbStructure -> Schema -> TableName -> [Column]
tableCols dbs tSchema tName = filter (\Column{colTable=Table{tableSchema=s, tableName=t}} -> s==tSchema && t==tName) $ dbColumns dbs

-- TODO Table could hold references to all its PrimaryKeys
tablePKCols :: DbStructure -> Schema -> TableName -> [Text]
tablePKCols dbs tSchema tName =  pkName <$> filter (\pk -> tSchema == (tableSchema . pkTable) pk && tName == (tableName . pkTable) pk) (dbPrimaryKeys dbs)

data PgArg = PgArg {
  pgaName :: Text
, pgaType :: Text
, pgaReq  :: Bool
, pgaVar  :: Bool
} deriving (Eq, Ord, Generic, JSON.ToJSON)

data PgType = Scalar | Composite QualifiedIdentifier deriving (Eq, Ord, Generic, JSON.ToJSON)

data RetType = Single PgType | SetOf PgType deriving (Eq, Ord, Generic, JSON.ToJSON)

data ProcVolatility = Volatile | Stable | Immutable
  deriving (Eq, Ord, Generic, JSON.ToJSON)

data ProcDescription = ProcDescription {
  pdSchema      :: Schema
, pdName        :: Text
, pdDescription :: Maybe Text
, pdArgs        :: [PgArg]
, pdReturnType  :: RetType
, pdVolatility  :: ProcVolatility
, pdHasVariadic :: Bool
} deriving (Eq, Generic, JSON.ToJSON)

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

data Table = Table {
  tableSchema      :: Schema
, tableName        :: TableName
, tableDescription :: Maybe Text
, tableInsertable  :: Bool
} deriving (Show, Ord, Generic, JSON.ToJSON)

instance Eq Table where
  Table{tableSchema=s1,tableName=n1} == Table{tableSchema=s2,tableName=n2} = s1 == s2 && n1 == n2

tableQi :: Table -> QualifiedIdentifier
tableQi Table{tableSchema=s, tableName=n} = QualifiedIdentifier s n

newtype ForeignKey = ForeignKey { fkCol :: Column } deriving (Eq, Ord, Generic, JSON.ToJSON)

data Column =
    Column {
      colTable       :: Table
    , colName        :: FieldName
    , colDescription :: Maybe Text
    , colNullable    :: Bool
    , colType        :: Text
    , colMaxLen      :: Maybe Int32
    , colDefault     :: Maybe Text
    , colEnum        :: [Text]
    , colFK          :: Maybe ForeignKey
    } deriving (Ord, Generic, JSON.ToJSON)

instance Eq Column where
  Column{colTable=t1,colName=n1} == Column{colTable=t2,colName=n2} = t1 == t2 && n1 == n2

-- | The source table column a view column refers to
type SourceColumn = (Column, ViewColumn)
type ViewColumn = Column

data PrimaryKey = PrimaryKey {
    pkTable :: Table
  , pkName  :: Text
} deriving (Generic, JSON.ToJSON)

data OrderDirection = OrderAsc | OrderDesc deriving (Eq)
instance Show OrderDirection where
  show OrderAsc  = "ASC"
  show OrderDesc = "DESC"

data OrderNulls = OrderNullsFirst | OrderNullsLast deriving (Eq)
instance Show OrderNulls where
  show OrderNullsFirst = "NULLS FIRST"
  show OrderNullsLast  = "NULLS LAST"

data OrderTerm = OrderTerm {
  otTerm      :: Field
, otDirection :: Maybe OrderDirection
, otNullOrder :: Maybe OrderNulls
} deriving (Eq)

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

data PayloadJSON =
  -- | Cached attributes of a JSON payload
  ProcessedJSON {
    -- | This is the raw ByteString that comes from the request body.
    -- We cache this instead of an Aeson Value because it was detected that for large payloads the encoding
    -- had high memory usage, see https://github.com/PostgREST/postgrest/pull/1005 for more details
    pjRaw  :: BL.ByteString
    -- | Keys of the object or if it's an array these keys are guaranteed to be the same across all its objects
  , pjKeys :: S.Set Text
  }|
  RawJSON {
    pjRaw  :: BL.ByteString
  }

data PJType = PJArray { pjaLength :: Int } | PJObject

data Proxy = Proxy {
  proxyScheme :: Text
, proxyHost   :: Text
, proxyPort   :: Integer
, proxyPath   :: Text
}

type Operator = Text
operators :: M.HashMap Operator SqlFragment
operators = M.union (M.fromList [
  ("eq", "="),
  ("gte", ">="),
  ("gt", ">"),
  ("lte", "<="),
  ("lt", "<"),
  ("neq", "<>"),
  ("like", "LIKE"),
  ("ilike", "ILIKE"),
  ("in", "IN"),
  ("is", "IS"),
  ("cs", "@>"),
  ("cd", "<@"),
  ("ov", "&&"),
  ("sl", "<<"),
  ("sr", ">>"),
  ("nxr", "&<"),
  ("nxl", "&>"),
  ("adj", "-|-")]) ftsOperators

ftsOperators :: M.HashMap Operator SqlFragment
ftsOperators = M.fromList [
  ("fts", "@@ to_tsquery"),
  ("plfts", "@@ plainto_tsquery"),
  ("phfts", "@@ phraseto_tsquery"),
  ("wfts", "@@ websearch_to_tsquery")
  ]

data OpExpr = OpExpr Bool Operation deriving (Eq)
data Operation = Op Operator SingleVal |
                 In ListVal |
                 Fts Operator (Maybe Language) SingleVal deriving (Eq)
type Language = Text

-- | Represents a single value in a filter, e.g. id=eq.singleval
type SingleVal = Text
-- | Represents a list value in a filter, e.g. id=in.(val1,val2,val3)
type ListVal = [Text]

data LogicOperator = And | Or deriving Eq
instance Show LogicOperator where
  show And = "AND"
  show Or  = "OR"
{-|
  Boolean logic expression tree e.g. "and(name.eq.N,or(id.eq.1,id.eq.2))" is:

            And
           /   \
  name.eq.N     Or
               /  \
         id.eq.1   id.eq.2
-}
data LogicTree = Expr Bool LogicOperator [LogicTree] | Stmnt Filter deriving (Eq)

type FieldName = Text
{-|
  Json path operations as specified in https://www.postgresql.org/docs/current/static/functions-json.html
-}
type JsonPath = [JsonOperation]
-- | Represents the single arrow `->` or double arrow `->>` operators
data JsonOperation = JArrow{jOp :: JsonOperand} | J2Arrow{jOp :: JsonOperand} deriving (Eq)
-- | Represents the key(`->'key'`) or index(`->'1`::int`), the index is Text because we reuse our escaping functons and let pg do the casting with '1'::int
data JsonOperand = JKey{jVal :: Text} | JIdx{jVal :: Text} deriving (Eq)

type Field = (FieldName, JsonPath)
type Alias = Text
type Cast = Text
type NodeName = Text


{-|
  Custom guc header, it's obtained by parsing the json in a:
  `SET LOCAL "response.headers" = '[{"Set-Cookie": ".."}]'
-}
newtype GucHeader = GucHeader (CI.CI ByteString, ByteString)

instance JSON.FromJSON GucHeader where
  parseJSON (JSON.Object o) = case headMay (M.toList o) of
    Just (k, JSON.String s) | M.size o == 1 -> pure $ GucHeader (CI.mk $ toS k, toS s)
                            | otherwise     -> mzero
    _ -> mzero
  parseJSON _          = mzero

unwrapGucHeader :: GucHeader -> Header
unwrapGucHeader (GucHeader (k, v)) = (k, v)

-- | Add headers not already included to allow the user to override them instead of duplicating them
addHeadersIfNotIncluded :: [Header] -> [Header] -> [Header]
addHeadersIfNotIncluded newHeaders initialHeaders =
  filter (\(nk, _) -> isNothing $ find (\(ik, _) -> ik == nk) initialHeaders) newHeaders ++
  initialHeaders

{-|
  This type will hold information about which particular 'Relation' between two tables to choose when there are multiple ones.
  Specifically, it will contain the name of the foreign key or the join table in many to many relations.
-}
type SelectItem = (Field, Maybe Cast, Maybe Alias, Maybe EmbedHint)
-- | Disambiguates an embedding operation when there's multiple relationships between two tables.
-- | Can be the name of a foreign key constraint, column name or the junction in an m2m relationship.
type EmbedHint = Text
-- | Path of the embedded levels, e.g "clients.projects.name=eq.." gives Path ["clients", "projects"]
type EmbedPath = [Text]
data Filter = Filter { field::Field, opExpr::OpExpr } deriving (Eq)
data JoinCondition = JoinCondition (QualifiedIdentifier, FieldName)
                                   (QualifiedIdentifier, FieldName) deriving (Eq)

data ReadQuery = Select {
    select         :: [SelectItem]
  , from           :: QualifiedIdentifier
-- | A table alias is used in case of self joins
  , fromAlias      :: Maybe Alias
-- | Only used for Many to Many joins. Parent and Child joins use explicit joins.
  , implicitJoins  :: [QualifiedIdentifier]
  , where_         :: [LogicTree]
  , joinConditions :: [JoinCondition]
  , order          :: [OrderTerm]
  , range_         :: NonnegRange
} deriving (Eq)

data MutateQuery =
  Insert {
    in_        :: QualifiedIdentifier
  , insCols    :: S.Set FieldName
  , insBody    :: Maybe BL.ByteString
  , onConflict :: Maybe (PreferResolution, [FieldName])
  , where_     :: [LogicTree]
  , returning  :: [FieldName]
  }|
  Update {
    in_       :: QualifiedIdentifier
  , updCols   :: S.Set FieldName
  , updBody   :: Maybe BL.ByteString
  , where_    :: [LogicTree]
  , returning :: [FieldName]
  }|
  Delete {
    in_       :: QualifiedIdentifier
  , where_    :: [LogicTree]
  , returning :: [FieldName]
  }

type ReadRequest = Tree ReadNode
type MutateRequest = MutateQuery

type ReadNode = (ReadQuery, (NodeName, Maybe Relation, Maybe Alias, Maybe EmbedHint, Depth))
type Depth = Integer

-- First level FieldNames(e.g get a,b from /table?select=a,b,other(c,d))
fstFieldNames :: ReadRequest -> [FieldName]
fstFieldNames (Node (sel, _) _) =
  fst . view _1 <$> select sel

data PgVersion = PgVersion {
  pgvNum  :: Int32
, pgvName :: Text
} deriving (Eq, Generic, JSON.ToJSON)

instance Ord PgVersion where
  (PgVersion v1 _) `compare` (PgVersion v2 _) = v1 `compare` v2

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: PgVersion
minimumPgVersion = pgVersion95

pgVersion95 :: PgVersion
pgVersion95 = PgVersion 90500 "9.5"

pgVersion96 :: PgVersion
pgVersion96 = PgVersion 90600 "9.6"

pgVersion100 :: PgVersion
pgVersion100 = PgVersion 100000 "10"

pgVersion109 :: PgVersion
pgVersion109 = PgVersion 100009 "10.9"

pgVersion110 :: PgVersion
pgVersion110 = PgVersion 110000 "11.0"

pgVersion112 :: PgVersion
pgVersion112 = PgVersion 110002 "11.2"

pgVersion114 :: PgVersion
pgVersion114 = PgVersion 110004 "11.4"

pgVersion121 :: PgVersion
pgVersion121 = PgVersion 120001 "12.1"

pgVersion130 :: PgVersion
pgVersion130 = PgVersion 130000 "13.0"

sourceCTEName :: SqlFragment
sourceCTEName = "pgrst_source"

-- | full jspath, e.g. .property[0].attr.detail
type JSPath = [JSPathExp]
-- | jspath expression, e.g. .property, .property[0] or ."property-dash"
data JSPathExp = JSPKey Text | JSPIdx Int

instance Show JSPathExp where
  -- TODO: this needs to be quoted properly for special chars
  show (JSPKey k) = "." <> show k
  show (JSPIdx i) = "[" <> show i <> "]"

-- | Schema cache status
data SCacheStatus
  = SCLoaded
  | SCOnRetry
  | SCFatalFail

data LogLevel = LogCrit | LogError | LogWarn | LogInfo

instance Show LogLevel where
  show LogCrit  = "crit"
  show LogError = "error"
  show LogWarn  = "warn"
  show LogInfo  = "info"
