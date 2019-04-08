{-|
Module      : PostgREST.Types
Description : PostgREST common types and functions used by the rest of the modules
-}
{-# LANGUAGE DuplicateRecordFields    #-}
module PostgREST.Types where
import           Protolude
import qualified GHC.Show
import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as M
import qualified Data.Set                  as S
import           Data.Tree
import           PostgREST.RangeQuery (NonnegRange)
import           Network.HTTP.Types.Header (hContentType, Header)

-- | Enumeration of currently supported response content types
data ContentType = CTApplicationJSON | CTTextCSV | CTOpenAPI
                 | CTSingularJSON | CTOctetStream
                 | CTAny | CTOther ByteString deriving Eq

-- | Convert from ContentType to a full HTTP Header
toHeader :: ContentType -> Header
toHeader ct = (hContentType, toMime ct <> "; charset=utf-8")

-- | Convert from ContentType to a ByteString representing the mime type
toMime :: ContentType -> ByteString
toMime CTApplicationJSON = "application/json"
toMime CTTextCSV         = "text/csv"
toMime CTOpenAPI         = "application/openapi+json"
toMime CTSingularJSON    = "application/vnd.pgrst.object+json"
toMime CTOctetStream     = "application/octet-stream"
toMime CTAny             = "*/*"
toMime (CTOther ct)      = ct

data ApiRequestError = ActionInappropriate
                     | InvalidBody ByteString
                     | InvalidRange
                     | ParseRequestError Text Text
                     | UnknownRelation
                     | NoRelationBetween Text Text
                     | UnsupportedVerb
                     | InvalidFilters
                     deriving (Show, Eq)

data PreferResolution = MergeDuplicates | IgnoreDuplicates deriving Eq
instance Show PreferResolution where
  show MergeDuplicates  = "resolution=merge-duplicates"
  show IgnoreDuplicates = "resolution=ignore-duplicates"

data DbStructure = DbStructure {
  dbTables      :: [Table]
, dbColumns     :: [Column]
, dbRelations   :: [Relation]
, dbPrimaryKeys :: [PrimaryKey]
-- ProcDescription is a list because a function can be overloaded
, dbProcs       :: M.HashMap Text [ProcDescription]
, pgVersion     :: PgVersion
} deriving (Show, Eq)

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
} deriving (Show, Eq, Ord)

data PgType = Scalar QualifiedIdentifier | Composite QualifiedIdentifier deriving (Eq, Show, Ord)

data RetType = Single PgType | SetOf PgType deriving (Eq, Show, Ord)

data ProcVolatility = Volatile | Stable | Immutable
  deriving (Eq, Show, Ord)

data ProcDescription = ProcDescription {
  pdName        :: Text
, pdDescription :: Maybe Text
, pdArgs        :: [PgArg]
, pdReturnType  :: RetType
, pdVolatility  :: ProcVolatility
} deriving (Show, Eq)

-- Order by least number of args in the case of overloaded functions
instance Ord ProcDescription where
  ProcDescription name1 des1 args1 rt1 vol1 `compare` ProcDescription name2 des2 args2 rt2 vol2
    | name1 == name2 && length args1 < length args2  = LT
    | name1 == name2 && length args1 > length args2  = GT
    | otherwise = (name1, des1, args1, rt1, vol1) `compare` (name2, des2, args2, rt2, vol2)

{-|
  Search a pg procedure by its parameters, since a function can be overloaded the name is not enough to find it.
  An overloaded function can have a different volatility or even a different return type.
-}
findProc :: QualifiedIdentifier -> S.Set Text -> Bool -> M.HashMap Text [ProcDescription] -> Maybe ProcDescription
findProc qi payloadKeys paramsAsSingleObject allProcs =
  let procs = M.lookup (qiName qi) allProcs in
 -- Handle overloaded functions case
  join $ (case length <$> procs of
    Just 1 -> headMay -- if it's not an overloaded function then immediatly get the ProcDescription
    _ -> find (\x ->
           if paramsAsSingleObject
             then length (pdArgs x) == 1 -- if the arg is not of json type let the db give the err
             else payloadKeys `S.isSubsetOf` S.fromList (pgaName <$> pdArgs x))
  ) <$> procs

{-|
  Search the procedure parameters by matching them with the specified keys.
  If the key doesn't match a parameter, a parameter with a default type "text" is assumed.
-}
specifiedProcArgs :: S.Set FieldName -> Maybe ProcDescription -> [PgArg]
specifiedProcArgs keys proc =
  let
    args = maybe [] pdArgs proc
  in
  (\k -> fromMaybe (PgArg k "text" True) (find ((==) k . pgaName) args)) <$> S.toList keys

type Schema = Text
type TableName = Text
type SqlQuery = Text
type SqlFragment = Text

data Table = Table {
  tableSchema      :: Schema
, tableName        :: TableName
, tableDescription :: Maybe Text
, tableInsertable  :: Bool
} deriving (Show, Ord)

instance Eq Table where
  Table{tableSchema=s1,tableName=n1} == Table{tableSchema=s2,tableName=n2} = s1 == s2 && n1 == n2

newtype ForeignKey = ForeignKey { fkCol :: Column } deriving (Show, Eq, Ord)

data Column =
    Column {
      colTable       :: Table
    , colName        :: FieldName
    , colDescription :: Maybe Text
    , colPosition    :: Int32
    , colNullable    :: Bool
    , colType        :: Text
    , colUpdatable   :: Bool
    , colMaxLen      :: Maybe Int32
    , colPrecision   :: Maybe Int32
    , colDefault     :: Maybe Text
    , colEnum        :: [Text]
    , colFK          :: Maybe ForeignKey
    } deriving (Show, Ord)

instance Eq Column where
  Column{colTable=t1,colName=n1} == Column{colTable=t2,colName=n2} = t1 == t2 && n1 == n2

-- | A view column that refers to a table column
type Synonym = (Column, ViewColumn)
type ViewColumn = Column

data PrimaryKey = PrimaryKey {
    pkTable :: Table
  , pkName  :: Text
} deriving (Show, Eq)

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
} deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier {
  qiSchema :: Schema
, qiName   :: TableName
} deriving (Show, Eq, Ord)


data RelationType = Child | Parent | Many | Root deriving (Show, Eq)

{-|
  The name 'Relation' here is used with the meaning
  "What is the relation between the current node and the parent node".
  It has nothing to do with PostgreSQL referring to tables/views as relations.
  The order of the relColumns and relFColumns should be maintained to get
  the join conditions right.
  TODO merge relColumns and relFColumns to a tuple or Data.Bimap
-}
data Relation = Relation {
  relTable     :: Table
, relColumns   :: [Column]
, relFTable    :: Table
, relFColumns  :: [Column]
, relType      :: RelationType
-- The Link attrs are used when RelationType == Many
, relLinkTable :: Maybe Table
, relLinkCols1 :: Maybe [Column]
, relLinkCols2 :: Maybe [Column]
} deriving (Show, Eq)

isSelfJoin :: Relation -> Bool
isSelfJoin r = relType r /= Root && relTable r == relFTable r

data PayloadJSON =
  -- | Cached attributes of a JSON payload
  ProcessedJSON {
    -- | This is the raw ByteString that comes from the request body.
    -- We cache this instead of an Aeson Value because it was detected that for large payloads the encoding
    -- had high memory usage, see #1005 for more details
    pjRaw  :: BL.ByteString
  , pjType :: PJType
    -- | Keys of the object or if it's an array these keys are guaranteed to be the same across all its objects
  , pjKeys :: S.Set Text
  }|
  RawJSON {
    pjRaw  :: BL.ByteString
  } deriving (Show, Eq)

data PJType = PJArray { pjaLength :: Int } | PJObject deriving (Show, Eq)

data Proxy = Proxy {
  proxyScheme     :: Text
, proxyHost       :: Text
, proxyPort       :: Integer
, proxyPath       :: Text
} deriving (Show, Eq)

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
  ("phfts", "@@ phraseto_tsquery")
  ]

data OpExpr = OpExpr Bool Operation deriving (Eq, Show)
data Operation = Op Operator SingleVal |
                 In ListVal |
                 Fts Operator (Maybe Language) SingleVal deriving (Eq, Show)
type Language = Text

-- | Represents a single value in a filter, e.g. id=eq.singleval
type SingleVal = Text
-- | Represents a list value in a filter, e.g. id=in.(val1,val2,val3)
type ListVal = [Text]

data LogicOperator = And | Or deriving Eq
instance Show LogicOperator where
  show And  = "AND"
  show Or = "OR"
{-|
  Boolean logic expression tree e.g. "and(name.eq.N,or(id.eq.1,id.eq.2))" is:

            And
           /   \
  name.eq.N     Or
               /  \
         id.eq.1   id.eq.2
-}
data LogicTree = Expr Bool LogicOperator [LogicTree] | Stmnt Filter deriving (Show, Eq)

type FieldName = Text
{-|
  Json path operations as specified in https://www.postgresql.org/docs/9.4/static/functions-json.html
-}
type JsonPath = [JsonOperation]
-- | Represents the single arrow `->` or double arrow `->>` operators
data JsonOperation = JArrow{jOp :: JsonOperand} | J2Arrow{jOp :: JsonOperand} deriving (Show, Eq)
-- | Represents the key(`->'key'`) or index(`->'1`::int`), the index is Text because we reuse our escaping functons and let pg do the casting with '1'::int
data JsonOperand = JKey{jVal :: Text} | JIdx{jVal :: Text} deriving (Show, Eq)

type Field = (FieldName, JsonPath)
type Alias = Text
type Cast = Text
type NodeName = Text

-- Rpc query param, only used for GET rpcs
type RpcQParam = (Text, Text)

{-|
  Custom guc header, it's obtained by parsing the json in a:
  `SET LOCAL "response.headers" = '[{"Set-Cookie": ".."}]'
-}
newtype GucHeader = GucHeader (Text, Text)

instance JSON.FromJSON GucHeader where
  parseJSON (JSON.Object o) = case headMay (M.toList o) of
    Just (k, JSON.String s) | M.size o == 1 -> pure $ GucHeader (k, s)
                            | otherwise     -> mzero
    _ -> mzero
  parseJSON _          = mzero

toHeaders :: [GucHeader] -> [Header]
toHeaders = map $ \(GucHeader (k, v)) -> (CI.mk $ toS k, toS v)

{-|
  This type will hold information about which particular 'Relation' between two tables to choose when there are multiple ones.
  Specifically, it will contain the name of the foreign key or the join table in many to many relations.
-}
type RelationDetail = Text
type SelectItem = (Field, Maybe Cast, Maybe Alias, Maybe RelationDetail)
-- | Path of the embedded levels, e.g "clients.projects.name=eq.." gives Path ["clients", "projects"]
type EmbedPath = [Text]
data Filter = Filter { field::Field, opExpr::OpExpr } deriving (Show, Eq)
data JoinCondition = JoinCondition (QualifiedIdentifier, FieldName)
                                   (QualifiedIdentifier, FieldName) deriving (Show, Eq)

data ReadQuery = Select {
    select         :: [SelectItem]
  , from           :: TableName
-- | A table alias is used in case of self joins
  , fromAlias      :: Maybe Alias
-- | Only used for Many to Many joins. Parent and Child joins use explicit joins.
  , implicitJoins  :: [TableName]
  , where_         :: [LogicTree]
  , joinConditions :: [JoinCondition]
  , order          :: [OrderTerm]
  , range_         :: NonnegRange
} deriving (Show, Eq)

data MutateQuery =
  Insert {
    in_        :: TableName
  , insCols    :: S.Set FieldName
  , onConflict :: Maybe (PreferResolution, [FieldName])
  , where_     :: [LogicTree]
  , returning  :: [FieldName]
  }|
  Update {
    in_        :: TableName
  , updCols    :: S.Set FieldName
  , where_     :: [LogicTree]
  , returning  :: [FieldName]
  }|
  Delete {
    in_        :: TableName
  , where_     :: [LogicTree]
  , returning  :: [FieldName]
  } deriving (Show, Eq)

data DbRequest = DbRead ReadRequest | DbMutate MutateRequest
type ReadRequest = Tree ReadNode
type ReadNode = (ReadQuery, (NodeName, Maybe Relation, Maybe Alias, Maybe RelationDetail, Depth))
-- Depth of the ReadRequest tree
type Depth = Integer
type MutateRequest = MutateQuery

data PgVersion = PgVersion {
  pgvNum  :: Int32
, pgvName :: Text
} deriving (Eq, Show)

instance Ord PgVersion where
  (PgVersion v1 _) `compare` (PgVersion v2 _) = v1 `compare` v2

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: PgVersion
minimumPgVersion = pgVersion94

pgVersion94 :: PgVersion
pgVersion94 = PgVersion 90400 "9.4"

pgVersion95 :: PgVersion
pgVersion95 = PgVersion 90500 "9.5"

pgVersion96 :: PgVersion
pgVersion96 = PgVersion 90600 "9.6"

pgVersion100 :: PgVersion
pgVersion100 = PgVersion 100000 "10"

pgVersion112 :: PgVersion
pgVersion112 = PgVersion 110002 "11.2"

sourceCTEName :: SqlFragment
sourceCTEName = "pg_source"

-- | full jspath, e.g. .property[0].attr.detail
type JSPath = [JSPathExp]
-- | jspath expression, e.g. .property, .property[0] or ."property-dash"
data JSPathExp = JSPKey Text | JSPIdx Int deriving (Eq, Show)
