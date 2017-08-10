{-# LANGUAGE DuplicateRecordFields    #-}
module PostgREST.Types where
import           Protolude
import qualified GHC.Show
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as M
import           Data.Tree
import qualified Data.Vector          as V
import           PostgREST.RangeQuery (NonnegRange)
import           Network.HTTP.Types.Header (hContentType, Header)

-- | Enumeration of currently supported response content types
data ContentType = CTApplicationJSON | CTTextCSV | CTOpenAPI
                 | CTSingularJSON | CTOctetStream
                 | CTAny | CTOther ByteString deriving Eq

data ApiRequestError = ActionInappropriate
                     | InvalidBody ByteString
                     | InvalidRange
                     | ParseRequestError Text Text
                     | UnknownRelation
                     | NoRelationBetween Text Text
                     | UnsupportedVerb
                     deriving (Show, Eq)

data DbStructure = DbStructure {
  dbTables      :: [Table]
, dbColumns     :: [Column]
, dbRelations   :: [Relation]
, dbPrimaryKeys :: [PrimaryKey]
, dbProcs       :: M.HashMap Text ProcDescription
} deriving (Show, Eq)

data PgArg = PgArg {
  pgaName :: Text
, pgaType :: Text
, pgaReq  :: Bool
} deriving (Show, Eq)

data PgType = Scalar QualifiedIdentifier | Composite QualifiedIdentifier deriving (Eq, Show)

data RetType = Single PgType | SetOf PgType deriving (Eq, Show)

data ProcVolatility = Volatile | Stable | Immutable
  deriving (Eq, Show)

data ProcDescription = ProcDescription {
  pdName        :: Text
, pdDescription :: Maybe Text
, pdArgs        :: [PgArg]
, pdReturnType  :: RetType
, pdVolatility  :: ProcVolatility
} deriving (Show, Eq)

type Schema = Text
type TableName = Text
type SqlQuery = Text
type SqlFragment = Text
type RequestBody = BL.ByteString

data Table = Table {
  tableSchema      :: Schema
, tableName        :: TableName
, tableDescription :: Maybe Text
, tableInsertable  :: Bool
} deriving (Show, Ord)

newtype ForeignKey = ForeignKey { fkCol :: Column } deriving (Show, Eq, Ord)

data Column =
    Column {
      colTable       :: Table
    , colName        :: Text
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

type Synonym = (Column,Column)

data PrimaryKey = PrimaryKey {
    pkTable :: Table
  , pkName  :: Text
} deriving (Show, Eq)

data OrderDirection = OrderAsc | OrderDesc deriving (Eq)
instance Show OrderDirection where
  show OrderAsc  = "asc"
  show OrderDesc = "desc"

data OrderNulls = OrderNullsFirst | OrderNullsLast deriving (Eq)
instance Show OrderNulls where
  show OrderNullsFirst = "nulls first"
  show OrderNullsLast  = "nulls last"

data OrderTerm = OrderTerm {
  otTerm      :: Field
, otDirection :: Maybe OrderDirection
, otNullOrder :: Maybe OrderNulls
} deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier {
  qiSchema :: Schema
, qiName   :: TableName
} deriving (Show, Eq)


data RelationType = Child | Parent | Many | Root deriving (Show, Eq)

{-|
  The name 'Relation' here is used with the meaning
  "What is the relation between the current node and the parent node".
  It has nothing to do with PostgreSQL referring to tables/views as relations.
-}
data Relation = Relation {
  relTable    :: Table
, relColumns  :: [Column]
, relFTable   :: Table
, relFColumns :: [Column]
, relType     :: RelationType
, relLTable   :: Maybe Table
, relLCols1   :: Maybe [Column]
, relLCols2   :: Maybe [Column]
} deriving (Show, Eq)

-- | An array of JSON objects that has been verified to have
-- the same keys in every object
newtype PayloadJSON = PayloadJSON (V.Vector Object)
  deriving (Show, Eq)

unPayloadJSON :: PayloadJSON -> V.Vector Object
unPayloadJSON (PayloadJSON objs) = objs

data Proxy = Proxy {
  proxyScheme     :: Text
, proxyHost       :: Text
, proxyPort       :: Integer
, proxyPath       :: Text
} deriving (Show, Eq)

type Operator = Text
operators :: M.HashMap Operator SqlFragment
operators = M.fromList [
  ("eq", "="),
  ("gte", ">="),
  ("gt", ">"),
  ("lte", "<="),
  ("lt", "<"),
  ("neq", "<>"),
  ("like", "LIKE"),
  ("ilike", "ILIKE"),
  ("in", "IN"),
  ("notin", "NOT IN"),
  ("isnot", "IS NOT"),
  ("is", "IS"),
  ("@@", "@@"),
  ("@>", "@>"),
  ("<@", "<@"),
  ("cs", "@>"),
  ("cd", "<@"),
  ("ov", "&&"),
  ("sl", "<<"),
  ("sr", ">>"),
  ("nxr", "&<"),
  ("nxl", "&>"),
  ("adj", "-|-")]
data Operation = Operation{ hasNot::Bool, expr::(Operator, Operand) } deriving (Eq, Show)
data Operand = VText Text | VTextL [Text] | VForeignKey QualifiedIdentifier ForeignKey deriving (Show, Eq)

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
type JsonPath = [Text]
type Field = (FieldName, Maybe JsonPath)
type Alias = Text
type Cast = Text
type NodeName = Text

{-|
  This type will hold information about which particular 'Relation' between two tables to choose when there are multiple ones.
  Specifically, it will contain the name of the foreign key or the join table in many to many relations.
-}
type RelationDetail = Text
type SelectItem = (Field, Maybe Cast, Maybe Alias, Maybe RelationDetail)
-- | Path of the embedded levels, e.g "clients.projects.name=eq.." gives Path ["clients", "projects"]
type EmbedPath = [Text]
data Filter = Filter { field::Field, operation::Operation } deriving (Show, Eq)

data ReadQuery = Select { select::[SelectItem], from::[TableName], where_::[LogicTree], order::Maybe [OrderTerm], range_::NonnegRange } deriving (Show, Eq)
data MutateQuery = Insert { in_::TableName, qPayload::PayloadJSON, returning::[FieldName] }
                 | Delete { in_::TableName, where_::[LogicTree], returning::[FieldName] }
                 | Update { in_::TableName, qPayload::PayloadJSON, where_::[LogicTree], returning::[FieldName] } deriving (Show, Eq)
type ReadNode = (ReadQuery, (NodeName, Maybe Relation, Maybe Alias, Maybe RelationDetail))
type ReadRequest = Tree ReadNode
type MutateRequest = MutateQuery
data DbRequest = DbRead ReadRequest | DbMutate MutateRequest

instance Eq Table where
  Table{tableSchema=s1,tableName=n1} == Table{tableSchema=s2,tableName=n2} = s1 == s2 && n1 == n2

instance Eq Column where
  Column{colTable=t1,colName=n1} == Column{colTable=t2,colName=n2} = t1 == t2 && n1 == n2

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
