module PostgREST.Types where
import           Protolude
import qualified GHC.Show
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Tree
import qualified Data.Vector          as V
import           PostgREST.RangeQuery (NonnegRange)

data DbStructure = DbStructure {
  dbTables      :: [Table]
, dbColumns     :: [Column]
, dbRelations   :: [Relation]
, dbPrimaryKeys :: [PrimaryKey]
, dbProcs       :: [(Text,ProcDescription)]
} deriving (Show, Eq)

data PgArg = PgArg {
  pgaName :: Text
, pgaType :: Text
, pgaReq  :: Bool
} deriving (Show, Eq)

data ProcDescription = ProcDescription {
  pdName       :: Text
, pdArgs       :: [PgArg]
, pdReturnType :: Text
} deriving (Show, Eq)

type Schema = Text
type TableName = Text
type SqlQuery = Text
type SqlFragment = Text
type RequestBody = BL.ByteString

data Table = Table {
  tableSchema     :: Schema
, tableName       :: TableName
, tableInsertable :: Bool
} deriving (Show, Ord)

data ForeignKey = ForeignKey { fkCol :: Column } deriving (Show, Eq, Ord)

data Column =
    Column {
      colTable     :: Table
    , colName      :: Text
    , colPosition  :: Int32
    , colNullable  :: Bool
    , colType      :: Text
    , colUpdatable :: Bool
    , colMaxLen    :: Maybe Int32
    , colPrecision :: Maybe Int32
    , colDefault   :: Maybe Text
    , colEnum      :: [Text]
    , colFK        :: Maybe ForeignKey
    }
  | Star { colTable :: Table }
  deriving (Show, Ord)

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
newtype UniformObjects = UniformObjects (V.Vector Object)
  deriving (Show, Eq)

unUniformObjects :: UniformObjects -> V.Vector Object
unUniformObjects (UniformObjects objs) = objs

-- | When Hasql supports the COPY command then we can
-- have a special payload just for CSV, but until
-- then CSV is converted to a JSON array.
data Payload = PayloadJSON UniformObjects
             | PayloadParseError BS.ByteString
             deriving (Show, Eq)

data Proxy = Proxy {
  proxyScheme     :: Text
, proxyHost       :: Text
, proxyPort       :: Integer
, proxyPath       :: Text
} deriving (Show, Eq)

type Operator = Text
data FValue = VText Text | VForeignKey QualifiedIdentifier ForeignKey deriving (Show, Eq)
type FieldName = Text
type JsonPath = [Text]
type Field = (FieldName, Maybe JsonPath)
type Alias = Text
type Cast = Text
type NodeName = Text
type SelectItem = (Field, Maybe Cast, Maybe Alias)
type Path = [Text]
data ReadQuery = Select { select::[SelectItem], from::[TableName], flt_::[Filter], order::Maybe [OrderTerm], range_::NonnegRange } deriving (Show, Eq)
data MutateQuery = Insert { in_::TableName, qPayload::Payload }
                 | Delete { in_::TableName, where_::[Filter] }
                 | Update { in_::TableName, qPayload::Payload, where_::[Filter] } deriving (Show, Eq)
data Filter = Filter {field::Field, operator::Operator, value::FValue} deriving (Show, Eq)
type ReadNode = (ReadQuery, (NodeName, Maybe Relation, Maybe Alias))
type ReadRequest = Tree ReadNode
type MutateRequest = MutateQuery
data DbRequest = DbRead ReadRequest | DbMutate MutateRequest


instance ToJSON Column where
  toJSON c = object [
      "schema"    .= tableSchema t
    , "name"      .= colName c
    , "position"  .= colPosition c
    , "nullable"  .= colNullable c
    , "type"      .= colType c
    , "updatable" .= colUpdatable c
    , "maxLen"    .= colMaxLen c
    , "precision" .= colPrecision c
    , "references".= colFK c
    , "default"   .= colDefault c
    , "enum"      .= colEnum c ]
    where
      t = colTable c

instance ToJSON ForeignKey where
  toJSON fk = object [
      "schema" .= tableSchema t
    , "table"  .= tableName t
    , "column" .= colName c ]
    where
      c = fkCol fk
      t = colTable c

instance ToJSON Table where
  toJSON v = object [
      "schema"     .= tableSchema v
    , "name"       .= tableName v
    , "insertable" .= tableInsertable v ]

instance Eq Table where
  Table{tableSchema=s1,tableName=n1} == Table{tableSchema=s2,tableName=n2} = s1 == s2 && n1 == n2

instance Eq Column where
  Column{colTable=t1,colName=n1} == Column{colTable=t2,colName=n2} = t1 == t2 && n1 == n2
  _ == _ = False
