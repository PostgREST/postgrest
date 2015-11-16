module PostgREST.Types where
import Data.Text
import Data.Tree
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import Data.Aeson
import Data.Map

data DbStructure = DbStructure {
  dbTables :: [Table]
, dbColumns :: [Column]
, dbRelations :: [Relation]
, dbPrimaryKeys :: [PrimaryKey]
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

tableQi :: Table -> QualifiedIdentifier
tableQi t = QualifiedIdentifier (Just $ tableSchema t) (tableName t) Nothing

data ForeignKey = ForeignKey { fkCol :: Column } deriving (Show, Eq, Ord)

data Column = Column {
  colTable     :: Table
, colName      :: Text
, colPosition  :: Int
, colNullable  :: Bool
, colType      :: Text
, colUpdatable :: Bool
, colMaxLen    :: Maybe Int
, colPrecision :: Maybe Int
, colDefault   :: Maybe Text
, colEnum      :: [Text]
, colFK        :: Maybe ForeignKey
} | Star {
  colTable :: Table
} deriving (Show, Ord)

colQi :: Column -> QualifiedIdentifier
colQi c = QualifiedIdentifier (Just $ (tableSchema . colTable) c) ((tableName . colTable) c) (Just $ colName c)

type Synonym = (Column,Column)

data PrimaryKey = PrimaryKey {
    pkTable :: Table
  , pkName  :: Text
} deriving (Show, Eq)

data OrderTerm = OrderTerm {
  otTerm      :: Text
, otDirection :: BS.ByteString
, otNullOrder :: Maybe BS.ByteString
} deriving (Show, Eq)

data RelationType = Child | Parent | Many deriving (Show, Eq)
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

data QualifiedIdentifier = QualifiedIdentifier {
  qiSchema :: Maybe Schema
, qiTable  :: Text
, qiColumn :: Maybe Text
} | UnqualifiedIdentifier {
  qiName   :: Text
} deriving (Show, Eq, Ord)

type Path = [Text]
type JsonPath = [Text]
type Operator = Text
data FValue = VText Text | VForeignKey QualifiedIdentifier deriving (Show, Eq)
type Field = (QualifiedIdentifier, Maybe JsonPath)
type Cast = Text
type NodeName = Text
type SelectItem = (Field, Maybe Cast)

data Query = Select {
  select :: [SelectItem]
, from   :: [QualifiedIdentifier]
, where_ :: [Filter]
, order  :: Maybe [OrderTerm]
} | Insert {
  into   :: QualifiedIdentifier
, fields :: [Field]
, values :: [[Value]]
} | Delete {
  from   :: [QualifiedIdentifier]
, where_ :: [Filter]
} | Update {
  into   :: QualifiedIdentifier
, set    :: Map Field Value
, where_ :: [Filter]
} deriving (Show, Eq)

data Filter = Filter {
  field    :: Field
, operator :: Operator
, value    :: FValue
} deriving (Show, Eq)

type ApiNode = (Query, (NodeName, Maybe Relation))
type ApiRequest = Tree ApiNode


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
  t1 == t2 = tableQi t1 == tableQi t2

instance Eq Column where
  c1 == c2 = colQi c1 == colQi c2
