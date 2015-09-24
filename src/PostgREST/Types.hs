module PostgREST.Types where
import Data.Text
import Data.Tree
import qualified Data.ByteString.Char8 as BS

data DbStructure = DbStructure {
  tables :: [Table]
, columns :: [Column]
, relations :: [Relation]
, primaryKeys :: [PrimaryKey]
--, tablesAcl :: [(Text, Text, Text)]
}

data Table = Table {
  tableSchema :: Text
, tableName :: Text
, tableInsertable :: Bool
, tableAcl :: [Text]
} deriving (Show)

data ForeignKey = ForeignKey {
  fkTable::Text, fkCol::Text
} deriving (Eq, Show)


data Column = Column {
  colSchema :: Text
, colTable :: Text
, colName :: Text
, colPosition :: Int
, colNullable :: Bool
, colType :: Text
, colUpdatable :: Bool
, colMaxLen :: Maybe Int
, colPrecision :: Maybe Int
, colDefault :: Maybe Text
, colEnum :: [Text]
, colFK :: Maybe ForeignKey
} | Star {colSchema :: Text, colTable :: Text } deriving (Show)

data PrimaryKey = PrimaryKey {
  pkSchema::Text, pkTable::Text, pkName::Text
}

data OrderTerm = OrderTerm {
  otTerm :: Text
, otDirection :: BS.ByteString
, otNullOrder :: Maybe BS.ByteString
} deriving (Show, Eq)


data Relation = Relation {
  relSchema  :: Text
, relTable   :: Text
, relColumn  :: Text
, relFTable  :: Text
, relFColumn :: Text
, relType    :: Text
} deriving (Show, Eq)


--------
-- Request Types
type Operator = String
type FValue = String
type ApiRequest = Tree RequestNode
type FieldName = String
type JsonPath = [String]
type Field = (FieldName, Maybe JsonPath)
type Cast = String
type SelectItem = (Field, Maybe Cast)
type Path = [String]
data RequestNode = RequestNode {
  nodeName::String
, fields::[SelectItem]
, filters::[Filter]
, order::Maybe [OrderTerm]
} deriving (Show, Eq)
data Filter = Filter {field::Field, operator::Operator, value::FValue} deriving (Show, Eq)

-- Db Request Types
type DbField = (Column, Maybe JsonPath)
type DbSelectItem = (DbField, Maybe Cast)
data DbValue = VText Text | VForeignKey Relation deriving (Show)
data Condition = Condition {conColumn::DbField, conOperator::Operator, conValue::DbValue} deriving (Show)
data Query = Select {
  qMainTable::Table
, qSelect::[DbSelectItem]
, qJoinTables::[Table]
, qWhere::[Condition]
, qRelation::Maybe Relation
, qOrder::Maybe [OrderTerm]
} deriving (Show)
type DbRequest = Tree Query
