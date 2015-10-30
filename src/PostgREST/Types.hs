module PostgREST.Types where
import Data.Text
import Data.Tree
import qualified Data.ByteString.Char8 as BS
import Data.Aeson
import Data.Map

data DbStructure = DbStructure {
  tables :: [Table]
, columns :: [Column]
, relations :: [Relation]
, primaryKeys :: [PrimaryKey]
}


data Table = Table {
  tableSchema :: Text
, tableName :: Text
, tableInsertable :: Bool
, tableAcl :: [Text]
} deriving (Show)

data ForeignKey = ForeignKey {
  fkTable::Text, fkCol::Text
} deriving (Show, Eq)


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

data QualifiedIdentifier = QualifiedIdentifier {
  qiSchema :: Text
, qiName   :: Text
} deriving (Show, Eq)


data RelationType = Child | Parent | Many deriving (Show, Eq)
data Relation = Relation {
  relSchema  :: Text
, relTable   :: Text
, relColumns  :: [Text]
, relFTable  :: Text
, relFColumns :: [Text]
, relType    :: RelationType
, relLTable  :: Maybe Text
, relLCols1   :: Maybe [Text]
, relLCols2   :: Maybe [Text]
} deriving (Show, Eq)


type Operator = Text
data FValue = VText Text | VForeignKey QualifiedIdentifier ForeignKey deriving (Show, Eq)
type FieldName = Text
type JsonPath = [Text]
type Field = (FieldName, Maybe JsonPath)
type Cast = Text
type NodeName = Text
type SelectItem = (Field, Maybe Cast)
type Path = [Text]
data Query = Select { select::[SelectItem], from::[Text], where_::[Filter], order::Maybe [OrderTerm] }
           | Insert { into::Text, fields::[Field], values::[[Value]] }
           | Delete { from::[Text], where_::[Filter] }
           | Update { into::Text, set::Map Field Value, where_::[Filter] } deriving (Show, Eq)
data Filter = Filter {field::Field, operator::Operator, value::FValue} deriving (Show, Eq)
type ApiNode = (Query, (NodeName, Maybe Relation))
type ApiRequest = Tree ApiNode


instance ToJSON Column where
  toJSON c = object [
      "schema"    .= colSchema c
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

instance ToJSON ForeignKey where
  toJSON fk = object ["table".=fkTable fk, "column".=fkCol fk]

instance ToJSON Table where
  toJSON v = object [
      "schema"     .= tableSchema v
    , "name"       .= tableName v
    , "insertable" .= tableInsertable v ]
