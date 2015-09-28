module PostgREST.Types where
import Data.Text
import Data.Tree
import qualified Data.ByteString.Char8 as BS
import Data.Aeson

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
} deriving (Show)


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
, relLTable  :: Maybe Text
, relLCol1   :: Maybe Text
, relLCol2   :: Maybe Text
} deriving (Show, Eq)


--------
-- Request Types
type Operator = Text
data FValue = VText Text | VForeignKey Relation deriving (Show, Eq)
type FieldName = Text
type JsonPath = [Text]
type Field = (FieldName, Maybe JsonPath)
type Cast = Text
type SelectItem = (Field, Maybe Cast)
type Path = [Text]
data Query = Select {
  mainTable::Text
, fields::[SelectItem]
, joinTables::[Text]
, filters::[Filter]
, order::Maybe [OrderTerm]
, relation::Maybe Relation
} deriving (Show, Eq)
data Filter = Filter {field::Field, operator::Operator, value::FValue} deriving (Show, Eq)
type ApiRequest = Tree Query


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
