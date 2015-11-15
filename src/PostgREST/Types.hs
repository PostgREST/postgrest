module PostgREST.Types where

import           Data.Text (Text)
import           Data.Tree
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson
import           Data.Map (Map)

data DbStructure = DbStructure {
  dbTables      :: [Table]
, dbColumns     :: [Column]
, dbRelations   :: [Relation]
, dbPrimaryKeys :: [PrimaryKey]
} deriving (Show, Eq)

type Schema = Text

data Table = Table {
  tableSchema     :: Schema
, tableName       :: Text
, tableInsertable :: Bool
} deriving (Show)

data ForeignKey = ForeignKey { fkCol :: Column } deriving (Show, Eq)

data Column =
    Column {
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
    }
  | Star { colTable :: Table }
  deriving (Show)

type Synonym = (Column,Column)

data PrimaryKey = PrimaryKey {
  pkTable    :: Table
, pkName     :: Text
} deriving (Show, Eq)

data OrderTerm = OrderTerm {
  otTerm      :: Text
, otDirection :: BS.ByteString
, otNullOrder :: Maybe BS.ByteString
} deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier {
  qiSchema :: Schema
, qiName   :: Text
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

data Locus = Locus Table [Filter] deriving (Show, Eq)


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
