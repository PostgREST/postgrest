module PostgREST.Types where
import Data.Text

data DbStructure = DbStructure {
  tables :: [Table]
, columns :: [Column]
, relations :: [Relation]
, primaryKeys :: [PrimaryKey]
, tablesAcl :: [(Text, Text, Text)]
}

data Table = Table {
  tableSchema :: Text
, tableName :: Text
, tableInsertable :: Bool
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
} deriving (Show)

data PrimaryKey = PrimaryKey {
  pkSchema::Text, pkTable::Text, pkName::Text
}

data Relation = Relation {
  relSchema  :: Text
, relTable   :: Text
, relColumn  :: Text
, relFTable  :: Text
, relFColumn :: Text
, relType    :: Text
} deriving (Show, Eq)
