{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
module PostgREST.MegaQuery (getDbStructure) where

import           Control.Exception
import qualified Data.Aeson                    as Aeson
import           Data.Aeson (FromJSON)
import qualified Data.FileEmbed                as FileEmbed
import qualified Data.HashMap.Strict           as M
import qualified Data.List                     as L
import           Data.String
import qualified Hasql.Decoders                as HD
import qualified Hasql.Encoders                as HE
import qualified Hasql.Statement               as H
import qualified Hasql.Transaction             as HT
import qualified PostgREST.Private.Common as Common
import           PostgREST.Types hiding (Table(..), Column(..), Relation(..), Junction(..))

import qualified PostgREST.Types as Types
import Protolude


getDbStructure :: [Types.Schema] -> Types.DbStructure -> HT.Transaction Types.DbStructure
getDbStructure schemas baseDbStructure =
  do
    -- This voids the search path. The following queries need this for getting the
    -- fully qualified name(schema.name) of every db object.
    HT.sql "set local schema ''"
    raw <- getRawDbStructure schemas
    let
      newDbStructure = parseDbStructure raw

    return Types.DbStructure
      { Types.pgVersion = Types.pgVersion newDbStructure
      , Types.dbTables = Types.dbTables newDbStructure
      , Types.dbProcs = Types.dbProcs newDbStructure
      , Types.dbColumns = Types.dbColumns baseDbStructure
      , Types.dbRelations = Types.dbRelations baseDbStructure
      , Types.dbPrimaryKeys = Types.dbPrimaryKeys baseDbStructure
      }


parseDbStructure :: RawDbStructure -> DbStructure
parseDbStructure raw =
  let
    tabs = fmap loadTable $ rawDbTables raw

    tabsMap :: Tables
    tabsMap =
      M.fromList . map (\table -> (tableOid table, table)) $ rawDbTables raw

    cols = catMaybes . fmap (loadColumn tabsMap) $ rawDbColumns raw

    colsMap :: Columns
    colsMap =
      M.fromList . map (\col -> ((colTableOid col, colPosition col), col)) $ rawDbColumns raw

    rels = catMaybes . fmap (loadRelation tabsMap colsMap) $ rawDbRels raw

    procs = procsMap $ fmap loadProc $ rawDbProcs raw

    cols' = addForeignKeys rels cols
  in
  DbStructure
    { dbTables = tabs
    , dbColumns = cols'
    , dbRelations = rels
    , dbPrimaryKeys = []
    , dbProcs = procs
    , pgVersion = rawDbPgVer raw
    }

loadTable :: RawTable -> Types.Table
loadTable raw =
  Types.Table
    { Types.tableSchema = tableSchema raw
    , Types.tableName = tableName raw
    , Types.tableDescription = tableDescription raw
    , Types.tableInsertable = tableInsertable raw
    --, Types.tableIsAccessible = tableIsAccessible raw
    }

loadColumn :: Tables -> RawColumn -> Maybe Types.Column
loadColumn tabsMap col =
  loadColumn' col <$> M.lookup (colTableOid col) tabsMap

loadColumn' :: RawColumn -> RawTable -> Types.Column
loadColumn' col tab =
  Types.Column
    { Types.colTable = loadTable tab
    , Types.colPosition = colPosition col
    , Types.colName = colName col
    , Types.colDescription = colDescription col
    , Types.colNullable = colNullable col
    , Types.colType = colType col
    , Types.colUpdatable = colUpdatable col
    , Types.colMaxLen = colMaxLen col
    , Types.colPrecision = colPrecision col
    , Types.colDefault = colDefault col
    , Types.colEnum = colEnum col
    , Types.colFK = colFK col
    --, Types.colIsPrimaryKey = colIsPrimaryKey col
    }

type Tables = M.HashMap Oid RawTable

type Columns = M.HashMap (Oid, ColPosition) RawColumn

loadRelation :: Tables -> Columns -> RawRelation -> Maybe Types.Relation
loadRelation tabs cols raw =
  let
    junction =
      case relJunction raw of
        Just j ->
          loadJunction tabs cols j
        Nothing ->
          Nothing
  in
  loadRelation' raw cols junction
    <$> M.lookup (relTableOid raw) tabs
    <*> M.lookup (relFTableOid raw) tabs

loadRelation' :: RawRelation -> Columns -> Maybe Types.Junction -> RawTable -> RawTable -> Types.Relation
loadRelation' raw _ junc tab fTab =
  --let
  --  lookupCol :: RawTable -> ColPosition -> Maybe RawColumn
  --  lookupCol t pos =
  --    M.lookup (tableOid t, pos) cols
  --in
  Types.Relation
    { Types.relTable = loadTable tab
    , Types.relFTable = loadTable fTab
    , Types.relConstraint = relConstraint raw
    , Types.relType = relType raw
    , Types.relJunction = junc
    , Types.relColumns = []
        --fmap (\c -> loadColumn' c tab) . catMaybes .
        --  fmap ((lookupCol tab) . fromCol) $ relColMap raw
    , Types.relFColumns = []
        --fmap (\c -> loadColumn' c fTab) . catMaybes .
        --  fmap ((lookupCol fTab) . toCol) $ relColMap raw
    }

loadJunction :: Tables -> Columns -> RawJunction -> Maybe Types.Junction
loadJunction tabs cols raw =
  loadJunction' raw cols
    <$> M.lookup (junTableOid raw) tabs

loadJunction' :: RawJunction -> Columns -> RawTable -> Types.Junction
loadJunction' raw cols tab =
  let
    lookupCol :: RawTable -> ColPosition -> Maybe RawColumn
    lookupCol t pos =
        M.lookup (tableOid t, pos) cols
  in
  Types.Junction
    { Types.junTable = loadTable tab
    , Types.junConstraint1 = junConstraint1 raw
    , Types.junConstraint2 = junConstraint2 raw
    , Types.junCols1 =
        fmap (\c -> loadColumn' c tab) . catMaybes .
          fmap ((lookupCol tab) . fromCol) $ junColMap raw
    , Types.junCols2 =
        fmap (\c -> loadColumn' c tab) . catMaybes .
          fmap ((lookupCol tab) . toCol) $ junColMap raw
    }

procsMap :: [ProcDescription] -> ProcsMap
procsMap procs =
  M.fromListWith (++) . reverse . map (\(x,y) -> (x, [y])) . sort $ map addKey procs

loadProc :: RawProcDescription -> ProcDescription
loadProc raw =
  ProcDescription
    { pdSchema = procSchema raw
    , pdName = procName raw
    , pdDescription = procDescription raw
    , pdArgs = procArgs raw
    , pdReturnType =
        parseRetType
          (procReturnTypeQi raw)
          (procReturnTypeIsSetof raw)
          (procReturnTypeIsComposite raw)
    , pdVolatility = procVolatility raw
    --, pdIsAccessible = procIsAccessible raw
    }

addKey :: ProcDescription -> (QualifiedIdentifier, ProcDescription)
addKey pd = (QualifiedIdentifier (pdSchema pd) (pdName pd), pd)

parseRetType :: QualifiedIdentifier -> Bool -> Bool -> RetType
parseRetType qi isSetOf isComposite
  | isSetOf = SetOf pgType
  | otherwise = Single pgType
  where
    pgType = if isComposite then Composite qi else Scalar qi

addForeignKeys :: [Types.Relation] -> [Types.Column] -> [Types.Column]
addForeignKeys rels = map addFk
  where
    addFk col =
      col { Types.colFK = fk col }

    fk col =
      find (lookupFn col) rels >>= relToFk col

    lookupFn :: Types.Column -> Types.Relation -> Bool
    lookupFn c Types.Relation{Types.relColumns=cs, Types.relType=rty} =
      c `elem` cs && rty == M2O

    relToFk col Types.Relation{Types.relColumns=cols, Types.relFColumns=colsF} =
      do
        pos <- L.elemIndex col cols
        colF <- atMay colsF pos
        return $ ForeignKey colF


-- RAW DB STRUCTURE

getRawDbStructure :: [Schema] -> HT.Transaction RawDbStructure
getRawDbStructure schemas =
  do
    value <- HT.statement schemas rawDbStructureQuery

    case Aeson.fromJSON value of
      Aeson.Success m ->
        return m
      Aeson.Error err ->
        throw $ DbStructureDecodeException err

data DbStructureDecodeException =
    DbStructureDecodeException String
    deriving Show

instance Exception DbStructureDecodeException

rawDbStructureQuery :: H.Statement [Schema] Aeson.Value
rawDbStructureQuery =
  let
    sql =
      $(FileEmbed.embedFile "src/PostgREST/dbstructure.sql")

    decode =
      HD.singleRow $ Common.column HD.json
  in
  H.Statement sql (Common.arrayParam HE.text) decode True


-- Types

type Oid = String

type ColPosition = Int32

data RawDbStructure =
  RawDbStructure
    { rawDbPgVer  :: PgVersion
    , rawDbTables :: [RawTable]
    , rawDbColumns :: [RawColumn]
    , rawDbProcs :: [RawProcDescription]
    , rawDbRels :: [RawRelation]
    --, rawDbSchemas :: [SchemaDescription]
    }
    deriving (Show, Eq, Generic)

instance FromJSON RawDbStructure where
  parseJSON =
    Aeson.genericParseJSON aesonOptions

data RawProcDescription =
  RawProcDescription
    { procSchema :: Schema
    , procName :: Text
    , procDescription :: Maybe Text
    , procReturnTypeQi :: QualifiedIdentifier
    , procReturnTypeIsSetof :: Bool
    , procReturnTypeIsComposite :: Bool
    , procVolatility :: ProcVolatility
    , procIsAccessible :: Bool
    , procArgs :: [PgArg]
    } deriving (Show, Eq, Generic)

instance FromJSON RawProcDescription where
  parseJSON =
    Aeson.genericParseJSON aesonOptions

data RawTable =
  RawTable
    { tableOid :: Oid
    , tableSchema :: Schema
    , tableName :: TableName
    , tableDescription :: Maybe Text
    , tableInsertable :: Bool
    , tableIsAccessible :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON RawTable where
  parseJSON =
    Aeson.genericParseJSON aesonOptions

data RawColumn =
  RawColumn
    { colTableOid :: Oid
    , colPosition :: ColPosition
    , colName :: FieldName
    , colDescription :: Maybe Text
    , colNullable :: Bool
    , colType :: Text
    , colUpdatable :: Bool
    , colMaxLen :: Maybe Int32
    , colPrecision :: Maybe Int32
    , colDefault :: Maybe Text
    , colEnum :: [Text]
    , colFK :: Maybe ForeignKey
    , colIsPrimaryKey :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON RawColumn where
  parseJSON =
    Aeson.genericParseJSON aesonOptions

data RawRelation =
  RawRelation
    { relTableOid :: Oid
    , relFTableOid :: Oid
    , relConstraint :: Maybe ConstraintName
    --, relColMap :: [ColMapping]
    , relType :: Cardinality
    , relJunction :: Maybe RawJunction -- ^ Junction for M2M Cardinality
    } deriving (Show, Eq, Generic)

instance FromJSON RawRelation where
  parseJSON =
    Aeson.genericParseJSON aesonOptions

data ColMapping =
  ColMapping
    { fromCol :: Int32
    , toCol :: Int32
    } deriving (Show, Eq, Generic)

instance FromJSON ColMapping where
  parseJSON =
    Aeson.genericParseJSON aesonOptions

-- | Junction table on an M2M relationship
data RawJunction =
  Junction
    { junTableOid :: Oid
    , junConstraint1 :: Maybe ConstraintName
    , junConstraint2 :: Maybe ConstraintName
    , junColMap :: [ColMapping]
    } deriving (Show, Eq, Generic)

instance FromJSON RawJunction where
  parseJSON =
    Aeson.genericParseJSON aesonOptions
