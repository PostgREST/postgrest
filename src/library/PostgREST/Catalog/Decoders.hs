{-# LANGUAGE NamedFieldPuns #-}

module PostgREST.Catalog.Decoders
  ( arrayColumn
  , column
  , compositeArrayColumn
  , compositeField
  , compositeFieldArray
  , decodeFuncs
  , decodeRels
  , decodeTables
  , decodeViewKeyDeps
  , nullableColumn
  , nullableCompositeField
  , parseCols
  , relationshipsMap
  , relationshipsMapByTableSchema
  , viewKeyDepFromRow
  )
where

import Protolude

import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd qualified as HMI

import PostgREST.Catalog.Identifiers (QualifiedIdentifier (..))
import PostgREST.Catalog.Relationship (Cardinality (..), KeyDep (..), Relationship (..), RelationshipsMap, ViewKeyDependency (..))
import PostgREST.Catalog.Routine (FuncVolatility (..), PgType (..), RetType (..), Routine (..), RoutineMap, RoutineParam (..))
import PostgREST.Catalog.Table (Column (..), ColumnMap, Table (..), TablesMap)
import PostgREST.Config.Database (toIsolationLevel)

import Hasql.Decoders qualified as HD

decodeTables :: HD.Result TablesMap
decodeTables =
  HM.fromList . map (\tbl@Table{tableSchema, tableName} -> (QualifiedIdentifier tableSchema tableName, tbl)) <$> HD.rowList tblRow
  where
    tblRow =
      Table
        <$> column HD.text
        <*> column HD.text
        <*> nullableColumn HD.text
        <*> column HD.bool
        <*> column HD.bool
        <*> column HD.bool
        <*> column HD.bool
        <*> arrayColumn HD.text
        <*> parseCols
          ( compositeArrayColumn
              ( Column
                  <$> compositeField HD.text
                  <*> nullableCompositeField HD.text
                  <*> compositeField HD.bool
                  <*> compositeField HD.text
                  <*> compositeField HD.text
                  <*> nullableCompositeField HD.int4
                  <*> nullableCompositeField HD.text
                  <*> compositeFieldArray HD.text
              )
          )

parseCols :: HD.Row [Column] -> HD.Row ColumnMap
parseCols = fmap (HMI.fromList . map (\col@Column{colName} -> (colName, col)))

decodeRels :: HD.Result [Relationship]
decodeRels =
  HD.rowList relRow
  where
    relRow = (\(qi1, qi2, isSelf, constr, cols, isOneToOne) -> Relationship qi1 qi2 isSelf (if isOneToOne then O2O constr cols False else M2O constr cols) False False) <$> row
    row =
      (,,,,,)
        <$> (QualifiedIdentifier <$> column HD.text <*> column HD.text)
        <*> (QualifiedIdentifier <$> column HD.text <*> column HD.text)
        <*> column HD.bool
        <*> column HD.text
        <*> compositeArrayColumn ((,) <$> compositeField HD.text <*> compositeField HD.text)
        <*> column HD.bool

relationshipsMap :: [Relationship] -> RelationshipsMap
relationshipsMap =
  HM.fromListWith (<>) . fmap (\rel -> ((relTable rel, qiSchema $ relForeignTable rel), [rel]))

relationshipsMapByTableSchema :: [Relationship] -> RelationshipsMap
relationshipsMapByTableSchema =
  HM.fromListWith (<>) . fmap (\rel -> ((relTable rel, qiSchema $ relTable rel), [rel]))

decodeViewKeyDeps :: HD.Result [ViewKeyDependency]
decodeViewKeyDeps =
  map viewKeyDepFromRow <$> HD.rowList row
  where
    row =
      (,,,,,,)
        <$> column HD.text
        <*> column HD.text
        <*> column HD.text
        <*> column HD.text
        <*> column HD.text
        <*> column HD.text
        <*> compositeArrayColumn
          ( (,)
              <$> compositeField HD.text
              <*> compositeFieldArray HD.text
          )

viewKeyDepFromRow :: (Text, Text, Text, Text, Text, Text, [(Text, [Text])]) -> ViewKeyDependency
viewKeyDepFromRow (s1, t1, s2, v2, cons, consType, sCols) = ViewKeyDependency (QualifiedIdentifier s1 t1) (QualifiedIdentifier s2 v2) cons keyDep sCols
  where
    keyDep
      | consType == "p" = PKDep
      | consType == "f" = FKDep
      | otherwise = FKDepRef -- f_ref, we build this type in the query

decodeFuncs :: HD.Result RoutineMap
decodeFuncs =
  -- Duplicate rows for a function means they're overloaded, order these by least args according to Routine Ord instance
  map sort . HM.fromListWith (++) . map ((\(x, y) -> (x, [y])) . addKey) <$> HD.rowList funcRow
  where
    funcRow =
      Function
        <$> column HD.text
        <*> column HD.text
        <*> nullableColumn HD.text
        <*> compositeArrayColumn
          ( RoutineParam
              <$> compositeField HD.text
              <*> compositeField HD.text
              <*> compositeField HD.text
              <*> compositeField HD.bool
              <*> compositeField HD.bool
          )
        <*> ( parseRetType
                <$> column HD.text
                <*> column HD.text
                <*> column HD.bool
                <*> column HD.bool
                <*> column HD.bool
            )
        <*> (parseVolatility <$> column HD.char)
        <*> column HD.bool
        <*> nullableColumn (toIsolationLevel <$> HD.text)
        <*> compositeArrayColumn ((,) <$> compositeField HD.text <*> compositeField HD.text) -- function setting
    addKey :: Routine -> (QualifiedIdentifier, Routine)
    addKey pd = (QualifiedIdentifier (pdSchema pd) (pdName pd), pd)

    parseRetType :: Text -> Text -> Bool -> Bool -> Bool -> RetType
    parseRetType schema name isSetOf isComposite isCompositeAlias
      | isSetOf = SetOf pgType
      | otherwise = Single pgType
      where
        qi = QualifiedIdentifier schema name
        pgType
          | isComposite = Composite qi isCompositeAlias
          | otherwise = Scalar qi

    parseVolatility :: Char -> FuncVolatility
    parseVolatility v
      | v == 'i' = Immutable
      | v == 's' = Stable
      | otherwise = Volatile -- only 'v' can happen here

compositeArrayColumn :: HD.Composite a -> HD.Row [a]
compositeArrayColumn = arrayColumn . HD.composite

compositeField :: HD.Value a -> HD.Composite a
compositeField = HD.field . HD.nonNullable

nullableCompositeField :: HD.Value a -> HD.Composite (Maybe a)
nullableCompositeField = HD.field . HD.nullable

compositeFieldArray :: HD.Value a -> HD.Composite [a]
compositeFieldArray = HD.field . HD.nonNullable . HD.listArray . HD.nonNullable

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

nullableColumn :: HD.Value a -> HD.Row (Maybe a)
nullableColumn = HD.column . HD.nullable

arrayColumn :: HD.Value a -> HD.Row [a]
arrayColumn = column . HD.listArray . HD.nonNullable
